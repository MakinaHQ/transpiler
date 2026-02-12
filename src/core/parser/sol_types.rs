use saphyr::{MarkedYaml, Yaml, YamlData};

use alloy::{
    dyn_abi::{DynSolType, DynSolValue},
    hex,
    primitives::{Address, FixedBytes, I256, U256},
};

use miette::NamedSource;
use thiserror::Error;

use std::str::FromStr;

use crate::meta_sol_types;

use super::{
    common::ParserError as MietteParserError,
    errors::ParserError,
    helpers,
    transpiler_utils::{Keccak256Error, TranspilerUtil, UtilParseError},
};

#[derive(Debug, Error)]
pub enum SolTypeError {
    #[error("Invalid Sol type: {0}")]
    InvalidSolType(#[from] Box<alloy::dyn_abi::Error>),
    #[error("Invalid Sol value: {0}")]
    InvalidSolValue(String),
    #[error("Invalid Int value")]
    InvalidIntValue(#[from] Box<alloy::primitives::ParseSignedError>),
    #[error("Invalid Uint value")]
    InvalidUintValue(#[from] Box<alloy::primitives::ruint::ParseError>),
    #[error("Invalid hex value")]
    InvalidHexValue(#[from] Box<alloy::hex::FromHexError>),
    #[error("Invalid Yaml")]
    InvalidYaml(#[from] ParserError),
    #[error("Mismatching types: expected {expected}, got {got}")]
    MismatchingTypes {
        expected: Box<DynSolType>,
        got: Box<DynSolType>,
    },
    #[error("{0}")]
    UtilParseError(#[from] UtilParseError),
    #[error("{0}")]
    Keccak256Error(#[from] Keccak256Error),
}

pub type SolTypeResult<T> = Result<T, SolTypeError>;

#[derive(Debug, Clone, PartialEq)]
pub struct YamlSolValue {
    pub r#type: DynSolType,
    pub value: DynSolValue,
}

pub fn parse_yaml_sol_value(yaml: &Yaml, field_path: &str) -> SolTypeResult<YamlSolValue> {
    // Check that the yaml is a mapping.
    let mapping = helpers::get_mapping(yaml, field_path)?;

    let r#type = helpers::get_string_field(mapping, "type", field_path)?;
    let value = helpers::get_field(mapping, "value", field_path)?;

    // Get the type and value from the mapping.
    let r#type = parse_sol_type_str(r#type)?;
    let value = parse_sol_value(&r#type, value, field_path)?;

    Ok(YamlSolValue { r#type, value })
}

pub fn parse_sol_type_str(type_str: &str) -> SolTypeResult<DynSolType> {
    // We have to handle the case where the sol_type is a custom type, which we wrap around DynSolType::CustomStruct.
    if let Ok(meta_type) = meta_sol_types::MetaDynSolType::from_str(type_str) {
        return Ok(meta_type.to_dyn_sol_type());
    }
    DynSolType::parse(type_str).map_err(|err| SolTypeError::InvalidSolType(Box::new(err)))
}

pub fn parse_sol_value(
    sol_type: &DynSolType,
    yaml: &Yaml,
    field_path: &str,
) -> SolTypeResult<DynSolValue> {
    // Check for transpiler util first (e.g., ${keccak256(...)})
    // keccak256 returns bytes32
    if let Some(yaml_str) = yaml.as_str()
        && let Some(util_result) = TranspilerUtil::try_parse(yaml_str)
    {
        let util = util_result?;
        return Ok(util.evaluate_to_dyn_sol_value(sol_type)?);
    }

    // Regular type parsing
    match sol_type {
        DynSolType::Bool => {
            if yaml.is_string() {
                let value: bool = yaml.as_str().unwrap().parse().map_err(|_| {
                    SolTypeError::InvalidSolValue(format!(
                        "Expected valid boolean, got string: {}",
                        yaml.as_str().unwrap()
                    ))
                })?;

                Ok(DynSolValue::Bool(value))
            } else {
                Err(SolTypeError::InvalidSolValue(format!(
                    "Expected valid boolean, got {yaml:?}"
                )))
            }
        }
        DynSolType::Int(bits) => {
            // Handle both string and integer YAML values
            let value = if let Some(yaml_str) = yaml.as_str() {
                if yaml_str.starts_with("0x") {
                    I256::from_hex_str(yaml_str)
                        .map_err(|err| SolTypeError::InvalidIntValue(Box::new(err)))?
                } else {
                    I256::from_dec_str(yaml_str)
                        .map_err(|err| SolTypeError::InvalidIntValue(Box::new(err)))?
                }
            } else if let Some(yaml_int) = yaml.as_integer() {
                I256::try_from(yaml_int).map_err(|_| {
                    SolTypeError::InvalidSolValue(format!(
                        "Integer value {yaml_int} out of range for I256"
                    ))
                })?
            } else {
                return Err(ParserError::InvalidYaml.into());
            };

            Ok(DynSolValue::Int(value, *bits))
        }
        DynSolType::Uint(bits) => {
            // Handle both string and integer YAML values
            let value = if let Some(yaml_str) = yaml.as_str() {
                U256::from_str(yaml_str)
                    .map_err(|err| SolTypeError::InvalidUintValue(Box::new(err)))?
            } else if let Some(yaml_int) = yaml.as_integer() {
                if yaml_int < 0 {
                    return Err(SolTypeError::InvalidSolValue(format!(
                        "Expected unsigned integer, got negative value: {yaml_int}"
                    )));
                }
                U256::from(yaml_int as u64)
            } else {
                return Err(ParserError::InvalidYaml.into());
            };
            Ok(DynSolValue::Uint(value, *bits))
        }
        DynSolType::Address => {
            let yaml_value = yaml.as_str().ok_or(ParserError::InvalidYaml)?;
            let value = Address::from_str(yaml_value)
                .map_err(|err| SolTypeError::InvalidHexValue(Box::new(err)))?;
            Ok(DynSolValue::Address(value))
        }
        DynSolType::FixedBytes(bytes) => {
            let yaml_value = yaml.as_str().ok_or(ParserError::InvalidYaml)?;

            let mut value = hex::decode(yaml_value)
                .map_err(|err| SolTypeError::InvalidHexValue(Box::new(err)))?;

            // Check value length.
            if value.len() != *bytes {
                return Err(SolTypeError::InvalidSolValue(format!(
                    "Expected {} bytes, got {}",
                    *bytes,
                    value.len()
                )));
            }

            // If the value is not 32 bytes, pad with zeros.
            if value.len() < 32 {
                value.resize(32, 0);
            }

            let value = FixedBytes::from_slice(&value);

            Ok(DynSolValue::FixedBytes(value, *bytes))
        }
        DynSolType::String => {
            let yaml_value = yaml.as_str().ok_or(ParserError::InvalidYaml)?;
            Ok(DynSolValue::String(yaml_value.to_string()))
        }
        DynSolType::Bytes => {
            let yaml_value = yaml.as_str().ok_or(ParserError::InvalidYaml)?;
            let value = hex::decode(yaml_value)
                .map_err(|err| SolTypeError::InvalidHexValue(Box::new(err)))?;
            Ok(DynSolValue::Bytes(value))
        }
        DynSolType::Tuple(types) => {
            let yaml_sequence = yaml.as_sequence().ok_or(ParserError::InvalidYaml)?;

            let values = yaml_sequence
                .iter()
                .zip(types.iter())
                .map(|(value, expected_type)| {
                    let value = parse_yaml_sol_value(value, field_path)?;

                    if value.r#type != *expected_type {
                        return Err(SolTypeError::InvalidSolValue(format!(
                            "Expected type {}, got {}",
                            expected_type, value.r#type
                        )));
                    }

                    Ok(value.value)
                })
                .collect::<Result<Vec<_>, _>>()?;

            Ok(DynSolValue::Tuple(values))
        }
        DynSolType::Array(array_type) => {
            let yaml_sequence = yaml.as_sequence().ok_or(ParserError::InvalidYaml)?;

            let values = yaml_sequence
                .iter()
                .map(|value| parse_sol_value(array_type, value, field_path))
                .collect::<Result<Vec<_>, _>>()?;

            Ok(DynSolValue::Array(values))
        }
        DynSolType::FixedArray(fixed_array_type, size) => {
            let yaml_sequence = yaml.as_sequence().ok_or(ParserError::InvalidYaml)?;

            let values = yaml_sequence
                .iter()
                .map(|value| parse_sol_value(fixed_array_type, value, field_path))
                .collect::<Result<Vec<_>, _>>()?;

            if values.len() != *size {
                return Err(SolTypeError::InvalidSolValue(format!(
                    "Expected {} values, got {}",
                    size,
                    values.len()
                )));
            }

            Ok(DynSolValue::FixedArray(values))
        }
        _ => Err(SolTypeError::InvalidSolValue(format!(
            "Unsupported type: {sol_type}"
        ))),
    }
}

pub fn parse_sol_value_marked<'a>(
    sol_type: &DynSolType,
    field: &'a MarkedYaml<'a>,
    named_source: NamedSource<String>,
) -> miette::Result<Option<DynSolValue>> {
    match parse_sol_value(sol_type, &unmark(field.clone()), "") {
        Ok(value) => Ok(Some(value)),
        // Propagate transpiler util errors - these are actionable and should be shown to users
        Err(SolTypeError::UtilParseError(e)) => {
            Err(MietteParserError::new(named_source, field.span, e.to_string()).into())
        }
        Err(SolTypeError::Keccak256Error(e)) => {
            Err(MietteParserError::new(named_source, field.span, e.to_string()).into())
        }
        Err(_) => Ok(None),
    }
}

fn unmark<'a>(input: MarkedYaml<'a>) -> Yaml<'a> {
    match input.data {
        YamlData::Value(value) => Yaml::Value(value),
        YamlData::Representation(str, style, tag) => Yaml::Representation(str, style, tag),
        YamlData::Mapping(mapping) => Yaml::Mapping(
            mapping
                .into_iter()
                .map(|(k, v)| (unmark(k), unmark(v)))
                .collect(),
        ),
        YamlData::Sequence(sequence) => {
            Yaml::Sequence(sequence.into_iter().map(|e| unmark(e)).collect())
        }
        YamlData::BadValue => Yaml::BadValue,
        YamlData::Alias(alias) => Yaml::Alias(alias),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloy::primitives::keccak256;

    fn yaml_string(s: &str) -> Yaml<'static> {
        Yaml::Value(saphyr::Scalar::String(s.to_string().into()))
    }

    // ========== Transpiler Util Integration Tests ==========
    // Note: Detailed parsing/validation tests are in transpiler_utils.rs
    // These tests verify the full pipeline through parse_sol_value

    #[test]
    fn test_transpiler_util_keccak256_bytes32() {
        // Note: keccak256() hashes the literal string, not a config lookup
        let yaml = yaml_string("${keccak256(my_storage_slot)}");
        let sol_type = DynSolType::FixedBytes(32);

        let result = parse_sol_value(&sol_type, &yaml, "test").unwrap();

        let DynSolValue::FixedBytes(bytes, size) = result else {
            panic!("Expected FixedBytes");
        };
        assert_eq!(size, 32);
        assert_eq!(bytes, keccak256("my_storage_slot".as_bytes()));
    }

    #[test]
    fn test_transpiler_util_type_mismatch_bytes16() {
        // keccak256 returns bytes32, should fail for bytes16
        let yaml = yaml_string("${keccak256(test)}");
        let sol_type = DynSolType::FixedBytes(16);

        let result = parse_sol_value(&sol_type, &yaml, "test");
        assert!(matches!(result, Err(SolTypeError::Keccak256Error(_))));
    }

    #[test]
    fn test_transpiler_util_type_mismatch_address() {
        // keccak256 returns bytes32, should fail for address
        let yaml = yaml_string("${keccak256(test)}");
        let sol_type = DynSolType::Address;

        let result = parse_sol_value(&sol_type, &yaml, "test");
        match result {
            Err(SolTypeError::Keccak256Error(Keccak256Error::TypeMismatch(expected))) => {
                assert_eq!(expected, "address");
            }
            _ => panic!("Expected Keccak256Error::TypeMismatch"),
        }
    }

    #[test]
    fn test_transpiler_util_type_mismatch_uint256() {
        // keccak256 returns bytes32, should fail for uint256
        let yaml = yaml_string("${keccak256(test)}");
        let sol_type = DynSolType::Uint(256);

        let result = parse_sol_value(&sol_type, &yaml, "test");
        assert!(matches!(result, Err(SolTypeError::Keccak256Error(_))));
    }

    // ========== Regular Value Tests ==========

    #[test]
    fn test_hex_value_still_works() {
        let yaml =
            yaml_string("0x9c22ff5f21f0b81b113e63f7db6da94fedef11b2119b4088b89664fb9a3cb658");
        let sol_type = DynSolType::FixedBytes(32);

        let result = parse_sol_value(&sol_type, &yaml, "test").unwrap();

        let DynSolValue::FixedBytes(bytes, size) = result else {
            panic!("Expected FixedBytes");
        };
        assert_eq!(size, 32);
        assert_eq!(
            format!("{bytes}"),
            "0x9c22ff5f21f0b81b113e63f7db6da94fedef11b2119b4088b89664fb9a3cb658"
        );
    }

    #[test]
    fn test_template_variable_not_parsed_as_util() {
        // ${config.value} should NOT match util syntax (no parentheses)
        let yaml = yaml_string("${config.my_slot}");
        let sol_type = DynSolType::FixedBytes(32);

        let result = parse_sol_value(&sol_type, &yaml, "test");
        // Should fail as invalid hex, not as TranspilerUtilError
        assert!(matches!(result, Err(SolTypeError::InvalidHexValue(_))));
    }

    // ========== "Any String" Input Integration Tests ==========

    #[test]
    fn test_transpiler_util_keccak256_with_special_chars() {
        // Test that non-identifier strings (spaces, slashes, etc.) work through the full pipeline
        let yaml = yaml_string("${keccak256(test/path value)}");
        let sol_type = DynSolType::FixedBytes(32);

        let result = parse_sol_value(&sol_type, &yaml, "test").unwrap();

        let DynSolValue::FixedBytes(bytes, size) = result else {
            panic!("Expected FixedBytes");
        };
        assert_eq!(size, 32);
        assert_eq!(bytes, keccak256("test/path value".as_bytes()));
    }

    #[test]
    fn test_transpiler_util_keccak256_quoted_special_chars() {
        // Test quoted non-identifier strings work through the full pipeline
        let yaml = yaml_string("${keccak256(\"test/path value\")}");
        let sol_type = DynSolType::FixedBytes(32);

        let result = parse_sol_value(&sol_type, &yaml, "test").unwrap();

        let DynSolValue::FixedBytes(bytes, size) = result else {
            panic!("Expected FixedBytes");
        };
        assert_eq!(size, 32);
        assert_eq!(bytes, keccak256("test/path value".as_bytes()));
    }
}
