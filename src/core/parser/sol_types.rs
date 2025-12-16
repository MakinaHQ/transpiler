use saphyr::{MarkedYaml, Yaml, YamlData};

use alloy::{
    dyn_abi::{DynSolType, DynSolValue},
    hex,
    primitives::{Address, FixedBytes, I256, U256},
};

use thiserror::Error;

use std::str::FromStr;

use crate::meta_sol_types;

use super::{errors::ParserError, helpers};

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
}

pub type SolTypeResult<T> = Result<T, SolTypeError>;

/// Parse a uint string, supporting scientific notation (e.g., "1e18", "1E18").
pub fn parse_uint_str(s: &str) -> SolTypeResult<U256> {
    // Check for scientific notation (e.g., "1e18", "1E18", "5e6")
    if let Some(e_pos) = s.to_lowercase().find('e') {
        let (mantissa_str, exp_str) = s.split_at(e_pos);
        let exp_str = &exp_str[1..]; // Skip the 'e' or 'E'

        // Parse mantissa - could be integer or decimal
        let (mantissa, decimal_places) = if let Some(dot_pos) = mantissa_str.find('.') {
            let integer_part = &mantissa_str[..dot_pos];
            let fractional_part = &mantissa_str[dot_pos + 1..];
            let combined = format!("{}{}", integer_part, fractional_part);
            let mantissa = U256::from_str(&combined)
                .map_err(|err| SolTypeError::InvalidUintValue(Box::new(err)))?;
            (mantissa, fractional_part.len())
        } else {
            let mantissa = U256::from_str(mantissa_str)
                .map_err(|err| SolTypeError::InvalidUintValue(Box::new(err)))?;
            (mantissa, 0)
        };

        // Parse exponent
        let exp: i32 = exp_str.parse().map_err(|_| {
            SolTypeError::InvalidSolValue(format!("Invalid exponent in scientific notation: {}", s))
        })?;

        // Adjust exponent for decimal places in mantissa
        let effective_exp = exp - decimal_places as i32;

        if effective_exp < 0 {
            return Err(SolTypeError::InvalidSolValue(format!(
                "Scientific notation would result in non-integer value: {}",
                s
            )));
        }

        // Calculate 10^effective_exp
        let multiplier = U256::from(10u64).pow(U256::from(effective_exp as u64));
        let value = mantissa.checked_mul(multiplier).ok_or_else(|| {
            SolTypeError::InvalidSolValue(format!("Overflow in scientific notation: {}", s))
        })?;

        Ok(value)
    } else {
        // Standard integer parsing
        U256::from_str(s).map_err(|err| SolTypeError::InvalidUintValue(Box::new(err)))
    }
}

/// Parse a sol value directly from a raw string, without going through YAML.
/// This is useful for position variables where the type is inferred from usage.
pub fn parse_sol_value_from_str(sol_type: &DynSolType, raw_str: &str) -> SolTypeResult<DynSolValue> {
    match sol_type {
        DynSolType::Bool => {
            let value: bool = raw_str.parse().map_err(|_| {
                SolTypeError::InvalidSolValue(format!(
                    "Expected valid boolean, got string: {}",
                    raw_str
                ))
            })?;
            Ok(DynSolValue::Bool(value))
        }
        DynSolType::Int(bits) => {
            let value = if raw_str.starts_with("0x") {
                I256::from_hex_str(raw_str)
                    .map_err(|err| SolTypeError::InvalidIntValue(Box::new(err)))?
            } else {
                I256::from_dec_str(raw_str)
                    .map_err(|err| SolTypeError::InvalidIntValue(Box::new(err)))?
            };
            Ok(DynSolValue::Int(value, *bits))
        }
        DynSolType::Uint(bits) => {
            let value = parse_uint_str(raw_str)?;
            Ok(DynSolValue::Uint(value, *bits))
        }
        DynSolType::Address => {
            let value = Address::from_str(raw_str)
                .map_err(|err| SolTypeError::InvalidHexValue(Box::new(err)))?;
            Ok(DynSolValue::Address(value))
        }
        DynSolType::FixedBytes(bytes) => {
            let mut value = hex::decode(raw_str)
                .map_err(|err| SolTypeError::InvalidHexValue(Box::new(err)))?;

            if value.len() != *bytes {
                return Err(SolTypeError::InvalidSolValue(format!(
                    "Expected {} bytes, got {}",
                    *bytes,
                    value.len()
                )));
            }

            if value.len() < 32 {
                value.resize(32, 0);
            }

            let value = FixedBytes::from_slice(&value);
            Ok(DynSolValue::FixedBytes(value, *bytes))
        }
        DynSolType::String => Ok(DynSolValue::String(raw_str.to_string())),
        DynSolType::Bytes => {
            let value = hex::decode(raw_str)
                .map_err(|err| SolTypeError::InvalidHexValue(Box::new(err)))?;
            Ok(DynSolValue::Bytes(value))
        }
        _ => Err(SolTypeError::InvalidSolValue(format!(
            "Unsupported type for raw string parsing: {}",
            sol_type
        ))),
    }
}

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
            let yaml_value = yaml.as_str().ok_or(ParserError::InvalidYaml)?;

            let value = if yaml_value.starts_with("0x") {
                I256::from_hex_str(yaml_value)
                    .map_err(|err| SolTypeError::InvalidIntValue(Box::new(err)))?
            } else {
                I256::from_dec_str(yaml_value)
                    .map_err(|err| SolTypeError::InvalidIntValue(Box::new(err)))?
            };

            Ok(DynSolValue::Int(value, *bits))
        }
        DynSolType::Uint(bits) => {
            let yaml_value = yaml.as_str().ok_or(ParserError::InvalidYaml)?;
            let value = parse_uint_str(yaml_value)?;
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
) -> Option<DynSolValue> {
    parse_sol_value(sol_type, &unmark(field.clone()), "").ok()
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

    #[test]
    fn test_parse_uint_str_plain() {
        // Plain integers should work
        assert_eq!(parse_uint_str("100000").unwrap(), U256::from(100000u64));
        assert_eq!(parse_uint_str("0").unwrap(), U256::ZERO);
        assert_eq!(parse_uint_str("1").unwrap(), U256::from(1u64));
    }

    #[test]
    fn test_parse_uint_str_scientific_notation() {
        // 1e18 = 1 * 10^18 = 1000000000000000000
        assert_eq!(
            parse_uint_str("1e18").unwrap(),
            U256::from(1_000_000_000_000_000_000u64)
        );

        // 1E18 (uppercase) should work too
        assert_eq!(
            parse_uint_str("1E18").unwrap(),
            U256::from(1_000_000_000_000_000_000u64)
        );

        // 5e6 = 5000000
        assert_eq!(parse_uint_str("5e6").unwrap(), U256::from(5_000_000u64));

        // 10e2 = 1000
        assert_eq!(parse_uint_str("10e2").unwrap(), U256::from(1000u64));

        // 1e0 = 1
        assert_eq!(parse_uint_str("1e0").unwrap(), U256::from(1u64));
    }

    #[test]
    fn test_parse_uint_str_decimal_with_exponent() {
        // 1.5e18 = 1500000000000000000
        assert_eq!(
            parse_uint_str("1.5e18").unwrap(),
            U256::from(1_500_000_000_000_000_000u64)
        );

        // 2.5e6 = 2500000
        assert_eq!(parse_uint_str("2.5e6").unwrap(), U256::from(2_500_000u64));

        // 1.23e3 = 1230
        assert_eq!(parse_uint_str("1.23e3").unwrap(), U256::from(1230u64));
    }

    #[test]
    fn test_parse_uint_str_invalid() {
        // Negative exponent that results in non-integer should fail
        assert!(parse_uint_str("1e-1").is_err());

        // Invalid exponent
        assert!(parse_uint_str("1eabc").is_err());
    }
}
