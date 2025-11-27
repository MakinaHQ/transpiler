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
            let value = U256::from_str(yaml_value)
                .map_err(|err| SolTypeError::InvalidUintValue(Box::new(err)))?;
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
