use indexmap::IndexMap;
use std::{collections::HashMap, path::PathBuf};

use alloy::{
    dyn_abi::{DynSolType, DynSolValue},
    primitives::{Address, Selector},
};

use crate::{core::parser::sol_types::YamlSolValue, meta_sol_types::MetaDynSolType};

#[derive(Debug, Clone, PartialEq)]
pub struct BlueprintValue {
    pub value: DynSolValue,
    pub r#type: DynSolType,
    pub description: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlueprintReservedSlot {
    Return(String),         // Reference to a previous call's return value.
    Scalar(BlueprintValue), // A scalar value.
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlueprintParameter {
    Return(String),                        // Reference to a previous call's return value.
    Input(String),                         // Reference to an input.
    InputSlot(String),                     // Reference to an input slot.
    Scalar(BlueprintValue),                // A scalar value.
    Tuple(Vec<BlueprintParameter>),        // A tuple of parameters.
    DynamicArray(Vec<BlueprintParameter>), // A dynamic array.
    FixedArray(Vec<BlueprintParameter>, usize), // A fixed-size array.
}

impl From<DynSolValue> for BlueprintParameter {
    fn from(sol: DynSolValue) -> Self {
        if let Some(inner) = sol.as_tuple() {
            let parsed: Vec<BlueprintParameter> = inner.iter().cloned().map(Into::into).collect();
            return BlueprintParameter::Tuple(parsed);
        }

        if let Some(inner) = sol.as_fixed_array() {
            let parsed: Vec<BlueprintParameter> = inner.iter().cloned().map(Into::into).collect();
            let size = inner.len();
            return BlueprintParameter::FixedArray(parsed, size);
        }

        if let Some(inner) = sol.as_array() {
            let parsed: Vec<BlueprintParameter> = inner.iter().cloned().map(Into::into).collect();
            return BlueprintParameter::DynamicArray(parsed);
        }

        let r#type = sol.as_type().expect("type must be known here");
        BlueprintParameter::Scalar(BlueprintValue {
            value: sol,
            r#type,
            description: None,
        })
    }
}

impl BlueprintParameter {
    /// Recursively collect all input slot names used in the [BlueprintParameter].
    pub fn input_slots(&self) -> Vec<String> {
        match self {
            BlueprintParameter::InputSlot(name) => vec![name.clone()],
            BlueprintParameter::Tuple(params)
            | BlueprintParameter::DynamicArray(params)
            | BlueprintParameter::FixedArray(params, _) => {
                params.iter().flat_map(|p| p.input_slots()).collect()
            }
            _ => vec![],
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlueprintInput {
    pub name: String,
    pub r#type: DynSolType,
    pub description: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlueprintInputSlot {
    pub name: String,
    pub r#type: DynSolType,
    pub description: Option<String>,
    pub meta_type: Option<MetaDynSolType>,
    pub meta_type_field: Option<String>,
    pub meta_type_name: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlueprintReturn {
    pub name: String,
    pub r#type: DynSolType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Blueprint {
    pub path: PathBuf,
    pub protocol: String,
    pub inputs: HashMap<String, BlueprintInput>,
    pub constants: HashMap<String, YamlSolValue>,
    pub actions: HashMap<String, BlueprintAction>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlueprintAction {
    pub calls: Vec<BlueprintCall>,
    pub returns_mapping: HashMap<String, usize>,
    pub reserved_slots: Vec<BlueprintReservedSlot>,
    pub input_slots: IndexMap<String, BlueprintInputSlot>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BlueprintTarget {
    Input(String),
    Address(Address),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlueprintCall {
    pub description: String,
    pub target: BlueprintTarget,
    pub selector: Selector,
    pub parameters: Vec<BlueprintParameter>,
    pub r#return: Option<BlueprintReturn>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlueprintTemplate {
    Input(BlueprintInput),
    InputSlot(BlueprintInputSlot),
    Constant(YamlSolValue),
    Return(BlueprintReturn),
    Raw(YamlSolValue),
}

impl BlueprintTemplate {
    pub fn as_constant(&self) -> Option<&YamlSolValue> {
        match self {
            Self::Constant(cons) => Some(cons),
            _ => None,
        }
    }

    pub fn as_input(&self) -> Option<&BlueprintInput> {
        match self {
            Self::Input(input) => Some(input),
            _ => None,
        }
    }

    pub fn as_type(&self) -> &DynSolType {
        match self {
            Self::Input(input) => &input.r#type,
            Self::InputSlot(slot) => &slot.r#type,
            Self::Constant(cons) => &cons.r#type,
            Self::Return(ret) => &ret.r#type,
            Self::Raw(raw) => &raw.r#type,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_input_slots() {
        let param = BlueprintParameter::Tuple(vec![
            BlueprintParameter::InputSlot("slot1".to_string()),
            BlueprintParameter::DynamicArray(vec![
                BlueprintParameter::InputSlot("slot2".to_string()),
                BlueprintParameter::Scalar(BlueprintValue {
                    value: DynSolValue::Bool(true),
                    r#type: DynSolType::Bool,
                    description: None,
                }),
            ]),
            BlueprintParameter::FixedArray(
                vec![
                    BlueprintParameter::InputSlot("slot3".to_string()),
                    BlueprintParameter::Return("return1".to_string()),
                ],
                2,
            ),
        ]);
        let slots = param.input_slots();
        assert_eq!(slots, vec!["slot1", "slot2", "slot3"]);
    }
}
