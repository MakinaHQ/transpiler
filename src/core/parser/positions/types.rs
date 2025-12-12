use std::{
    collections::{BTreeMap, HashMap},
    path::PathBuf,
};

use alloy::{
    dyn_abi::{DynSolType, DynSolValue},
    primitives::{Address, U256},
};

use crate::{core::parser::sol_types::YamlSolValue, token_list::TokenInfo, types::InstructionType};

#[derive(Debug)]
pub struct Root {
    pub tokens: BTreeMap<String, TokenInfo>,
    pub config: BTreeMap<String, InstructionTemplate>,
    pub positions: Vec<Position>,
}

#[derive(Debug)]
pub struct SolValue {
    pub r#type: DynSolType,
    pub value: DynSolValue,
    pub description: Option<String>,
}

#[derive(Debug)]
pub struct Position {
    pub id: U256,
    pub group_id: U256,
    pub description: Option<String>,
    pub instructions: Vec<Instruction>,
    pub global_tags: Vec<(String, String)>,
}

impl Position {
    /// Returns all tags that match a given instruction name.
    pub fn get_tags(&self, name: &str) -> Vec<String> {
        self.global_tags
            .iter()
            .filter(|(action, _)| action == name)
            .map(|(_, tag)| tag)
            .cloned()
            .collect()
    }
}

#[derive(Debug)]
pub struct Instruction {
    pub description: Option<String>,
    pub is_debt: bool,
    pub instruction_type: InstructionType,
    pub affected_tokens: Vec<Address>,
    pub definition: InstructionDefinition,
}

#[derive(Debug)]
pub struct InstructionDefinition {
    /// The path of instructions blueprint.
    pub blueprint_path: PathBuf,
    pub name: String,
    pub label: String,
    pub override_name: Option<String>,
    pub inputs: HashMap<String, SolValue>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InstructionTemplateEnum {
    Config(YamlSolValue),
    Raw(YamlSolValue),
    TokenInfo(TokenInfo),
    /// Position-level variables defined in the `vars` field of a position.
    /// Type is inferred from the instruction where the variable is used.
    PositionVar(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct InstructionTemplate {
    pub template: InstructionTemplateEnum,
    pub is_used: bool,
}

impl InstructionTemplate {
    pub fn new(template: InstructionTemplateEnum) -> Self {
        Self {
            template,
            is_used: false,
        }
    }

    pub fn as_token(&self) -> Option<&TokenInfo> {
        match &self.template {
            InstructionTemplateEnum::TokenInfo(token) => Some(token),
            _ => None,
        }
    }

    /// Returns the type of the template value.
    /// Panics for `PositionVar` since its type is inferred from usage context.
    pub fn as_type(&self) -> &DynSolType {
        match &self.template {
            InstructionTemplateEnum::Raw(raw) => &raw.r#type,
            InstructionTemplateEnum::Config(config) => &config.r#type,
            InstructionTemplateEnum::TokenInfo(_) => &DynSolType::Address,
            InstructionTemplateEnum::PositionVar(_) => {
                panic!("PositionVar type must be inferred from usage context")
            }
        }
    }

    /// Returns true if this template has a known type (not a position var).
    pub fn has_known_type(&self) -> bool {
        !matches!(self.template, InstructionTemplateEnum::PositionVar(_))
    }
}
