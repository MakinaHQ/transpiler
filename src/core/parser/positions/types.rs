use std::{collections::HashMap, path::PathBuf};

use alloy::{
    dyn_abi::{DynSolType, DynSolValue},
    primitives::{Address, U256},
};

use crate::types::InstructionType;

#[derive(Debug)]
pub struct Root {
    pub config: HashMap<String, SolValue>,
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
