//! Bound program IR: the input to weiroll codegen.
//!
//! Every name is resolved: operands are literals, slot indices or call indices.

use std::collections::BTreeMap;

use alloy::{
    dyn_abi::{DynSolType, DynSolValue},
    primitives::{Address, Selector, U256},
};

use crate::{
    token_list::TokenInfo,
    types::{InputType, InstructionType, MetaTypeData},
};

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub tokens: BTreeMap<String, TokenInfo>,
    /// One entry per source position, in declaration order (IDs need not be unique).
    pub positions: Vec<Position>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Position {
    pub id: U256,
    pub group_id: U256,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instruction {
    /// Rootfile keys.
    pub protocol: String,
    pub name: String,
    pub label: String,
    pub position_id: U256,
    pub group_id: U256,
    pub is_debt: bool,
    pub instruction_type: InstructionType,
    pub affected_tokens: Vec<Address>,
    pub position_tokens: Vec<Address>,
    pub description: Option<String>,
    pub tags: Vec<String>,
    pub action: Action,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Action {
    pub calls: Vec<Call>,
    /// Reserved slots, in rootfile order.
    pub outputs: Vec<Output>,
    /// Input slots. The rootfile index of `slots[i]` is `outputs.len() + i`.
    pub slots: Vec<Slot>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub target: Address,
    pub selector: Selector,
    pub args: Vec<Operand>,
    pub returns: Option<DynSolType>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Literal(Literal),
    /// Index into [Action::slots].
    Slot(usize),
    /// Index into [Action::calls] of an earlier call.
    Return(usize),
    Tuple(Vec<Operand>),
    Array(Vec<Operand>),
    FixedArray(Vec<Operand>),
}

/// A constant operand. The variant records where the value came from, because the two
/// sources are encoded differently (see `codegen::literal`).
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    /// A value supplied by the instruction's inputs.
    Input(DynSolValue),
    /// A value written in the blueprint.
    Scalar(DynSolValue),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Output {
    /// Index into [Action::calls].
    Return(usize),
    Literal(DynSolValue),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Slot {
    /// The placeholder in the state is `keccak256(name)`.
    pub name: String,
    pub ty: DynSolType,
    pub description: Option<String>,
    pub input_type: Option<InputType>,
    pub meta_type: Option<MetaTypeData>,
}
