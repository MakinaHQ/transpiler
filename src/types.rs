use std::{collections::BTreeMap, str::FromStr};

use alloy::{
    dyn_abi::DynSolType,
    primitives::{Address, Bytes, FixedBytes, U128, U256, keccak256},
    sol_types::SolValue,
};
use eyre::eyre;
use serde::{Deserialize, Serialize};
use serde_repr::{Deserialize_repr, Serialize_repr};

use crate::merkletree::MerkleTree;

#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum InputSlotType {
    Uint,
    Int,
    Address,
    Bytes,
    String,
    Bool,
    Swap,
    Array(Box<InputSlotType>),
    FixedBytes32,
}

impl From<InputSlotType> for DynSolType {
    fn from(value: InputSlotType) -> Self {
        match value {
            InputSlotType::Uint => DynSolType::Uint(256usize),
            InputSlotType::Int => DynSolType::Int(256usize),
            InputSlotType::Address => DynSolType::Address,
            InputSlotType::Bytes => DynSolType::Bytes,
            InputSlotType::String => DynSolType::String,
            InputSlotType::Bool => DynSolType::Bool,
            InputSlotType::Swap => DynSolType::Bytes,
            InputSlotType::Array(inner) => DynSolType::Array(Box::new(DynSolType::from(*inner))),
            InputSlotType::FixedBytes32 => DynSolType::FixedBytes(32),
        }
    }
}

impl From<DynSolType> for InputSlotType {
    fn from(ty: DynSolType) -> Self {
        match ty {
            DynSolType::Uint(_) => InputSlotType::Uint,
            DynSolType::Int(_) => InputSlotType::Int,
            DynSolType::Address => InputSlotType::Address,
            DynSolType::Bytes => InputSlotType::Bytes,
            DynSolType::String => InputSlotType::String,
            DynSolType::Bool => InputSlotType::Bool,
            DynSolType::Array(inner) => InputSlotType::Array(Box::new(InputSlotType::from(*inner))),
            DynSolType::FixedBytes(32) => InputSlotType::FixedBytes32,
            _ => panic!("unsupported type: {ty:?}"),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RootfileInputSlot {
    pub index: usize,
    pub name: String,
    pub r#type: InputSlotType,
    pub meta_type: Option<MetaTypeData>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MetaTypeData {
    pub r#type: String,
    pub name: String,
    pub field: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize_repr, Deserialize_repr)]
#[repr(u8)]
pub enum InstructionType {
    Management = 0,
    Accounting = 1,
    Harvest = 2,
    FlashloanManagement = 3,
}

impl FromStr for InstructionType {
    type Err = eyre::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "MANAGEMENT" => Ok(InstructionType::Management),
            "ACCOUNTING" => Ok(InstructionType::Accounting),
            "HARVEST" => Ok(InstructionType::Harvest),
            "FLASHLOAN_MANAGEMENT" => Ok(InstructionType::FlashloanManagement),
            _ => Err(eyre!("unexpected instruction type")),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MakinaInstruction {
    #[serde(with = "alloy::serde::displayfromstr")]
    pub position_id: U256,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    #[serde(default = "Vec::default")]
    pub tags: Vec<String>,
    pub is_debt: bool,
    #[serde(with = "alloy::serde::displayfromstr")]
    pub group_id: U256,
    pub instruction_type: InstructionType,
    pub affected_tokens: Vec<Address>,
    pub commands: Vec<FixedBytes<32>>,
    pub state: Vec<Bytes>,
    #[serde(with = "alloy::serde::displayfromstr")]
    pub bitmap: U128,
    pub inputs_slots: Vec<RootfileInputSlot>,
}

#[derive(Clone, Debug)]
pub struct NamedMakinaInstruction {
    pub inner: MakinaInstruction,
    pub instruction_name: String,
    pub token_name: String,
    pub protocol_name: String,
}

impl MakinaInstruction {
    pub fn commands_hash(&self) -> FixedBytes<32> {
        keccak256(self.commands.abi_encode_packed())
    }

    pub fn state_hash(&self) -> FixedBytes<32> {
        if self.bitmap.is_zero() {
            return FixedBytes::<32>::default();
        }

        let bitmask = U128::from(170141183460469231731687303715884105728u128);
        let mut preimage = vec![];
        for (i, state) in self.state.iter().enumerate() {
            if self.bitmap & (bitmask >> i) != U128::ZERO {
                preimage.push(keccak256(state.abi_encode_packed()));
            }
        }

        keccak256(preimage.abi_encode_packed())
    }

    pub fn affected_tokens_hash(&self) -> FixedBytes<32> {
        keccak256(self.affected_tokens.abi_encode_packed())
    }

    pub fn hash(&self) -> FixedBytes<32> {
        let preimage = (
            self.commands_hash(),
            self.state_hash(),
            U256::from(self.bitmap),
            self.position_id,
            self.is_debt,
            self.group_id,
            self.affected_tokens_hash(),
            U256::from(self.instruction_type as u8),
        );

        keccak256(preimage.abi_encode())
    }
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct Rootfile {
    pub instructions: BTreeMap<String, BTreeMap<String, BTreeMap<String, MakinaInstruction>>>,
}

impl FromStr for Rootfile {
    type Err = toml::de::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        toml::from_str(s)
    }
}

impl From<Vec<NamedMakinaInstruction>> for Rootfile {
    fn from(instructions: Vec<NamedMakinaInstruction>) -> Self {
        let mut rootfile = Rootfile::default();

        for inst in instructions {
            rootfile
                .instructions
                .entry(inst.protocol_name)
                .or_default()
                .entry(inst.instruction_name)
                .or_default()
                .insert(inst.token_name, inst.inner);
        }

        rootfile
    }
}

impl Rootfile {
    pub fn all_instructions(&self) -> Vec<NamedMakinaInstruction> {
        let mut instructions = vec![];
        for (protocol, protocol_inst) in self.instructions.iter() {
            for (instruction_name, token_inst) in protocol_inst.iter() {
                for (token, instruction) in token_inst.iter() {
                    instructions.push(NamedMakinaInstruction {
                        inner: instruction.clone(),
                        instruction_name: instruction_name.clone(),
                        protocol_name: protocol.clone(),
                        token_name: token.clone(),
                    });
                }
            }
        }
        instructions
    }

    /// returns all accounting instructions included in the rootfile
    pub fn accounting_instructions(&self) -> Vec<NamedMakinaInstruction> {
        self.all_instructions()
            .iter()
            .filter(|&i| i.inner.instruction_type == InstructionType::Accounting)
            .cloned()
            .collect()
    }

    /// returns all instructions associated with the protocol
    pub fn protocol_instructions(&self, protocol: &str) -> Vec<NamedMakinaInstruction> {
        self.all_instructions()
            .iter()
            .filter(|&i| i.protocol_name == protocol)
            .cloned()
            .collect()
    }

    /// returns the merkletree built from this rootfile
    pub fn merkletree(&self) -> MerkleTree {
        let leafs = self
            .all_instructions()
            .iter()
            .map(|i| i.inner.hash())
            .collect();
        MerkleTree::new(leafs)
    }

    /// returns root of this files merkletree
    pub fn root(&self) -> FixedBytes<32> {
        *self.merkletree().get_root()
    }
}
