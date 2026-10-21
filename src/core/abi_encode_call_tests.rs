//! End-to-end transpilation of `abi_encode_call` (test_data/blueprints/safe-module.yaml).

use alloy::{
    dyn_abi::{DynSolType, DynSolValue},
    primitives::{Address, Bytes, U256, address, keccak256, ruint::aliases::U128},
};
use std::collections::HashMap;

use crate::{
    core::{
        parser::positions::types::{Instruction, InstructionDefinition, SolValue},
        transpiler::transpile,
    },
    types::InstructionType,
};

fn safe_module_instruction(name: &str) -> Instruction {
    let mut inputs = HashMap::new();
    for (key, value) in [
        (
            "safe",
            address!("0x71331B157234ca87F0A53F7F89743e3f73Fc5E0b"),
        ),
        (
            "token",
            address!("0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"),
        ),
        (
            "recipient",
            address!("0xdc385c31c322351dac9ff4d7ded24e29b5d94768"),
        ),
    ] {
        inputs.insert(
            key.into(),
            SolValue {
                r#type: DynSolType::Address,
                value: value.into(),
                description: None,
            },
        );
    }
    Instruction {
        description: None,
        is_debt: false,
        affected_tokens: vec![],
        instruction_type: InstructionType::Harvest,
        definition: InstructionDefinition {
            blueprint_path: "test_data/blueprints/safe-module.yaml".parse().unwrap(),
            label: "safe".into(),
            name: name.into(),
            override_name: None,
            inputs,
        },
    }
}

#[test]
fn test_abi_encode_call_compile_time_is_a_literal() {
    let (_, _, _, m) = transpile(
        &safe_module_instruction("approve"),
        U256::from(1),
        U256::ZERO,
        &[],
    )
    .unwrap();

    // No helper calls: just the Safe call.
    assert_eq!(m.commands.len(), 1);

    let calldata = DynSolValue::Bytes(
        [
            keccak256("approve(address,uint256)")[..4].to_vec(),
            DynSolValue::Tuple(vec![
                address!("0xdc385c31c322351dac9ff4d7ded24e29b5d94768").into(),
                U256::MAX.into(),
            ])
            .abi_encode_params(),
        ]
        .concat(),
    );
    let literal: Bytes = calldata.abi_encode()[32..].to_vec().into();
    let index = m
        .state
        .iter()
        .position(|s| *s == literal)
        .expect("calldata literal in state");
    // committed in the merkle leaf
    assert_ne!(m.bitmap & (U128::from(1) << (127 - index)), U128::ZERO);
}

#[test]
fn test_abi_encode_call_runtime_splices_words() {
    let (_, _, _, m) = transpile(
        &safe_module_instruction("sweep"),
        U256::from(1),
        U256::ZERO,
        &[],
    )
    .unwrap();

    // balanceOf, div/mul/sub/mul to split the balance, add to join it, the Safe call
    // (extended: 9 argument slots), so 8 command words.
    assert_eq!(m.commands.len(), 8);
    let helper = address!("0x3D623B199E290358416415eA7e05B635E442e3c0");
    let helper_selectors: Vec<String> = m.commands[1..6]
        .iter()
        .inspect(|c| assert_eq!(Address::from_slice(&c[12..]), helper))
        .map(|c| c[..4].to_vec().into())
        .map(|b: Bytes| b.to_string())
        .collect();
    let sel = |s: &str| Bytes::from(keccak256(s)[..4].to_vec()).to_string();
    assert_eq!(
        helper_selectors,
        [
            sel("div(uint256,uint256)"),
            sel("mul(uint256,uint256)"),
            sel("sub(uint256,uint256)"),
            sel("mul(uint256,uint256)"),
            sel("add(uint256,uint256)"),
        ]
    );

    // The bytes argument is a dynamic tuple: |fc| length w0 w1 w2 |fb|
    let indices = &m.commands[7];
    assert_eq!(indices[2], 0xfc);
    assert_eq!(indices[7], 0xfb);

    // Committed constants: the length (68), w0 (selector ‖ recipient >> 32), and the
    // recipient's low 4 bytes shifted to the top (added to the balance's high part).
    let word = |w: U256| Bytes::from(w.to_be_bytes::<32>().to_vec());
    let recipient =
        U256::from_be_slice(address!("0xdc385c31c322351dac9ff4d7ded24e29b5d94768").as_slice());
    let selector = U256::from_be_slice(&keccak256("transfer(address,uint256)")[..4]);
    for committed in [
        U256::from(68),
        (selector << 224) | (recipient >> 32),
        (recipient & U256::from(0xffff_ffffu64)) << 224,
    ] {
        let index = m
            .state
            .iter()
            .position(|s| *s == word(committed))
            .expect("constant in state");
        assert_ne!(m.bitmap & (U128::from(1) << (127 - index)), U128::ZERO);
    }
}
