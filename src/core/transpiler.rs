use indexmap::IndexMap;
use miette::miette;
use std::cmp::Ordering;
use std::collections::HashMap;

use alloy::{
    dyn_abi::{DynSolType, DynSolValue},
    primitives::{Address, Bytes, FixedBytes, U128, U256, keccak256},
};
use weiroll::{
    Planner,
    cmds::{CommandFlags, Literal, ReturnValue, Value},
};

use crate::{
    core::parser::{
        blueprint::{
            BlueprintAction, BlueprintCall, BlueprintInputSlot, BlueprintParameter,
            BlueprintParser, BlueprintReservedSlot, BlueprintTarget,
        },
        positions::types::{Instruction, InstructionDefinition, Position},
    },
    types::{MakinaInstruction, MetaTypeData, NamedMakinaInstruction, Rootfile, RootfileInputSlot},
};

pub fn get_rootfile_from_positions(positions: &[Position]) -> miette::Result<Rootfile> {
    let mut instructions = Vec::new();

    for position in positions {
        instructions.extend(create_rootfile_instructions(position)?);
    }

    Ok(instructions.into())
}

pub fn create_rootfile_instructions(
    position: &Position,
) -> miette::Result<Vec<NamedMakinaInstruction>> {
    let mut instructions = Vec::new();

    for inst in &position.instructions {
        let (protocol_name, instruction_name, token_name, mut inner) =
            transpile(inst, position.id, position.group_id)?;

        inner.description = position.description.clone();
        inner.tags = position.get_tags(&instruction_name);

        instructions.push(NamedMakinaInstruction {
            inner,
            instruction_name,
            token_name,
            protocol_name,
        });
    }

    // Check for invalid tags that don't match any position
    for (action, tag) in &position.global_tags {
        if !instructions.iter().any(|i| &i.instruction_name == action) {
            return Err(miette!(
                "invalid tag \"{tag}:{action}\", no action with name \"{action}\" exists"
            ));
        }
    }

    Ok(instructions)
}

fn transpile(
    inst: &Instruction,
    id: U256,
    group_id: U256,
) -> miette::Result<(String, String, String, MakinaInstruction)> {
    // get the blueprint of the instruction
    // we exit early and print the error to get nice diagnostics
    // this can be removed once the general parser also returns miette::Report
    let blueprint = match BlueprintParser::new(&inst.definition.blueprint_path)?.parse() {
        Ok(blueprint) => blueprint,
        Err(err) => {
            eprintln!("{:?}", err);
            return Err(miette!("aborting"));
        }
    };

    let (_name, action) = blueprint
        .actions
        .iter()
        .find(|(name, _)| name == &&inst.definition.name)
        .unwrap_or_else(|| {
            panic!(
                "could not find blueprint with name: {}",
                inst.definition.name
            )
        });

    let mut planner = Planner::default();
    let mut checked_slots = Vec::new();
    let mut return_values = Vec::new();

    for call in action.calls.iter() {
        let target = transpile_target(&call.target, &inst.definition);

        let args = transpile_parameters(
            &call.parameters,
            &action.returns_mapping,
            &action.input_slots,
            &inst.definition,
            &return_values,
            &mut checked_slots,
        );

        let (return_types, command_flags) = resolve_call_metadata(call);

        return_values.push(
            planner
                .call(
                    target,
                    command_flags,
                    call.selector.into(),
                    args,
                    return_types,
                    None, // native value is always None
                )
                .expect("could not add call"),
        );
    }

    let reserved_slots = transpile_reserved_slots(action, &return_values, &mut checked_slots);
    let inputs_slots = transpile_input_slots(action);
    let (commands, state) = planner
        .plan(reserved_slots)
        .map_err(|err| miette!("{}", err))?;
    let bitmap = generate_bitmap(&state, &checked_slots);

    let protocol = blueprint.protocol.clone();
    let label = inst.definition.label.clone();
    let name = inst
        .definition
        .override_name
        .clone()
        .unwrap_or(inst.definition.name.clone());

    let makina_inst = MakinaInstruction {
        position_id: id,
        description: inst.description.clone(),
        tags: vec![],
        is_debt: inst.is_debt,
        group_id,
        instruction_type: inst.instruction_type,
        affected_tokens: inst.affected_tokens.clone(),
        commands: commands.iter().map(|c| FixedBytes::from_slice(c)).collect(),
        state,
        bitmap,
        inputs_slots,
    };

    Ok((protocol, name, label, makina_inst))
}

fn transpile_target(target: &BlueprintTarget, definition: &InstructionDefinition) -> Address {
    match target {
        BlueprintTarget::Address(target) => *target,
        BlueprintTarget::Input(name) => {
            let (_, input) = definition
                .inputs
                .iter()
                .find(|(label, _)| label == &name)
                .expect("could not find input");

            input.value.as_address().expect("target must be address")
        }
    }
}

fn transpile_parameters(
    parameters: &[BlueprintParameter],
    returns_mapping: &HashMap<String, usize>,
    input_slots: &IndexMap<String, BlueprintInputSlot>,
    definition: &InstructionDefinition,
    return_values: &[ReturnValue],
    checked_slots: &mut Vec<Bytes>,
) -> Vec<Value> {
    let mut result = Vec::new();

    for parameter in parameters.iter() {
        let value = transpile_parameter(
            parameter,
            returns_mapping,
            input_slots,
            definition,
            return_values,
            checked_slots,
        );

        result.push(value);
    }

    result
}

fn transpile_parameter(
    parameter: &BlueprintParameter,
    returns_mapping: &HashMap<String, usize>,
    input_slots: &IndexMap<String, BlueprintInputSlot>,
    definition: &InstructionDefinition,
    return_values: &[ReturnValue],
    checked_slots: &mut Vec<Bytes>,
) -> Value {
    match parameter {
        BlueprintParameter::Input(name) => {
            let (_, input) = definition
                .inputs
                .iter()
                .find(|(label, _)| label == &name)
                .expect("could not find input");

            let encoded: Bytes = match input.r#type {
                DynSolType::String => input
                    .value
                    .abi_encode_params()
                    .into_iter()
                    .skip(32)
                    .collect(),
                _ => input.value.abi_encode_params().into(),
            };

            checked_slots.push(encoded.clone());

            Value::Literal(Literal::new(encoded, input.value.is_dynamic()))
        }
        BlueprintParameter::InputSlot(name) => {
            let (name, input) = input_slots
                .iter()
                .find(|(label, _)| label == &name)
                .expect("could not find input slot");

            let name_hash: Bytes = keccak256(name.as_bytes()).into();

            Value::Literal(Literal::new(name_hash, is_type_dynamic(&input.r#type)))
        }
        BlueprintParameter::Return(name) => {
            let index = returns_mapping
                .get(name)
                .expect("could not get return index");
            let ret = return_values
                .get(*index)
                .expect("return index does not exist")
                .clone();

            Value::Return(ret)
        }
        BlueprintParameter::DynamicArray(params) => {
            let mut values = Vec::new();

            for param in params.iter() {
                let value = transpile_parameter(
                    param,
                    returns_mapping,
                    input_slots,
                    definition,
                    return_values,
                    checked_slots,
                );
                values.push(value);
            }

            Value::Array(values)
        }
        BlueprintParameter::Tuple(params) => {
            let mut values = Vec::new();

            for param in params.iter() {
                let value = transpile_parameter(
                    param,
                    returns_mapping,
                    input_slots,
                    definition,
                    return_values,
                    checked_slots,
                );
                values.push(value);
            }

            Value::Tuple(values)
        }
        BlueprintParameter::FixedArray(params, _) => {
            let mut values = Vec::new();

            for param in params.iter() {
                let value = transpile_parameter(
                    param,
                    returns_mapping,
                    input_slots,
                    definition,
                    return_values,
                    checked_slots,
                );
                values.push(value);
            }

            Value::FixedArray(values)
        }
        BlueprintParameter::Scalar(value) => {
            let mut encoded = value.value.abi_encode();

            if matches!(value.value, DynSolValue::String(_) | DynSolValue::Bytes(_)) {
                encoded = encoded.iter().skip(32).cloned().collect();
            }

            checked_slots.push(encoded.clone().into());

            Value::Literal(Literal::new(encoded.into(), value.value.is_dynamic()))
        }
    }
}

fn transpile_reserved_slots(
    action: &BlueprintAction,
    return_values: &[ReturnValue],
    checked_slots: &mut Vec<Bytes>,
) -> Vec<Value> {
    let mut reserved_slots: Vec<_> = action
        .reserved_slots
        .iter()
        .map(|rs| match rs {
            BlueprintReservedSlot::Return(name) => {
                let index = action
                    .returns_mapping
                    .get(name)
                    .expect("could not get return index");
                let ret = return_values
                    .get(*index)
                    .expect("return index does not exist")
                    .clone();
                Value::Return(ret)
            }
            BlueprintReservedSlot::Scalar(value) => {
                let encoded: Bytes = value.value.abi_encode_params().into();
                checked_slots.push(encoded.clone());
                Value::Literal(Literal::new(encoded, false))
            }
        })
        .collect();

    reserved_slots.extend(action.input_slots.iter().map(|(name, is)| {
        let is_dynamic = matches!(
            is.r#type,
            DynSolType::Bytes | DynSolType::String | DynSolType::Array(_)
        );
        Value::Literal(Literal::new(keccak256(name).into(), is_dynamic))
    }));

    reserved_slots
}

fn transpile_input_slots(action: &BlueprintAction) -> Vec<RootfileInputSlot> {
    action
        .input_slots
        .iter()
        .enumerate()
        .map(|(i, (k, v))| {
            let meta_type_data = v.meta_type.as_ref().map(|meta_type| MetaTypeData {
                r#type: meta_type.to_string(),
                name: v.meta_type_name.clone().unwrap_or_default(),
                field: v.meta_type_field.clone().unwrap_or_default(),
            });
            RootfileInputSlot {
                name: k.into(),
                r#type: v.r#type.clone().into(),
                index: i + action.reserved_slots.len(),
                meta_type: meta_type_data,
            }
        })
        .collect()
}

fn generate_bitmap(state: &[Bytes], checked_slots: &[Bytes]) -> U128 {
    let mut bitmap = 0u128;
    for (i, s) in state.iter().enumerate() {
        if checked_slots.contains(s) {
            bitmap |= 1u128 << (127 - i);
        }
    }
    U128::from(bitmap)
}

fn is_type_dynamic(ty: &DynSolType) -> bool {
    match ty {
        DynSolType::Address
        | DynSolType::Function
        | DynSolType::Bool
        | DynSolType::Uint(..)
        | DynSolType::Int(..)
        | DynSolType::FixedBytes(..) => false,
        DynSolType::Bytes | DynSolType::String | DynSolType::Array(_) => true,
        DynSolType::Tuple(tuple) => tuple.iter().any(is_type_dynamic),
        DynSolType::FixedArray(inner, _) => is_type_dynamic(inner),
        DynSolType::CustomStruct { tuple, .. } => tuple.iter().any(is_type_dynamic),
    }
}

fn resolve_call_metadata(call: &BlueprintCall) -> (DynSolType, CommandFlags) {
    let mut return_type = call
        .r#return
        .as_ref()
        .map(|ret| ret.r#type.clone())
        .unwrap_or_else(|| DynSolType::Tuple(vec![DynSolType::Bool]));

    // we always want the return type to be a tuple
    if !matches!(return_type, DynSolType::Tuple(_)) {
        return_type = DynSolType::Tuple(vec![return_type]);
    }

    let mut command_flags = CommandFlags::CALL;

    if let DynSolType::Tuple(t) = return_type.clone() {
        match t.len().cmp(&1) {
            Ordering::Greater => command_flags |= CommandFlags::TUPLE_RETURN,
            Ordering::Equal => return_type = t[0].clone(),
            Ordering::Less => (),
        }
    };

    (return_type, command_flags)
}

#[cfg(test)]
mod test {
    use std::str::FromStr;

    use alloy::primitives::{address, map::HashMap, ruint::aliases::U128};

    use crate::{
        core::parser::positions::types::{InstructionDefinition, SolValue},
        types::{InputSlotType, InstructionType},
    };

    use super::*;

    // expected outputs taken from makina rootfile generated by registry-merkletree

    #[test]
    fn test_aave_add_collateral() {
        let mut inputs = HashMap::new();
        let on_behalf_of = SolValue {
            r#type: DynSolType::Address,
            value: address!("0x158f44107cecf59e281a9e9f0a5680ea2867a5cd").into(),
            description: None,
        };
        let token = SolValue {
            r#type: DynSolType::Address,
            value: address!("0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2").into(),
            description: None,
        };
        inputs.insert("on_behalf_of".into(), on_behalf_of);
        inputs.insert("token".into(), token);

        let affected_tokens = vec![address!("0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2")];
        let inst = Instruction {
            description: None,
            is_debt: false,
            affected_tokens: affected_tokens.clone(),
            instruction_type: InstructionType::Management,
            definition: InstructionDefinition {
                blueprint_path: "test_data/blueprints/deposit.yaml".parse().unwrap(),
                label: "weth".into(),
                name: "deposit".into(),
                override_name: None,
                inputs,
            },
        };

        let id = U256::from(123);
        let group_id = U256::from(1);
        let (protocol, name, label, m) = transpile(&inst, id, group_id).unwrap();

        assert_eq!(protocol, "aavev3");
        assert_eq!(name, "deposit");
        assert_eq!(label, "weth");

        assert_eq!(m.position_id, id);
        assert_eq!(m.affected_tokens, affected_tokens);

        assert_eq!(m.commands.len(), 2);
        //    sel      f  inputs              o  target
        // 0x 095ea7b3 01 01 04       ff00000 ff c02aaa39b223fe8d0a0e5c4f27ead9083c756cc2
        // 0x 617ba037 01 02 00 03 04    ff00 ff 87870bca3f3fd6335c3f4ce8392d69350b4fa4e2
        assert_eq!(
            m.commands[0].to_string(),
            "0x095ea7b3010100ff000000ffc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2"
        );
        assert_eq!(
            m.commands[1].to_string(),
            "0x617ba0370102000304ff00ff87870bca3f3fd6335c3f4ce8392d69350b4fa4e2"
        );

        assert_eq!(m.state.len(), 5);
        let state = vec![
            Bytes::from_str("0x99a8daf1b71f292524ef087d2fc434cdacbce67100d981ca25cecd529417368e")
                .unwrap(),
            Bytes::from_str("0x00000000000000000000000087870bca3f3fd6335c3f4ce8392d69350b4fa4e2")
                .unwrap(),
            Bytes::from_str("0x000000000000000000000000c02aaa39b223fe8d0a0e5c4f27ead9083c756cc2")
                .unwrap(),
            Bytes::from_str("0x000000000000000000000000158f44107cecf59e281a9e9f0a5680ea2867a5cd")
                .unwrap(),
            Bytes::from_str("0x0000000000000000000000000000000000000000000000000000000000000000")
                .unwrap(),
        ];
        assert_eq!(m.state, state);

        assert_eq!(
            m.bitmap,
            U128::from(159507359494189904748456847233641349120u128)
        );

        assert_eq!(m.inputs_slots.len(), 1);
        assert_eq!(m.inputs_slots[0].index, 0);
        assert_eq!(m.inputs_slots[0].name, "amount_in");
        assert!(matches!(m.inputs_slots[0].r#type, InputSlotType::Uint));
    }

    #[test]
    fn test_aave_account_weth() {
        let affected_tokens = vec![address!("0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2")];
        let mut inputs = HashMap::new();
        let aave_token = SolValue {
            r#type: DynSolType::Address,
            value: address!("0x4d5f47fa6a74757f35c14fd3a6ef8e3c9bc514e8").into(),
            description: None,
        };
        let account = SolValue {
            r#type: DynSolType::Address,
            value: address!("0x158f44107cecf59e281a9e9f0a5680ea2867a5cd").into(),
            description: None,
        };
        inputs.insert("aave_token".into(), aave_token);
        inputs.insert("account".into(), account);

        let inst = Instruction {
            description: None,
            is_debt: false,
            instruction_type: InstructionType::Accounting,
            affected_tokens: affected_tokens.clone(),
            definition: InstructionDefinition {
                inputs,
                label: "weth".into(),
                override_name: None,
                name: "account".into(),
                blueprint_path: "test_data/blueprints/account.yaml".parse().unwrap(),
            },
        };

        let id = U256::from(123);
        let group_id = U256::from(1);
        let (protocol, name, label, m) = transpile(&inst, id, group_id).unwrap();

        assert_eq!(protocol, "aavev3");
        assert_eq!(name, "account");
        assert_eq!(label, "weth");

        assert_eq!(m.position_id, id);
        assert!(!m.is_debt);
        assert!(matches!(m.instruction_type, InstructionType::Accounting));

        assert_eq!(m.commands.len(), 1);
        //    sel      f  inputs              o  target
        // 0x 70a08231 01 02       ff00000000 00 4d5f47fa6a74757f35c14fd3a6ef8e3c9bc514e8

        assert_eq!(
            m.commands[0].to_string(),
            "0x70a082310102ff00000000004d5f47fa6a74757f35c14fd3a6ef8e3c9bc514e8"
        );

        assert_eq!(m.state.len(), 3);
        assert_eq!(m.state[0], Bytes::from_str("0x").unwrap());
        assert_eq!(
            m.state[1],
            Bytes::from_str("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")
                .unwrap()
        );
        assert_eq!(
            m.state[2],
            Bytes::from_str("0x000000000000000000000000158f44107cecf59e281a9e9f0a5680ea2867a5cd")
                .unwrap()
        );

        assert_eq!(
            m.bitmap,
            U128::from(127605887595351923798765477786913079296u128)
        );
        assert_eq!(m.inputs_slots.len(), 0);
    }

    #[test]
    fn test_position_with_tags() {
        let mut inputs = HashMap::new();
        let on_behalf_of = SolValue {
            r#type: DynSolType::Address,
            value: address!("0x158f44107cecf59e281a9e9f0a5680ea2867a5cd").into(),
            description: None,
        };
        let token = SolValue {
            r#type: DynSolType::Address,
            value: address!("0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2").into(),
            description: None,
        };
        inputs.insert("on_behalf_of".into(), on_behalf_of);
        inputs.insert("token".into(), token);

        let affected_tokens = vec![address!("0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2")];
        let inst = Instruction {
            description: None,
            is_debt: false,
            affected_tokens: affected_tokens.clone(),
            instruction_type: InstructionType::Management,
            definition: InstructionDefinition {
                blueprint_path: "test_data/blueprints/deposit.yaml".parse().unwrap(),
                label: "weth".into(),
                name: "deposit".into(),
                override_name: None,
                inputs,
            },
        };

        let pos = Position {
            id: U256::ZERO,
            group_id: U256::ZERO,
            description: None,
            instructions: vec![inst],
            global_tags: vec![("deposit".to_string(), "test_tag".to_string())],
        };

        let transpiled = create_rootfile_instructions(&pos).unwrap();
        assert_eq!(transpiled.len(), 1);

        let makina_inst = transpiled.first().unwrap();
        assert_eq!(makina_inst.inner.tags.len(), 1);
        assert_eq!(
            makina_inst.inner.tags.first(),
            Some(&String::from("test_tag"))
        );
    }

    #[test]
    fn test_reject_tag_with_invalid_instruction() {
        let mut inputs = HashMap::new();
        let on_behalf_of = SolValue {
            r#type: DynSolType::Address,
            value: address!("0x158f44107cecf59e281a9e9f0a5680ea2867a5cd").into(),
            description: None,
        };
        let token = SolValue {
            r#type: DynSolType::Address,
            value: address!("0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2").into(),
            description: None,
        };
        inputs.insert("on_behalf_of".into(), on_behalf_of);
        inputs.insert("token".into(), token);

        let affected_tokens = vec![address!("0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2")];
        let inst = Instruction {
            description: None,
            is_debt: false,
            affected_tokens: affected_tokens.clone(),
            instruction_type: InstructionType::Management,
            definition: InstructionDefinition {
                blueprint_path: "test_data/blueprints/deposit.yaml".parse().unwrap(),
                label: "weth".into(),
                name: "deposit".into(),
                override_name: None,
                inputs,
            },
        };

        let pos = Position {
            id: U256::ZERO,
            group_id: U256::ZERO,
            description: None,
            instructions: vec![inst],
            global_tags: vec![("unknown".to_string(), "test_tag".to_string())],
        };

        let err = create_rootfile_instructions(&pos).unwrap_err().to_string();
        err.find("no action with name").unwrap();
    }
}
