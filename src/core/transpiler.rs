use miette::{WrapErr, miette};
use std::collections::BTreeMap;

use alloy::primitives::Address;

use crate::types::InstructionType;
use crate::{
    core::{
        codegen::{lower_instruction, lower_program},
        ir,
        parser::{
            blueprint::{
                Blueprint, BlueprintAction, BlueprintInputType, BlueprintParameter,
                BlueprintParser, BlueprintReservedSlot, BlueprintTarget,
            },
            positions::types::{Instruction, InstructionDefinition, Position, SolValue},
        },
    },
    token_list::TokenInfo,
    types::{InputType, MetaTypeData, NamedMakinaInstruction, Rootfile},
};

/// Transpile positions into a makina rootfile. Include tokens used in the transpilation.
/// Returns a [Rootfile] on success.
pub fn get_rootfile_from_positions(
    positions: &[Position],
    tokens_used: &BTreeMap<String, TokenInfo>,
) -> miette::Result<Rootfile> {
    lower_program(&bind_program(positions, tokens_used)?)
}

pub fn bind_program(
    positions: &[Position],
    tokens_used: &BTreeMap<String, TokenInfo>,
) -> miette::Result<ir::Program> {
    Ok(ir::Program {
        positions: positions
            .iter()
            .map(bind_position)
            .collect::<miette::Result<_>>()?,
        tokens: tokens_used.clone(),
    })
}

pub fn create_rootfile_instructions(
    position: &Position,
) -> miette::Result<Vec<NamedMakinaInstruction>> {
    bind_position(position)?
        .instructions
        .iter()
        .map(lower_instruction)
        .collect()
}

fn bind_position(position: &Position) -> miette::Result<ir::Position> {
    let instructions = position
        .instructions
        .iter()
        .map(|inst| bind_instruction(position, inst))
        .collect::<miette::Result<Vec<_>>>()?;

    for (action, tag) in &position.global_tags {
        if !instructions.iter().any(|i| &i.name == action) {
            return Err(miette!(
                "invalid tag \"{tag}:{action}\", no action with name \"{action}\" exists"
            ));
        }
    }

    Ok(ir::Position {
        id: position.id,
        group_id: position.group_id,
        instructions,
    })
}

fn bind_instruction(position: &Position, inst: &Instruction) -> miette::Result<ir::Instruction> {
    // get the blueprint of the instruction
    // we exit early and print the error to get nice diagnostics
    // this can be removed once the general parser also returns miette::Report
    let blueprint = match BlueprintParser::new(&inst.definition.blueprint_path)?.parse() {
        Ok(blueprint) => blueprint,
        Err(err) => {
            eprintln!("{err:?}");
            return Err(miette!("aborting"));
        }
    };

    bind(&blueprint, inst, position)
}

/// Resolves the names an instruction's blueprint action refers to (inputs, returns, input
/// slots) into a bound [ir::Instruction].
fn bind(
    blueprint: &Blueprint,
    inst: &Instruction,
    position: &Position,
) -> miette::Result<ir::Instruction> {
    bind_action(blueprint, inst, position).wrap_err_with(|| {
        format!(
            "could not bind instruction \"{}\" (label \"{}\") of position {} with blueprint {}",
            inst.definition.name,
            inst.definition.label,
            position.id,
            blueprint.path.display()
        )
    })
}

fn bind_action(
    blueprint: &Blueprint,
    inst: &Instruction,
    position: &Position,
) -> miette::Result<ir::Instruction> {
    let action = blueprint
        .actions
        .get(&inst.definition.name)
        .ok_or_else(|| {
            let mut available: Vec<_> = blueprint.actions.keys().collect();
            available.sort();
            miette!(
                "blueprint has no action \"{}\"\n  available actions: {available:?}",
                inst.definition.name
            )
        })?;

    check_input_types(blueprint, &inst.definition)?;

    let mut calls: Vec<ir::Call> = Vec::new();

    for call in action.calls.iter() {
        let target = bind_target(&call.target, &inst.definition, blueprint)?;

        let args = call
            .parameters
            .iter()
            .map(|parameter| {
                bind_parameter(parameter, action, calls.len(), &inst.definition, blueprint)
            })
            .collect::<miette::Result<_>>()?;

        calls.push(ir::Call {
            target,
            selector: call.selector,
            args,
            returns: call.r#return.as_ref().map(|ret| ret.r#type.clone()),
        });
    }

    let outputs = action
        .reserved_slots
        .iter()
        .map(|rs| match rs {
            BlueprintReservedSlot::Return(name) => {
                Ok(ir::Output::Return(bind_return(name, action, calls.len())?))
            }
            BlueprintReservedSlot::Scalar(value) => Ok(ir::Output::Literal(value.value.clone())),
        })
        .collect::<miette::Result<_>>()?;

    let slots = bind_slots(action, &inst.definition)?;
    let name = inst
        .definition
        .override_name
        .as_ref()
        .unwrap_or(&inst.definition.name);

    Ok(ir::Instruction {
        protocol: blueprint.protocol.clone(),
        name: name.clone(),
        label: inst.definition.label.clone(),
        position_id: position.id,
        group_id: position.group_id,
        is_debt: inst.is_debt,
        instruction_type: inst.instruction_type,
        affected_tokens: inst.affected_tokens.clone(),
        position_tokens: if inst.instruction_type == InstructionType::Accounting {
            position.position_tokens.clone()
        } else {
            Vec::new()
        },
        description: position.description.clone(),
        tags: position.get_tags(name),
        action: ir::Action {
            calls,
            outputs,
            slots,
        },
    })
}

/// Every supplied input the blueprint declares must have the declared Solidity type, whether
/// or not the action uses it.
fn check_input_types(
    blueprint: &Blueprint,
    definition: &InstructionDefinition,
) -> miette::Result<()> {
    let mut mismatches: Vec<_> = definition
        .inputs
        .iter()
        .filter_map(|(name, supplied)| {
            let declared = blueprint
                .inputs
                .values()
                .find(|input| &input.name == name)?;
            (supplied.r#type != declared.r#type).then(|| {
                format!(
                    "input \"{name}\" is supplied as {} but the blueprint declares {}",
                    supplied.r#type, declared.r#type
                )
            })
        })
        .collect();
    if mismatches.is_empty() {
        return Ok(());
    }
    mismatches.sort();
    Err(miette!("{}", mismatches.join("\n")))
}

fn find_input<'a>(
    name: &str,
    definition: &'a InstructionDefinition,
    blueprint: &Blueprint,
) -> miette::Result<&'a SolValue> {
    definition.inputs.get(name).ok_or_else(|| {
        let mut available: Vec<_> = definition.inputs.keys().collect();
        available.sort();
        let mut required: Vec<_> = blueprint.inputs.values().map(|input| &input.name).collect();
        required.sort();
        miette!(
            "could not find input \"{name}\"\n  \
             available inputs: {available:?}\n  \
             required inputs: {required:?}"
        )
    })
}

fn bind_target(
    target: &BlueprintTarget,
    definition: &InstructionDefinition,
    blueprint: &Blueprint,
) -> miette::Result<Address> {
    match target {
        BlueprintTarget::Address(target) => Ok(*target),
        BlueprintTarget::Input(name) => {
            let input = find_input(name, definition, blueprint)?;
            input.value.as_address().ok_or_else(|| {
                miette!(
                    "call target input \"{name}\" must be an address, got {}",
                    input.r#type
                )
            })
        }
    }
}

/// `available_returns` is the number of calls before the one whose parameter is bound.
fn bind_parameter(
    parameter: &BlueprintParameter,
    action: &BlueprintAction,
    available_returns: usize,
    definition: &InstructionDefinition,
    blueprint: &Blueprint,
) -> miette::Result<ir::Operand> {
    let bind_all = |params: &[BlueprintParameter]| {
        params
            .iter()
            .map(|param| bind_parameter(param, action, available_returns, definition, blueprint))
            .collect::<miette::Result<_>>()
    };

    Ok(match parameter {
        BlueprintParameter::Input(name) => ir::Operand::Literal(ir::Literal::Input(
            find_input(name, definition, blueprint)?.value.clone(),
        )),
        BlueprintParameter::InputSlot(name) => ir::Operand::Slot(
            action
                .input_slots
                .get_index_of(name)
                .ok_or_else(|| miette!("could not find input slot \"{name}\""))?,
        ),
        BlueprintParameter::Return(name) => {
            ir::Operand::Return(bind_return(name, action, available_returns)?)
        }
        BlueprintParameter::DynamicArray(params) => ir::Operand::Array(bind_all(params)?),
        BlueprintParameter::Tuple(params) => ir::Operand::Tuple(bind_all(params)?),
        BlueprintParameter::FixedArray(params, _) => ir::Operand::FixedArray(bind_all(params)?),
        BlueprintParameter::Scalar(value) => {
            ir::Operand::Literal(ir::Literal::Scalar(value.value.clone()))
        }
    })
}

fn bind_return(
    name: &str,
    action: &BlueprintAction,
    available_returns: usize,
) -> miette::Result<usize> {
    let index = *action
        .returns_mapping
        .get(name)
        .ok_or_else(|| miette!("could not find return \"{name}\""))?;
    if index >= available_returns {
        return Err(miette!(
            "return \"{name}\" of call {index} is used before that call returns"
        ));
    }
    Ok(index)
}

fn bind_slots(
    action: &BlueprintAction,
    definition: &InstructionDefinition,
) -> miette::Result<Vec<ir::Slot>> {
    action
        .input_slots
        .iter()
        .map(|(k, v)| {
            let meta_type_data = v.meta_type.as_ref().map(|meta_type| MetaTypeData {
                r#type: meta_type.to_string(),
                name: v.meta_type_name.clone().unwrap_or_default(),
                field: v.meta_type_field.clone().unwrap_or_default(),
            });
            let input_type = match &v.input_type {
                Some(BlueprintInputType::Amount { token }) => {
                    let token = match token {
                        BlueprintTarget::Address(token) => *token,
                        BlueprintTarget::Input(name) => definition
                            .inputs
                            .get(name)
                            .and_then(|input| input.value.as_address())
                            .ok_or_else(|| {
                                miette!(
                                    "input_type for slot \"{k}\" could not resolve amount token from address input \"{name}\""
                                )
                            })?,
                    };
                    Some(InputType::Amount { token })
                }
                Some(BlueprintInputType::Relative {}) => Some(InputType::Relative {}),
                None => None,
            };
            Ok(ir::Slot {
                name: k.into(),
                ty: v.r#type.clone(),
                description: v.description.clone(),
                input_type,
                meta_type: meta_type_data,
            })
        })
        .collect()
}

#[cfg(test)]
mod test {
    use std::str::FromStr;

    use crate::{
        core::codegen::codegen,
        core::parser::blueprint::BlueprintInput,
        core::parser::positions::types::{InstructionDefinition, SolValue},
        types::{InputSlotType, InputType, InstructionType},
    };
    use alloy::{
        dyn_abi::{DynSolType, DynSolValue},
        primitives::{Bytes, U256, address, map::HashMap, ruint::aliases::U128},
    };

    use super::*;

    #[test]
    fn lowers_retained_program_without_sources() {
        use crate::core::parser::positions::parser::PositionParser;
        use std::{fs, path::Path};

        let dir =
            std::env::temp_dir().join(format!("transpiler-program-{}", rand::random::<u64>()));
        let caliber = "caliber_with_token_list_vars.yaml";
        for file in [
            caliber,
            "instructions/generic-deposit.yaml",
            "blueprints/deposit.yaml",
            "blueprints/account.yaml",
            "token_lists/test.json",
        ] {
            let dest = dir.join(file);
            fs::create_dir_all(dest.parent().unwrap()).unwrap();
            fs::copy(Path::new("test_data").join(file), dest).unwrap();
        }
        let mut parsed = PositionParser::new(
            dir.join(caliber),
            Some(dir.join("token_lists/test.json")),
            None,
        )
        .unwrap()
        .parse()
        .unwrap();
        let id = parsed.positions[0].id;
        parsed.positions[1].id = id;
        parsed.positions.insert(
            1,
            Position {
                id,
                group_id: U256::from(17),
                instructions: vec![],
                position_tokens: vec![],
                description: None,
                global_tags: vec![],
            },
        );

        let program = bind_program(&parsed.positions, &parsed.tokens).unwrap();
        drop(parsed);
        fs::remove_dir_all(dir).unwrap();

        assert_eq!(program.positions.len(), 3);
        assert_eq!(program.positions[1].id, id);
        assert_eq!(program.positions[1].group_id, U256::from(17));
        let groups: Vec<Vec<_>> = program
            .positions
            .iter()
            .map(|p| {
                p.instructions
                    .iter()
                    .map(|i| (i.name.as_str(), i.label.as_str()))
                    .collect()
            })
            .collect();
        assert_eq!(
            groups,
            [
                vec![("deposit", "dusd"), ("account", "dusd")],
                vec![],
                vec![("deposit", "deth"), ("account", "deth")]
            ]
        );

        let rootfile = lower_program(&program).unwrap();
        let keys: Vec<_> = rootfile
            .all_instructions()
            .into_iter()
            .map(|i| (i.instruction_name, i.token_name))
            .collect();
        assert_eq!(
            keys,
            [
                ("account", "deth"),
                ("account", "dusd"),
                ("deposit", "deth"),
                ("deposit", "dusd")
            ]
            .map(|(name, label)| (name.into(), label.into()))
        );
        assert_eq!(rootfile.tokens.len(), 2);
        assert_eq!(
            rootfile.tokens["${token_list.mainnet.DUSD}"].name,
            "Dialectic USD"
        );
        assert_eq!(
            rootfile.tokens["${token_list.mainnet.DETH}"].name,
            "Dialectic ETH"
        );
    }

    // expected outputs taken from makina rootfile generated by registry-merkletree

    #[test]
    fn rejects_amount_input_type_when_instruction_token_is_unresolved() {
        let blueprint = BlueprintParser::new("test_data/blueprints/deposit.yaml")
            .unwrap()
            .parse()
            .unwrap();
        let definition = InstructionDefinition {
            blueprint_path: "test_data/blueprints/deposit.yaml".into(),
            name: "deposit".into(),
            label: "weth".into(),
            override_name: None,
            inputs: HashMap::new(),
        };

        let error = bind_slots(&blueprint.actions["deposit"], &definition).unwrap_err();
        assert!(
            error
                .to_string()
                .contains("could not resolve amount token from address input \"token\"")
        );
    }

    #[test]
    fn rejects_unbindable_instructions() {
        let mut blueprint = BlueprintParser::new("test_data/blueprints/deposit.yaml")
            .unwrap()
            .parse()
            .unwrap();
        let address_input = |value| SolValue {
            r#type: DynSolType::Address,
            value: DynSolValue::Address(value),
            description: None,
        };
        let mut position = Position {
            id: U256::from(7),
            group_id: U256::ZERO,
            position_tokens: vec![],
            description: None,
            instructions: vec![Instruction {
                description: None,
                is_debt: false,
                affected_tokens: vec![],
                instruction_type: InstructionType::Management,
                definition: InstructionDefinition {
                    blueprint_path: "test_data/blueprints/deposit.yaml".into(),
                    name: "deposit".into(),
                    label: "weth".into(),
                    override_name: None,
                    inputs: HashMap::from_iter([
                        (
                            "token".into(),
                            address_input(address!("0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2")),
                        ),
                        (
                            "on_behalf_of".into(),
                            address_input(address!("0x158f44107cecf59e281a9e9f0a5680ea2867a5cd")),
                        ),
                    ]),
                },
            }],
            global_tags: vec![],
        };
        let error = |blueprint: &Blueprint, position: &Position| {
            bind(blueprint, &position.instructions[0], position)
                .unwrap_err()
                .chain()
                .map(|error| format!("{error}\n"))
                .collect::<String>()
        };
        fn definition(position: &mut Position) -> &mut InstructionDefinition {
            &mut position.instructions[0].definition
        }

        definition(&mut position).name = "withdraw".into();
        let err = error(&blueprint, &position);
        assert!(
            err.contains("could not bind instruction \"withdraw\" (label \"weth\") of position 7")
        );
        assert!(err.contains("no action \"withdraw\""));
        definition(&mut position).name = "deposit".into();

        let token = definition(&mut position).inputs.remove("token").unwrap();
        definition(&mut position).inputs.insert(
            "token".into(),
            SolValue {
                r#type: DynSolType::Uint(256),
                value: DynSolValue::Uint(
                    U256::from_be_slice(token.value.as_address().unwrap().as_slice()),
                    256,
                ),
                description: None,
            },
        );
        assert!(
            error(&blueprint, &position).contains(
                "input \"token\" is supplied as uint256 but the blueprint declares address"
            )
        );
        definition(&mut position)
            .inputs
            .insert("token".into(), token);
        bind(&blueprint, &position.instructions[0], &position).unwrap();

        let on_behalf_of = definition(&mut position)
            .inputs
            .remove("on_behalf_of")
            .unwrap();
        assert!(error(&blueprint, &position).contains("could not find input \"on_behalf_of\""));
        definition(&mut position)
            .inputs
            .insert("on_behalf_of".into(), on_behalf_of);

        blueprint.inputs.insert(
            "${inputs.referrer}".into(),
            BlueprintInput {
                name: "referrer".into(),
                r#type: DynSolType::Uint(16),
                description: None,
            },
        );
        definition(&mut position).inputs.insert(
            "referrer".into(),
            SolValue {
                r#type: DynSolType::Uint(256),
                value: DynSolValue::Uint(U256::ZERO, 256),
                description: None,
            },
        );
        assert!(error(&blueprint, &position).contains(
            "input \"referrer\" is supplied as uint256 but the blueprint declares uint16"
        ));
        definition(&mut position).inputs.insert(
            "referrer".into(),
            SolValue {
                r#type: DynSolType::Uint(16),
                value: DynSolValue::Uint(U256::ZERO, 16),
                description: None,
            },
        );
        bind(&blueprint, &position.instructions[0], &position).unwrap();

        let mut set_parameter = |parameter| {
            let action = blueprint.actions.get_mut("deposit").unwrap();
            action.calls[0].parameters[1] = parameter;
            action.returns_mapping.insert("balance".into(), 1);
            error(&blueprint, &position)
        };
        assert!(
            set_parameter(BlueprintParameter::InputSlot("missing".into()))
                .contains("could not find input slot \"missing\"")
        );
        assert!(
            set_parameter(BlueprintParameter::Return("balance".into()))
                .contains("return \"balance\" of call 1 is used before that call returns")
        );
        assert!(
            set_parameter(BlueprintParameter::Return("missing".into()))
                .contains("could not find return \"missing\"")
        );
    }

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
            description: Some("instruction description".into()),
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
        let position = Position {
            id,
            group_id: U256::from(1),
            position_tokens: affected_tokens.clone(),
            description: None,
            instructions: vec![inst],
            global_tags: vec![],
        };
        let bound = bind_instruction(&position, &position.instructions[0]).unwrap();
        assert_eq!(bound.description, None);
        assert!(bound.position_tokens.is_empty());
        assert_eq!(bound.protocol, "aavev3");
        assert_eq!(bound.name, "deposit");
        assert_eq!(bound.label, "weth");
        let m = codegen(&bound).unwrap();

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
        assert_eq!(
            m.inputs_slots[0].description.as_deref(),
            Some("The amount of tokens to add as collateral")
        );
        assert!(matches!(m.inputs_slots[0].r#type, InputSlotType::Uint));
        assert_eq!(
            m.inputs_slots[0].input_type,
            Some(InputType::Amount {
                token: address!("0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2")
            })
        );

        let hash = m.hash();
        let root = Rootfile::from(vec![NamedMakinaInstruction {
            inner: m.clone(),
            instruction_name: bound.name,
            token_name: bound.label,
            protocol_name: bound.protocol,
        }])
        .root();
        let mut without_input_type = m;
        without_input_type.inputs_slots[0].input_type = None;
        assert_eq!(without_input_type.hash(), hash);
        assert_eq!(
            Rootfile::from(vec![NamedMakinaInstruction {
                inner: without_input_type,
                instruction_name: "deposit".into(),
                token_name: "weth".into(),
                protocol_name: "aavev3".into(),
            }])
            .root(),
            root
        );
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
            description: Some("instruction description".into()),
            is_debt: false,
            instruction_type: InstructionType::Accounting,
            affected_tokens: affected_tokens.clone(),
            definition: InstructionDefinition {
                inputs,
                label: "weth".into(),
                override_name: Some("balance".into()),
                name: "account".into(),
                blueprint_path: "test_data/blueprints/account.yaml".parse().unwrap(),
            },
        };

        let id = U256::from(123);
        let mut position = Position {
            id,
            group_id: U256::from(1),
            position_tokens: vec![address!("0x4d5f47fa6a74757f35c14fd3a6ef8e3c9bc514e8")],
            description: Some("position description".into()),
            instructions: vec![inst],
            global_tags: vec![
                ("balance".into(), "balance tag".into()),
                ("account".into(), "account tag".into()),
            ],
        };
        let bound = bind_instruction(&position, &position.instructions[0]).unwrap();
        assert_eq!(bound.description.as_deref(), Some("position description"));
        assert_eq!(bound.tags, ["balance tag"]);
        assert_eq!(bound.position_tokens, position.position_tokens);
        assert_eq!(bound.protocol, "aavev3");
        assert_eq!(bound.name, "balance");
        assert_eq!(bound.label, "weth");

        let error = create_rootfile_instructions(&position).unwrap_err();
        assert!(
            error
                .to_string()
                .contains("no action with name \"account\"")
        );
        position.global_tags.pop();
        let compiled = create_rootfile_instructions(&position).unwrap();
        let [instruction] = compiled.as_slice() else {
            panic!("expected one instruction")
        };
        assert_eq!(instruction.instruction_name, "balance");
        let m = &instruction.inner;
        assert_eq!(m.tags, ["balance tag"]);
        assert_eq!(m.description.as_deref(), Some("position description"));

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
            position_tokens: vec![],
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
            position_tokens: vec![],
            description: None,
            instructions: vec![inst],
            global_tags: vec![("unknown".to_string(), "test_tag".to_string())],
        };

        let err = create_rootfile_instructions(&pos).unwrap_err().to_string();
        err.find("no action with name").unwrap();
    }

    #[test]
    fn test_position_tokens_in_accounting_instruction() {
        // Create an Accounting instruction (similar to test_aave_account_weth)
        let mut accounting_inputs = HashMap::new();
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
        accounting_inputs.insert("aave_token".into(), aave_token);
        accounting_inputs.insert("account".into(), account);

        let accounting_inst = Instruction {
            description: None,
            is_debt: false,
            instruction_type: InstructionType::Accounting,
            affected_tokens: vec![address!("0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2")],
            definition: InstructionDefinition {
                inputs: accounting_inputs,
                label: "weth".into(),
                override_name: None,
                name: "account".into(),
                blueprint_path: "test_data/blueprints/account.yaml".parse().unwrap(),
            },
        };

        // Create a Management instruction (should NOT receive position tokens)
        let mut management_inputs = HashMap::new();
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
        management_inputs.insert("on_behalf_of".into(), on_behalf_of);
        management_inputs.insert("token".into(), token);

        let management_inst = Instruction {
            description: None,
            is_debt: false,
            affected_tokens: vec![address!("0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2")],
            instruction_type: InstructionType::Management,
            definition: InstructionDefinition {
                blueprint_path: "test_data/blueprints/deposit.yaml".parse().unwrap(),
                label: "weth".into(),
                name: "deposit".into(),
                override_name: None,
                inputs: management_inputs,
            },
        };

        // Create Position with position_tokens
        let position_tokens = vec![
            address!("0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2"),
            address!("0x4d5f47fa6a74757f35c14fd3a6ef8e3c9bc514e8"),
        ];

        let pos = Position {
            id: U256::from(123),
            group_id: U256::from(1),
            position_tokens: position_tokens.clone(),
            description: None,
            instructions: vec![management_inst, accounting_inst],
            global_tags: vec![],
        };

        // Call create_rootfile_instructions
        let transpiled = create_rootfile_instructions(&pos).unwrap();
        assert_eq!(transpiled.len(), 2);

        // First instruction is Management - should have empty position_tokens
        let management_result = &transpiled[0];
        assert_eq!(management_result.instruction_name, "deposit");
        assert!(
            management_result.inner.position_tokens.is_empty(),
            "Management instruction should have empty position_tokens"
        );

        // Second instruction is Accounting - should have the position's tokens
        let accounting_result = &transpiled[1];
        assert_eq!(accounting_result.instruction_name, "account");
        assert_eq!(
            accounting_result.inner.position_tokens, position_tokens,
            "Accounting instruction should have the position's tokens"
        );
    }
}
