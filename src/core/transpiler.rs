use indexmap::IndexMap;
use miette::miette;
use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap};

use alloy::{
    dyn_abi::{DynSolType, DynSolValue},
    primitives::{Address, Bytes, FixedBytes, U128, U256, keccak256},
};
use weiroll::{
    Planner,
    cmds::{CommandFlags, Literal, ReturnValue, Value},
};

use crate::types::InstructionType;
use crate::{
    core::abi_encode_call::{self, ArgWord, Emitter, MathOp},
    core::parser::{
        blueprint::{
            AbiEncodeCall, Blueprint, BlueprintAction, BlueprintCall, BlueprintInputSlot,
            BlueprintInputType, BlueprintParameter, BlueprintParser, BlueprintReservedSlot,
            BlueprintTarget,
        },
        positions::types::{Instruction, InstructionDefinition, Position, SolValue},
    },
    token_list::TokenInfo,
    types::{
        InputType, MakinaInstruction, MetaTypeData, NamedMakinaInstruction, Rootfile,
        RootfileInputSlot,
    },
};

/// Transpile positions into a makina rootfile. Include tokens used in the transpilation.
/// Returns a [Rootfile] on success.
pub fn get_rootfile_from_positions(
    positions: &[Position],
    tokens_used: &BTreeMap<String, TokenInfo>,
) -> miette::Result<Rootfile> {
    let mut instructions = Vec::new();

    for position in positions {
        instructions.extend(create_rootfile_instructions(position)?);
    }

    let rootfile: Rootfile = instructions.into();
    Ok(rootfile.with_tokens(tokens_used.clone()))
}

pub fn create_rootfile_instructions(
    position: &Position,
) -> miette::Result<Vec<NamedMakinaInstruction>> {
    let mut instructions = Vec::new();

    for inst in &position.instructions {
        // Note: even though Management instructions can also receive position tokens they are not actually used during execution.
        let position_tokens = if inst.instruction_type == InstructionType::Accounting {
            &position.position_tokens
        } else {
            &vec![]
        };

        let (protocol_name, instruction_name, token_name, mut inner) =
            transpile(inst, position.id, position.group_id, position_tokens)?;

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

pub(crate) fn transpile(
    inst: &Instruction,
    id: U256,
    group_id: U256,
    position_tokens: &[Address],
) -> miette::Result<(String, String, String, MakinaInstruction)> {
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

    for (name, supplied) in &inst.definition.inputs {
        let declared = blueprint.inputs.values().find(|input| &input.name == name).ok_or_else(|| {
            miette!(
                "instruction \"{}\" supplies input \"{name}\", which blueprint {} does not declare",
                inst.definition.label,
                blueprint.path.display()
            )
        })?;
        if supplied.r#type != declared.r#type {
            return Err(miette!(
                "instruction \"{}\": input \"{name}\" is {}, but blueprint {} declares {}",
                inst.definition.label,
                supplied.r#type,
                blueprint.path.display(),
                declared.r#type
            ));
        }
    }

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
        let target = transpile_target(&call.target, &inst.definition, &blueprint);

        let args = transpile_parameters(
            &call.parameters,
            &mut Context {
                planner: &mut planner,
                returns_mapping: &action.returns_mapping,
                input_slots: &action.input_slots,
                definition: &inst.definition,
                return_values: &return_values,
                checked_slots: &mut checked_slots,
                blueprint: &blueprint,
            },
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
    let inputs_slots = transpile_input_slots(action, &inst.definition)?;
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
        position_tokens: Vec::from(position_tokens),
        commands: commands.iter().map(|c| FixedBytes::from_slice(c)).collect(),
        state,
        bitmap,
        inputs_slots,
    };

    Ok((protocol, name, label, makina_inst))
}

fn transpile_target(
    target: &BlueprintTarget,
    definition: &InstructionDefinition,
    blueprint: &Blueprint,
) -> Address {
    match target {
        BlueprintTarget::Address(target) => *target,
        BlueprintTarget::Input(name) => {
            let (_, input) = definition
                .inputs
                .iter()
                .find(|(label, _)| label == &name)
                .unwrap_or_else(|| {
                    let available: Vec<_> = definition.inputs.keys().collect();
                    let required: Vec<_> = blueprint.inputs.keys().collect();
                    panic!(
                        "could not find input \"{name}\"\n  \
                         blueprint: {}\n  \
                         available inputs: {available:?}\n  \
                         required inputs: {required:?}",
                        blueprint.path.display()
                    )
                });

            input.value.as_address().expect("target must be address")
        }
    }
}

/// What parameter transpilation reads, and where it records committed state and helper calls.
struct Context<'a> {
    planner: &'a mut Planner,
    returns_mapping: &'a HashMap<String, usize>,
    input_slots: &'a IndexMap<String, BlueprintInputSlot>,
    definition: &'a InstructionDefinition,
    return_values: &'a [ReturnValue],
    checked_slots: &'a mut Vec<Bytes>,
    blueprint: &'a Blueprint,
}

fn transpile_parameters(parameters: &[BlueprintParameter], ctx: &mut Context) -> Vec<Value> {
    parameters
        .iter()
        .map(|parameter| transpile_parameter(parameter, ctx))
        .collect()
}

fn find_input<'a>(ctx: &'a Context, name: &str) -> &'a SolValue {
    ctx.definition
        .inputs
        .iter()
        .find(|(label, _)| *label == name)
        .map(|(_, input)| input)
        .unwrap_or_else(|| {
            let available: Vec<_> = ctx.definition.inputs.keys().collect();
            let required: Vec<_> = ctx.blueprint.inputs.keys().collect();
            panic!(
                "could not find input \"{name}\"\n  \
                 blueprint: {}\n  \
                 available inputs: {available:?}\n  \
                 required inputs: {required:?}",
                ctx.blueprint.path.display()
            )
        })
}

fn transpile_parameter(parameter: &BlueprintParameter, ctx: &mut Context) -> Value {
    match parameter {
        BlueprintParameter::Input(name) => {
            let input = find_input(ctx, name);

            // inputs are one word (checked when parsing both sides)
            let encoded: Bytes = input.value.abi_encode_params().into();

            ctx.checked_slots.push(encoded.clone());

            Value::Literal(Literal::new(encoded, false))
        }
        BlueprintParameter::InputSlot(name) => {
            let (name, input) = ctx
                .input_slots
                .iter()
                .find(|(label, _)| label == &name)
                .expect("could not find input slot");

            let name_hash: Bytes = keccak256(name.as_bytes()).into();

            Value::Literal(Literal::new(name_hash, is_type_dynamic(&input.r#type)))
        }
        BlueprintParameter::Return(name) => {
            let index = ctx
                .returns_mapping
                .get(name)
                .expect("could not get return index");
            let ret = ctx
                .return_values
                .get(*index)
                .expect("return index does not exist")
                .clone();

            Value::Return(ret)
        }
        BlueprintParameter::DynamicArray(params) => {
            Value::Array(params.iter().map(|p| transpile_parameter(p, ctx)).collect())
        }
        BlueprintParameter::Tuple(params) => {
            Value::Tuple(params.iter().map(|p| transpile_parameter(p, ctx)).collect())
        }
        BlueprintParameter::FixedArray(params, _) => {
            Value::FixedArray(params.iter().map(|p| transpile_parameter(p, ctx)).collect())
        }
        BlueprintParameter::Scalar(value) => scalar_literal(&value.value, ctx),
        BlueprintParameter::AbiEncodeCall(call) => transpile_abi_encode_call(call, ctx),
    }
}

/// A compile-time value as a committed state literal.
fn scalar_literal(value: &DynSolValue, ctx: &mut Context) -> Value {
    let mut encoded = value.abi_encode();

    if matches!(value, DynSolValue::String(_) | DynSolValue::Bytes(_)) {
        encoded = encoded.iter().skip(32).cloned().collect();
    }

    ctx.checked_slots.push(encoded.clone().into());

    Value::Literal(Literal::new(encoded.into(), value.is_dynamic()))
}

/// The value of a parameter if it is known at compile time.
fn const_value(parameter: &BlueprintParameter, ctx: &Context) -> Option<DynSolValue> {
    match parameter {
        BlueprintParameter::Scalar(value) => Some(value.value.clone()),
        BlueprintParameter::Input(name) => Some(find_input(ctx, name).value.clone()),
        BlueprintParameter::Return(_) | BlueprintParameter::InputSlot(_) => None,
        BlueprintParameter::Tuple(params) => params
            .iter()
            .map(|p| const_value(p, ctx))
            .collect::<Option<_>>()
            .map(DynSolValue::Tuple),
        BlueprintParameter::DynamicArray(params) => params
            .iter()
            .map(|p| const_value(p, ctx))
            .collect::<Option<_>>()
            .map(DynSolValue::Array),
        BlueprintParameter::FixedArray(params, _) => params
            .iter()
            .map(|p| const_value(p, ctx))
            .collect::<Option<_>>()
            .map(DynSolValue::FixedArray),
        BlueprintParameter::AbiEncodeCall(call) => {
            let args = call
                .parameters
                .iter()
                .map(|p| const_value(p, ctx))
                .collect::<Option<Vec<_>>>()?;
            let mut calldata = call.selector.to_vec();
            calldata.extend(DynSolValue::Tuple(args).abi_encode_params());
            Some(DynSolValue::Bytes(calldata))
        }
    }
}

/// `abi_encode_call`: a literal when every argument is known at compile time; otherwise
/// helper calls that splice the runtime words, passed as a dynamic tuple (see
/// [`abi_encode_call`]). The parser guarantees the runtime case only has single-word
/// static arguments and a `math_helper`.
fn transpile_abi_encode_call(call: &AbiEncodeCall, ctx: &mut Context) -> Value {
    let wrapped = BlueprintParameter::AbiEncodeCall(Box::new(call.clone()));
    if let Some(calldata) = const_value(&wrapped, ctx) {
        return scalar_literal(&calldata, ctx);
    }

    let args: Vec<ArgWord<Value>> = call
        .parameters
        .iter()
        .map(|p| match const_value(p, ctx) {
            Some(value) => ArgWord::Const(FixedBytes::from_slice(&value.abi_encode())),
            None => ArgWord::Runtime(transpile_parameter(p, ctx)),
        })
        .collect();

    let helper = transpile_target(
        call.math_helper
            .as_ref()
            .expect("parser requires math_helper for runtime arguments"),
        ctx.definition,
        ctx.blueprint,
    );
    let mut emitter = PlannerEmitter { ctx, helper };
    let (length, words) = abi_encode_call::lower(&mut emitter, call.selector, args);

    let length = emitter.literal(U256::from(length));
    Value::DynamicTuple(std::iter::once(length).chain(words).collect())
}

struct PlannerEmitter<'c, 'a> {
    ctx: &'c mut Context<'a>,
    helper: Address,
}

impl Emitter for PlannerEmitter<'_, '_> {
    type Value = Value;

    fn literal(&mut self, word: U256) -> Value {
        scalar_literal(&DynSolValue::Uint(word, 256), self.ctx)
    }

    fn op(&mut self, op: MathOp, a: Value, b: Value) -> Value {
        self.ctx
            .planner
            .call(
                self.helper,
                CommandFlags::CALL,
                op.selector().into(),
                vec![a, b],
                DynSolType::Uint(256),
                None,
            )
            .expect("could not add helper call")
            .into()
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

fn transpile_input_slots(
    action: &BlueprintAction,
    definition: &InstructionDefinition,
) -> miette::Result<Vec<RootfileInputSlot>> {
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
                None => None,
            };
            Ok(RootfileInputSlot {
                name: k.into(),
                description: v.description.clone(),
                r#type: v.r#type.clone().into(),
                index: i + action.reserved_slots.len(),
                input_type,
                meta_type: meta_type_data,
            })
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

    use crate::{
        core::parser::positions::types::{InstructionDefinition, SolValue},
        types::{InputSlotType, InputType, InstructionType},
    };
    use alloy::primitives::{address, map::HashMap, ruint::aliases::U128};

    use super::*;

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

        let error = transpile_input_slots(&blueprint.actions["deposit"], &definition).unwrap_err();
        assert!(
            error
                .to_string()
                .contains("could not resolve amount token from address input \"token\"")
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
        let position_tokens = vec![];
        let (protocol, name, label, m) = transpile(&inst, id, group_id, &position_tokens).unwrap();

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
            instruction_name: name,
            token_name: label,
            protocol_name: protocol,
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
        let position_tokens = vec![];
        let (protocol, name, label, m) = transpile(&inst, id, group_id, &position_tokens).unwrap();

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
