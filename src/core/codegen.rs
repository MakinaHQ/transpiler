//! Weiroll codegen: turns a bound [ir::Instruction] into a [MakinaInstruction].

use miette::miette;
use std::cmp::Ordering;

use alloy::{
    dyn_abi::{DynSolType, DynSolValue},
    primitives::{Bytes, FixedBytes, U128, keccak256},
};
use weiroll::{
    Planner,
    cmds::{CommandFlags, Literal, ReturnValue, Value},
};

use crate::{
    core::ir,
    types::{MakinaInstruction, RootfileInputSlot},
};

pub fn codegen(inst: &ir::Instruction) -> miette::Result<MakinaInstruction> {
    let action = &inst.action;

    let mut planner = Planner::default();
    let mut checked_slots = Vec::new();
    let mut return_values = Vec::new();

    for call in action.calls.iter() {
        let args = transpile_parameters(
            &call.args,
            &action.slots,
            &return_values,
            &mut checked_slots,
        );

        let (return_types, command_flags) = resolve_call_metadata(call);

        return_values.push(
            planner
                .call(
                    call.target,
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

    Ok(MakinaInstruction {
        position_id: inst.position_id,
        description: inst.description.clone(),
        tags: inst.tags.clone(),
        is_debt: inst.is_debt,
        group_id: inst.group_id,
        instruction_type: inst.instruction_type,
        affected_tokens: inst.affected_tokens.clone(),
        position_tokens: inst.position_tokens.clone(),
        commands: commands.iter().map(|c| FixedBytes::from_slice(c)).collect(),
        state,
        bitmap,
        inputs_slots,
    })
}

fn transpile_parameters(
    parameters: &[ir::Operand],
    slots: &[ir::Slot],
    return_values: &[ReturnValue],
    checked_slots: &mut Vec<Bytes>,
) -> Vec<Value> {
    let mut result = Vec::new();

    for parameter in parameters.iter() {
        let value = transpile_parameter(parameter, slots, return_values, checked_slots);

        result.push(value);
    }

    result
}

fn transpile_parameter(
    parameter: &ir::Operand,
    slots: &[ir::Slot],
    return_values: &[ReturnValue],
    checked_slots: &mut Vec<Bytes>,
) -> Value {
    match parameter {
        ir::Operand::Literal(literal) => {
            let (encoded, is_dynamic) = transpile_literal(literal);

            checked_slots.push(encoded.clone());

            Value::Literal(Literal::new(encoded, is_dynamic))
        }
        ir::Operand::Slot(index) => {
            let input = &slots[*index];

            let name_hash: Bytes = keccak256(input.name.as_bytes()).into();

            Value::Literal(Literal::new(name_hash, is_type_dynamic(&input.ty)))
        }
        ir::Operand::Return(index) => Value::Return(return_values[*index].clone()),
        ir::Operand::Array(params) => Value::Array(transpile_parameters(
            params,
            slots,
            return_values,
            checked_slots,
        )),
        ir::Operand::Tuple(params) => Value::Tuple(transpile_parameters(
            params,
            slots,
            return_values,
            checked_slots,
        )),
        ir::Operand::FixedArray(params) => Value::FixedArray(transpile_parameters(
            params,
            slots,
            return_values,
            checked_slots,
        )),
    }
}

/// Encodes a literal operand and returns it with its weiroll dynamic flag.
///
/// Quirk kept for root parity: input literals use `abi_encode_params`, while blueprint
/// scalars use `abi_encode` with the length prefix dropped for bytes/string. The two agree
/// for every static value.
fn transpile_literal(literal: &ir::Literal) -> (Bytes, bool) {
    match literal {
        ir::Literal::Input(value) => {
            let encoded: Bytes = match value {
                DynSolValue::String(_) => value.abi_encode_params().into_iter().skip(32).collect(),
                _ => value.abi_encode_params().into(),
            };

            (encoded, value.is_dynamic())
        }
        ir::Literal::Scalar(value) => {
            let mut encoded = value.abi_encode();

            if matches!(value, DynSolValue::String(_) | DynSolValue::Bytes(_)) {
                encoded = encoded.iter().skip(32).cloned().collect();
            }

            (encoded.into(), value.is_dynamic())
        }
    }
}

fn transpile_reserved_slots(
    action: &ir::Action,
    return_values: &[ReturnValue],
    checked_slots: &mut Vec<Bytes>,
) -> Vec<Value> {
    let mut reserved_slots: Vec<_> = action
        .outputs
        .iter()
        .map(|rs| match rs {
            ir::Output::Return(index) => Value::Return(return_values[*index].clone()),
            ir::Output::Literal(value) => {
                let encoded: Bytes = value.abi_encode_params().into();
                checked_slots.push(encoded.clone());
                Value::Literal(Literal::new(encoded, false))
            }
        })
        .collect();

    // Quirk kept for root parity: these placeholders are dynamic only for bytes, string and
    // arrays, while the same slot used as a call argument goes through `is_type_dynamic`
    // (which also treats tuples with dynamic members as dynamic).
    reserved_slots.extend(action.slots.iter().map(|is| {
        let is_dynamic = matches!(
            is.ty,
            DynSolType::Bytes | DynSolType::String | DynSolType::Array(_)
        );
        Value::Literal(Literal::new(keccak256(&is.name).into(), is_dynamic))
    }));

    reserved_slots
}

fn transpile_input_slots(action: &ir::Action) -> Vec<RootfileInputSlot> {
    action
        .slots
        .iter()
        .enumerate()
        .map(|(i, v)| RootfileInputSlot {
            name: v.name.clone(),
            description: v.description.clone(),
            r#type: v.ty.clone().into(),
            index: i + action.outputs.len(),
            input_type: v.input_type.clone(),
            meta_type: v.meta_type.clone(),
        })
        .collect()
}

/// Quirk kept for root parity: a state slot is checked when its bytes equal any checked
/// literal, so a slot whose value happens to match a literal (e.g. a placeholder) is
/// checked too.
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

fn resolve_call_metadata(call: &ir::Call) -> (DynSolType, CommandFlags) {
    // Quirk kept for root parity: a call with no declared return is planned as returning
    // `bool`.
    let mut return_type = call
        .returns
        .clone()
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
