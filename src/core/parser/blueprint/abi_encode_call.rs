//! Parsing of `abi_encode_call` parameter values.
//!
//! ```yaml
//! - type: "bytes"
//!   value:
//!     abi_encode_call:
//!       selector: "transfer(address,uint256)"
//!       math_helper: "${constants.math_helper_address}" # only with runtime parameters
//!       parameters:
//!         - type: "address"
//!           value: "${inputs.recipient}"
//!         - type: "uint256"
//!           value: "${returns.balance}"
//! ```

use std::collections::BTreeMap;

use alloy::dyn_abi::DynSolType;
use saphyr::MarkedYaml;

use crate::core::parser::{
    blueprint::{
        AbiEncodeCall, BlueprintParameter, BlueprintParser, BlueprintTarget,
        types::BlueprintTemplate,
    },
    common::Parser,
    sol_types::{is_one_word, parse_sol_value_marked},
};

impl BlueprintParser {
    /// `field` is the parameter (`type` + `value`), `node` the `abi_encode_call` mapping.
    pub(super) fn abi_encode_call<'a>(
        &self,
        field: &'a MarkedYaml<'a>,
        node: &'a MarkedYaml<'a>,
        templates: &BTreeMap<String, BlueprintTemplate>,
    ) -> miette::Result<BlueprintParameter> {
        let r#type = self.get_type(field, "type")?;
        if r#type.inner != DynSolType::Bytes {
            Err(self.error(
                r#type.span,
                "abi_encode_call produces bytes: declare type \"bytes\"",
            ))?;
        }

        let signature_field = self.get_str(node, "selector")?;
        let (selector, parameter_types) = self.selector(node)?;

        let parameter_fields: Vec<&MarkedYaml> = match node.data.as_mapping_get("parameters") {
            Some(parameters) => parameters
                .data
                .as_sequence()
                .ok_or_else(|| self.error(parameters, "must be a sequence"))?
                .iter()
                .collect(),
            None => vec![],
        };
        if parameter_fields.len() != parameter_types.len() {
            Err(self.error(
                signature_field.span,
                format!(
                    "signature has {} parameters, {} given",
                    parameter_types.len(),
                    parameter_fields.len()
                ),
            ))?;
        }

        let mut parameters = Vec::with_capacity(parameter_fields.len());
        for (elem, expected) in parameter_fields.iter().zip(&parameter_types) {
            let declared = self.get_type(elem, "type")?;
            if &declared.inner != expected {
                Err(self
                    .error(declared.span, "does not match the signature")
                    .with_second_location(signature_field.span, "signature"))?;
            }
            parameters.push(self.blueprint_parameter(elem, templates)?);
        }

        let math_helper = node
            .data
            .as_mapping_get("math_helper")
            .map(|helper| self.address_target(helper, templates))
            .transpose()?;

        if parameters.iter().any(BlueprintParameter::is_runtime) {
            if math_helper.is_none() {
                Err(self.error(
                    node,
                    "math_helper is required when a parameter is a runtime value (returns, input_slots)",
                ))?;
            }
            for ((elem, parameter), ty) in parameter_fields
                .iter()
                .zip(&parameters)
                .zip(&parameter_types)
            {
                if !is_one_word(ty) {
                    Err(self.error(
                        *elem,
                        "with runtime values, every parameter must be a one-word static type (address, bool, uintN, intN, bytesN)",
                    ))?;
                }
                if parameter.is_runtime()
                    && !matches!(
                        parameter,
                        BlueprintParameter::Return(_) | BlueprintParameter::InputSlot(_)
                    )
                {
                    Err(self.error(*elem, "runtime values must be passed directly, not nested"))?;
                }
            }
        }

        Ok(BlueprintParameter::AbiEncodeCall(Box::new(AbiEncodeCall {
            selector,
            parameters,
            math_helper,
        })))
    }

    /// An address given as a raw value, a constant or an input — the same sources a call
    /// target accepts.
    fn address_target<'a>(
        &self,
        field: &'a MarkedYaml<'a>,
        templates: &BTreeMap<String, BlueprintTemplate>,
    ) -> miette::Result<BlueprintTarget> {
        if !self.is_template(field) {
            let address = parse_sol_value_marked(&DynSolType::Address, field, self.named_source())?
                .and_then(|value| value.as_address())
                .ok_or_else(|| self.error(field, "must be an address"))?;
            return Ok(BlueprintTarget::Address(address));
        }

        let key = field
            .data
            .as_str()
            .expect("already checked to be a template");
        let template = self.parse_template_str(field)?;
        match templates.get(key) {
            Some(BlueprintTemplate::Constant(cons)) => cons
                .value
                .as_address()
                .map(BlueprintTarget::Address)
                .ok_or_else(|| self.error(field, "must be an address").into()),
            Some(BlueprintTemplate::Input(input)) if input.r#type == DynSolType::Address => {
                Ok(BlueprintTarget::Input(input.name.clone()))
            }
            Some(_) => Err(self.error(field, "must be an address constant or input"))?,
            None => Err(self.error(template.name.span, "unknown name"))?,
        }
    }
}
