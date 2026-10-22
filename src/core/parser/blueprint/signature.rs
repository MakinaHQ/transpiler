//! Call signatures: the selector, and the declared parameters checked against it.

use alloy::{
    dyn_abi::DynSolType,
    primitives::{Selector, keccak256},
};
use saphyr::MarkedYaml;

use crate::core::parser::{
    blueprint::{BlueprintParameter, BlueprintParser},
    common::Parser,
    sol_types::{is_one_word, parse_signature},
};

impl BlueprintParser {
    /// The selector of `node.selector`, which must be a canonical Solidity signature, and
    /// the signature's parameter types.
    pub(super) fn selector<'a>(
        &self,
        node: &'a MarkedYaml<'a>,
    ) -> miette::Result<(Selector, Vec<DynSolType>)> {
        let field = self.get_str(node, "selector")?;
        let (canonical, types) =
            parse_signature(*field).map_err(|err| self.error(field.span, err))?;
        Ok((
            Selector::from(keccak256(canonical).first_chunk().unwrap()),
            types,
        ))
    }

    /// Every declared parameter must fit the signature (see [`fits`]), and there must be
    /// exactly as many as the signature takes.
    pub(super) fn check_parameters<'a>(
        &self,
        field: &'a MarkedYaml<'a>,
        parameters: &[BlueprintParameter],
        expected: &[DynSolType],
    ) -> miette::Result<()> {
        let fields = field.data.as_sequence().expect("parameters are a sequence");
        if fields.len() != expected.len() {
            Err(self.error(
                field,
                format!(
                    "the signature takes {} parameters, {} given",
                    expected.len(),
                    fields.len()
                ),
            ))?;
        }
        for ((elem, parameter), expected) in fields.iter().zip(parameters).zip(expected) {
            let declared = self.get_type(elem, "type")?;
            if !fits(parameter, &declared, expected) {
                Err(self.error(
                    declared.span,
                    format!(
                        "declared {} does not fit the signature's {expected}",
                        declared.inner
                    ),
                ))?;
            }
        }
        Ok(())
    }
}

/// Whether a parameter declared as `declared` encodes as the signature's `expected`.
/// Equal types fit. Otherwise only these encoding-identical idioms do:
/// - a one-word value into `bytes32`, an untyped word (key-value store, helpers);
/// - an integer into an integer of the same signedness and another width (the callee's
///   ABI decoder rejects a value that does not fit);
/// - a multi-value return captured raw (a tuple-typed return) into `bytes`;
/// - a runtime `bytes` already holding an encoded struct, into that dynamic struct.
///
/// Tuples and arrays fit element by element.
fn fits(parameter: &BlueprintParameter, declared: &DynSolType, expected: &DynSolType) -> bool {
    use BlueprintParameter as P;
    use DynSolType as T;

    if declared == expected {
        return true;
    }
    match (declared, expected) {
        (d, T::FixedBytes(32)) => is_one_word(d),
        (T::Uint(_), T::Uint(_)) | (T::Int(_), T::Int(_)) => true,
        (T::Tuple(_), T::Bytes) => matches!(parameter, P::Return(_)),
        (T::Bytes, T::Tuple(_)) => {
            expected.is_dynamic() && matches!(parameter, P::Return(_) | P::InputSlot(_))
        }
        (T::Tuple(ds), T::Tuple(es)) => match parameter {
            P::Tuple(ps) if ds.len() == es.len() => {
                ps.iter().zip(ds).zip(es).all(|((p, d), e)| fits(p, d, e))
            }
            _ => false,
        },
        (T::Array(d), T::Array(e)) => match parameter {
            P::DynamicArray(ps) => ps.iter().all(|p| fits(p, d, e)),
            _ => false,
        },
        (T::FixedArray(d, n), T::FixedArray(e, m)) if n == m => match parameter {
            P::FixedArray(ps, _) => ps.iter().all(|p| fits(p, d, e)),
            _ => false,
        },
        (T::CustomStruct { tuple, .. }, _) => fits(parameter, &T::Tuple(tuple.clone()), expected),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::BlueprintParser;

    fn parse(calls: &str) -> miette::Result<crate::core::parser::blueprint::Blueprint> {
        BlueprintParser::from_source(format!(
            "protocol: test\ninputs:\n  to:\n    type: address\nactions:\n  use:\n    calls:\n{calls}"
        ))
        .parse()
    }

    fn call(selector: &str, params: &str) -> String {
        format!(
            "      - target: \"0x0000000000000000000000000000000000000001\"\n        selector: \"{selector}\"\n        parameters:{params}\n"
        )
    }

    fn param(ty: &str, value: &str) -> String {
        format!("\n          - type: \"{ty}\"\n            value: {value}")
    }

    fn err(calls: &str) -> String {
        format!("{:?}", parse(calls).unwrap_err())
    }

    #[test]
    fn selectors_must_be_canonical_solidity() {
        let one = param("uint256", "\"1\"");
        let selector =
            |sig: &str| parse(&call(sig, &one)).map(|bp| bp.actions["use"].calls[0].selector);
        assert_eq!(
            selector("f(uint256)").unwrap(),
            selector("f( uint256 )").unwrap()
        );
        assert!(err(&call("f(uint)", " []")).contains("write \"f(uint256)\""));
        assert!(err(&call("f(uint7)", " []")).contains("expected a function signature"));
    }

    #[test]
    fn parameters_must_fit_the_signature() {
        let to = param("address", "${inputs.to}");
        let one = param("uint256", "\"1\"");

        parse(&call("transfer(address,uint256)", &format!("{to}{one}"))).unwrap();
        // arity
        assert!(
            err(&call("transfer(address,uint256)", &to))
                .contains("the signature takes 2 parameters, 1 given")
        );
        // swapped arguments: both one-word, still rejected
        assert!(
            err(&call("transfer(address,uint256)", &format!("{one}{to}")))
                .contains("declared uint256 does not fit the signature's address")
        );
        // nested element types
        let bad = param(
            "uint256[]",
            "\n              - type: string\n                value: oops",
        );
        assert!(
            err(&call("f(uint256[])", &bad)).contains("expected element type uint256, got string")
        );
    }

    #[test]
    fn encoding_identical_idioms_fit() {
        // a one-word value into bytes32; an integer into a narrower integer
        parse(&call(
            "set(bytes32,bytes32)",
            &format!(
                "{}{}",
                param("uint256", "\"1\""),
                param("address", "${inputs.to}")
            ),
        ))
        .unwrap();
        parse(&call("supply(uint16)", &param("uint256", "\"0\""))).unwrap();
        // but not across signedness, or bool into an integer
        assert!(err(&call("f(int256)", &param("uint256", "\"1\""))).contains("does not fit"));
        assert!(err(&call("f(uint256)", &param("bool", "\"true\""))).contains("does not fit"));
        // a literal tuple is not a raw return: it cannot stand in for bytes
        let tuple = param(
            "(uint256,uint256)",
            "\n              - type: uint256\n                value: \"1\"\n              - type: uint256\n                value: \"2\"",
        );
        assert!(err(&call("f(bytes)", &tuple)).contains("does not fit"));
    }
}
