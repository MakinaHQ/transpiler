use std::{
    collections::{BTreeMap, HashSet},
    path::PathBuf,
    str::FromStr,
};

use alloy::{
    dyn_abi::DynSolType,
    primitives::{Address, Selector, keccak256},
};
use indexmap::IndexMap;
use miette::{NamedSource, miette};
use regex::Regex;
use saphyr::{LoadableYamlNode, MarkedYaml};

use crate::{
    core::parser::{
        blueprint::{
            Blueprint, BlueprintAction, BlueprintCall, BlueprintInput, BlueprintInputSlot,
            BlueprintInputType, BlueprintParameter, BlueprintReservedSlot, BlueprintReturn,
            BlueprintTarget, BlueprintValue, types::BlueprintTemplate,
        },
        common::{Marked, Parser},
        helpers,
        sol_types::{YamlSolValue, canonical_signature, parse_sol_value_marked},
    },
    meta_sol_types::MetaDynSolType,
};

pub struct BlueprintParser {
    file: PathBuf,
    source: String,
    template_regex: Regex,
}

impl BlueprintParser {
    pub fn new<P>(file: P) -> miette::Result<Self>
    where
        P: Into<PathBuf>,
    {
        let template_regex = Regex::new(r"\$\{(\w+)\.(\w+)(?:\.(\w*))?\}").expect("is valid regex");

        let file = file.into();
        let content = std::fs::read_to_string(&file)
            .map_err(|err| miette!("could not open blueprint: {}", err))?;

        let source = helpers::replace_builtins(&content)
            .map_err(|err| miette!("could not replace builtins: {err}"))?;

        Ok(Self {
            file,
            source,
            template_regex,
        })
    }

    pub fn parse(&self) -> miette::Result<Blueprint> {
        let parsed = MarkedYaml::load_from_str(&self.source)
            .map_err(|err| miette!("could not parse blueprint file: {err}"))?;

        if parsed.len() != 1 {
            return Err(miette!(
                "blueprint file can contain exactly one blueprint: {}",
                self.file.to_string_lossy()
            ));
        }
        let root = parsed.first().expect("parsed.len() == 1");

        let protocol = self.protocol(root)?;
        let mut templates = self.inputs(root)?;
        templates.append(&mut self.constants(root)?);
        let actions = self.actions(root, &templates)?;

        Ok(Blueprint {
            path: self.file.clone(),
            protocol: protocol.to_string(),
            constants: templates
                .iter()
                .filter_map(|(k, v)| v.as_constant().map(|cons| (k.clone(), cons.clone())))
                .collect(),
            inputs: templates
                .iter()
                .filter_map(|(k, v)| v.as_input().map(|cons| (k.clone(), cons.clone())))
                .collect(),
            actions: actions
                .into_iter()
                .map(|(k, v)| (k.to_string(), v))
                .collect(),
        })
    }

    fn protocol<'a>(&self, root: &'a MarkedYaml<'a>) -> miette::Result<&'a str> {
        self.get_str(root, "protocol").map(|m| *m)
    }

    fn constants<'a>(
        &self,
        root: &'a MarkedYaml<'a>,
    ) -> miette::Result<BTreeMap<String, BlueprintTemplate>> {
        let Some(constants) = root.data.as_mapping_get("constants") else {
            return Ok(BTreeMap::new());
        };

        let mapping = constants
            .data
            .as_mapping()
            .ok_or(self.error(constants.span, "constants must be mapping"))?;

        let mut map = BTreeMap::new();
        for (key, value) in mapping {
            let name = key
                .data
                .as_str()
                .ok_or(self.error(key.span, "Constant has invalid name"))?;

            let key = format!("${{constants.{name}}}");
            let value = self.sol_value(value)?;
            map.insert(key, BlueprintTemplate::Constant(value));
        }

        Ok(map)
    }

    fn inputs<'a>(
        &self,
        root: &'a MarkedYaml<'a>,
    ) -> miette::Result<BTreeMap<String, BlueprintTemplate>> {
        let Some(inputs) = root.data.as_mapping_get("inputs") else {
            return Ok(BTreeMap::new());
        };

        let mapping = inputs
            .data
            .as_mapping()
            .ok_or(self.error(inputs.span, "inputs must be mapping"))?;

        let mut map = BTreeMap::new();
        for (key, value) in mapping {
            let name = key
                .data
                .as_str()
                .ok_or(self.error(key.span, "input has invalid name"))?;

            let key = format!("${{inputs.{name}}}");
            let value = self.blueprint_input(name, value)?;
            map.insert(key, BlueprintTemplate::Input(value));
        }

        Ok(map)
    }

    fn actions<'a>(
        &self,
        root: &'a MarkedYaml<'a>,
        templates: &BTreeMap<String, BlueprintTemplate>,
    ) -> miette::Result<BTreeMap<&'a str, BlueprintAction>> {
        let actions = root
            .data
            .as_mapping_get("actions")
            .ok_or(self.error(root.span, "actions are required"))?;

        let mapping = actions
            .data
            .as_mapping()
            .ok_or(self.error(actions.span, "must be a mapping"))?;

        let mut map = BTreeMap::new();
        for (key, value) in mapping {
            let name = key
                .data
                .as_str()
                .ok_or(self.error(key.span, "invalid name"))?;

            let action = self.blueprint_action(value, templates)?;

            map.insert(name, action);
        }

        Ok(map)
    }

    fn blueprint_action<'a>(
        &self,
        field: &'a MarkedYaml<'a>,
        templates: &BTreeMap<String, BlueprintTemplate>,
    ) -> miette::Result<BlueprintAction> {
        let input_slots = self.blueprint_input_slots(field, templates)?;
        let mut templates = templates.clone();
        templates.append(
            &mut input_slots
                .iter()
                .map(|(k, v)| {
                    (
                        format!("${{input_slots.{k}}}"),
                        BlueprintTemplate::InputSlot(v.clone()),
                    )
                })
                .collect(),
        );
        let (calls, returns_mapping) = self.blueprint_calls(field, &mut templates)?;
        let reserved_slots = self.blueprint_reserved_slots(field, &templates)?;

        let used_slots: HashSet<_> = calls
            .iter()
            .flat_map(|c| &c.parameters)
            .flat_map(|p| p.input_slots())
            .collect();

        Ok(BlueprintAction {
            calls,
            returns_mapping: returns_mapping
                .into_iter()
                .map(|(k, v)| (k.to_string(), v))
                .collect(),
            reserved_slots,
            input_slots: input_slots
                .into_iter()
                .filter(|(name, _)| used_slots.contains(name))
                .collect(),
        })
    }

    fn blueprint_input_slots<'a>(
        &self,
        field: &'a MarkedYaml<'a>,
        templates: &BTreeMap<String, BlueprintTemplate>,
    ) -> miette::Result<IndexMap<String, BlueprintInputSlot>> {
        let Some(input_slots) = field.data.as_mapping_get("input_slots") else {
            return Ok(IndexMap::new());
        };

        let mapping = input_slots
            .data
            .as_mapping()
            .ok_or(self.error(input_slots.span, "must be a mapping"))?;

        let mut map = IndexMap::new();
        for (key, value) in mapping {
            let slot_name = key
                .data
                .as_str()
                .ok_or(self.error(key.span, "invalid name"))?;

            let input = self.blueprint_input(slot_name, value)?;
            let input_type = self.blueprint_input_type(value, &input.r#type, templates)?;

            match input.r#type {
                DynSolType::CustomStruct {
                    name,
                    prop_names,
                    tuple,
                } => {
                    let meta_type = MetaDynSolType::from_str(&name)
                        .or(Err(self.error(key.span, "invalid meta type")))?;

                    if prop_names.len() != tuple.len() {
                        return Err(self.error(value.span, "properties do not match tuple"))?;
                    }

                    for (prop_name, prop_type) in prop_names.into_iter().zip(tuple) {
                        map.insert(
                            format!("{slot_name}.{prop_name}"),
                            BlueprintInputSlot {
                                name: format!("{slot_name}.{prop_name}"),
                                r#type: prop_type,
                                description: input.description.to_owned(),
                                input_type: input_type.clone(),
                                meta_type: Some(meta_type.to_owned()),
                                meta_type_field: Some(prop_name.clone()),
                                meta_type_name: Some(slot_name.to_owned()),
                            },
                        );
                    }
                }
                _ => {
                    map.insert(
                        slot_name.to_string(),
                        BlueprintInputSlot {
                            name: slot_name.to_string(),
                            r#type: input.r#type,
                            description: input.description,
                            input_type,
                            meta_type: None,
                            meta_type_field: None,
                            meta_type_name: None,
                        },
                    );
                }
            }
        }

        Ok(map)
    }

    fn blueprint_input_type<'a>(
        &self,
        field: &'a MarkedYaml<'a>,
        slot_type: &DynSolType,
        templates: &BTreeMap<String, BlueprintTemplate>,
    ) -> miette::Result<Option<BlueprintInputType>> {
        let Some(input_type) = field.data.as_mapping_get("input_type") else {
            return Ok(None);
        };

        match slot_type {
            DynSolType::Uint(_) => {}
            _ => {
                return Err(self
                    .error(
                        input_type.span,
                        "input_type requires an unsigned integer slot",
                    )
                    .into());
            }
        }
        let variants = input_type
            .data
            .as_mapping()
            .ok_or(self.error(input_type.span, "input_type must be a mapping"))?;
        if variants.len() != 1 {
            return Err(self
                .error(
                    input_type.span,
                    "input_type must contain exactly one 'amount' variant",
                )
                .into());
        }

        let (variant, config) = variants.iter().next().expect("checked one variant");
        let variant_name = variant
            .data
            .as_str()
            .ok_or(self.error(variant.span, "input_type variant must be a string"))?;
        let config_map = config
            .data
            .as_mapping()
            .ok_or(self.error(config.span, "input_type variant must be a mapping"))?;

        let parsed = match variant_name {
            "amount" => {
                if config_map.len() != 1 || config.data.as_mapping_get("token").is_none() {
                    return Err(self
                        .error(config.span, "amount input_type requires only 'token'")
                        .into());
                }
                let token = config.data.as_mapping_get("token").expect("checked token");
                BlueprintInputType::Amount {
                    token: self.input_type_token(token, templates)?,
                }
            }
            _ => {
                return Err(self
                    .error(
                        variant.span,
                        "unknown input_type variant; expected 'amount'",
                    )
                    .into());
            }
        };

        Ok(Some(parsed))
    }

    fn input_type_token<'a>(
        &self,
        field: &'a MarkedYaml<'a>,
        templates: &BTreeMap<String, BlueprintTemplate>,
    ) -> miette::Result<BlueprintTarget> {
        if self.is_template(field) {
            let template = self.parse_template_str(field)?;
            if *template.source != "inputs" {
                return Err(self
                    .error(template.source, "amount token must reference an input")
                    .into());
            }
            let key = field.data.as_str().expect("already parsed as template");
            return match templates.get(key) {
                Some(BlueprintTemplate::Input(input)) if input.r#type == DynSolType::Address => {
                    Ok(BlueprintTarget::Input(input.name.clone()))
                }
                Some(_) => Err(self
                    .error(field.span, "amount token input must have type address")
                    .into()),
                None => Err(self.error(template.name.span, "unknown input").into()),
            };
        }

        let token = field
            .data
            .as_str()
            .and_then(|value| Address::from_str(value).ok())
            .ok_or(self.error(
                field.span,
                "amount token must be an address or input reference",
            ))?;
        Ok(BlueprintTarget::Address(token))
    }

    fn blueprint_calls<'a>(
        &self,
        field: &'a MarkedYaml<'a>,
        templates: &mut BTreeMap<String, BlueprintTemplate>,
    ) -> miette::Result<(Vec<BlueprintCall>, BTreeMap<&'a str, usize>)> {
        let calls = field
            .data
            .as_mapping_get("calls")
            .ok_or(self.error(field.span, "'calls' is missing"))?;

        let sequence = calls
            .data
            .as_sequence()
            .ok_or(self.error(calls.span, "must be a sequence"))?;

        let mut vec = Vec::new();
        let mut returns_mapping = BTreeMap::new();
        for (idx, elem) in sequence.iter().enumerate() {
            let description = self
                .try_get_str(elem, "description")?
                .map(|d| d.inner.to_string())
                .unwrap_or_default();

            let target = self.blueprint_target(elem, templates)?;

            let selector = self.selector(elem)?.0;

            let parameters_field = elem
                .data
                .as_mapping_get("parameters")
                .ok_or(self.error(elem.span, "'parameters' is missing"))?;
            let parameters = self.blueprint_parameters(parameters_field, templates)?;

            let r#return = if let Some(return_field) = elem.data.as_mapping_get("return") {
                let name = self.get_str(return_field, "name")?;
                let key = format!("${{returns.{}}}", name.inner);

                if templates.contains_key(&key) {
                    return Err(self.error(name.span, "duplicate return name"))?;
                }

                let r#type = self.get_type(return_field, "type")?;

                let blueprint_return = BlueprintReturn {
                    name: name.to_string(),
                    r#type: r#type.inner,
                };
                returns_mapping.insert(*name, idx);
                templates.insert(key, BlueprintTemplate::Return(blueprint_return.clone()));

                Some(blueprint_return)
            } else {
                None
            };

            vec.push(BlueprintCall {
                description,
                target,
                selector,
                parameters,
                r#return,
            });
        }

        Ok((vec, returns_mapping))
    }

    /// The selector of `node.selector`, hashed from the canonical signature (so `f(uint)`
    /// and `f(uint256)` agree), and the signature's parameter types.
    pub(super) fn selector<'a>(
        &self,
        node: &'a MarkedYaml<'a>,
    ) -> miette::Result<(Selector, Vec<DynSolType>)> {
        let field = self.get_str(node, "selector")?;
        let (canonical, types) = canonical_signature(*field).ok_or_else(|| {
            self.error(
                field.span,
                "expected a function signature, e.g. \"transfer(address,uint256)\"",
            )
        })?;
        let selector = Selector::from(keccak256(canonical).first_chunk().unwrap());
        Ok((selector, types))
    }

    fn blueprint_parameters<'a>(
        &self,
        field: &'a MarkedYaml<'a>,
        templates: &BTreeMap<String, BlueprintTemplate>,
    ) -> miette::Result<Vec<BlueprintParameter>> {
        let sequence = field
            .data
            .as_sequence()
            .ok_or(self.error(field.span, "must be sequence"))?;

        let mut vec = Vec::new();
        for elem in sequence.iter() {
            let parsed = self.blueprint_parameter(elem, templates)?;
            vec.push(parsed);
        }

        Ok(vec)
    }

    pub(super) fn blueprint_parameter<'a>(
        &self,
        field: &'a MarkedYaml<'a>,
        templates: &BTreeMap<String, BlueprintTemplate>,
    ) -> miette::Result<BlueprintParameter> {
        let value_field = field
            .data
            .as_mapping_get("value")
            .ok_or_else(|| self.error(field.span, "parameter has no value"))?;

        if let Some(node) = value_field.data.as_mapping_get("abi_encode_call") {
            return self.abi_encode_call(field, node, templates);
        }

        // for simple types we can parse the template variable directly
        if !value_field.is_sequence() {
            let parsed = match self.parse_template(field, templates)? {
                BlueprintTemplate::Input(input) => BlueprintParameter::Input(input.name),
                BlueprintTemplate::InputSlot(slot) => BlueprintParameter::InputSlot(slot.name),
                BlueprintTemplate::Constant(cons) => BlueprintParameter::Scalar(BlueprintValue {
                    value: cons.value,
                    r#type: cons.r#type,
                    description: None,
                }),
                BlueprintTemplate::Return(ret) => BlueprintParameter::Return(ret.name),
                BlueprintTemplate::Raw(raw) => raw.value.into(),
            };

            return Ok(parsed);
        }

        let sequence = value_field
            .data
            .as_sequence()
            .expect("already checked the type");

        let r#type = self.get_type(field, "type")?;
        match r#type.inner {
            DynSolType::Tuple(inner) => {
                if sequence.len() != inner.len() {
                    return Err(self
                        .error(value_field, "does not match tuple length")
                        .with_second_location(r#type.span, "defined here"))?;
                }

                let params = sequence
                    .iter()
                    .zip(&inner)
                    .map(|(elem, ty)| self.element_parameter(elem, ty, templates))
                    .collect::<miette::Result<Vec<_>>>()?;

                Ok(BlueprintParameter::Tuple(params))
            }
            DynSolType::FixedArray(inner, length) => {
                if sequence.len() != length {
                    return Err(self
                        .error(value_field, "does not match fixed array length")
                        .with_second_location(r#type.span, "defined here"))?;
                }

                let params = sequence
                    .iter()
                    .map(|elem| self.element_parameter(elem, &inner, templates))
                    .collect::<miette::Result<Vec<_>>>()?;

                Ok(BlueprintParameter::FixedArray(params, length))
            }
            DynSolType::Array(inner) => {
                let params = sequence
                    .iter()
                    .map(|elem| self.element_parameter(elem, &inner, templates))
                    .collect::<miette::Result<Vec<_>>>()?;

                Ok(BlueprintParameter::DynamicArray(params))
            }
            _ => Err(self.error(
                value_field,
                "only Array | FixedArray | Tuple are allowed here",
            ))?,
        }
    }

    /// An element of an array or tuple parameter, whose declared type must be the one its
    /// parent type implies (an `uint256[]` cannot hold a `string`).
    fn element_parameter<'a>(
        &self,
        elem: &'a MarkedYaml<'a>,
        expected: &DynSolType,
        templates: &BTreeMap<String, BlueprintTemplate>,
    ) -> miette::Result<BlueprintParameter> {
        let declared = self.get_type(elem, "type")?;
        if &declared.inner != expected {
            Err(self.error(
                declared.span,
                format!("expected element type {expected}, got {}", declared.inner),
            ))?;
        }
        self.blueprint_parameter(elem, templates)
    }

    /// Parses a field that can hold a raw value or a template variable
    /// Expects `field` to hold `type` and `value` as direct children
    fn parse_template<'a>(
        &self,
        field: &'a MarkedYaml<'a>,
        templates: &BTreeMap<String, BlueprintTemplate>,
    ) -> miette::Result<BlueprintTemplate> {
        let (value_field, r#type) = if let Some(value_field) = field.data.as_mapping_get("value") {
            let r#type = self.get_type(field, "type")?;
            (value_field, r#type)
        } else {
            let value_field = field
                .data
                .as_mapping_get("target")
                .ok_or_else(|| self.error(field, "expected 'value' or 'target'"))?;
            (value_field, Marked::new(DynSolType::Address, field.span))
        };

        debug_assert!(!field.is_sequence());

        // if value is not a template string
        // try parsing as raw value and return the result
        if !self.is_template(value_field) {
            let value = parse_sol_value_marked(&r#type, value_field, self.named_source())?
                .ok_or_else(|| {
                    self.error(value_field, "could not be parsed")
                        .with_second_location(r#type.span, "into specified type")
                })?;

            return Ok(BlueprintTemplate::Raw(YamlSolValue {
                r#type: r#type.inner,
                value,
            }));
        }

        let template = self.parse_template_str(value_field)?;
        if !["returns", "input_slots", "constants", "inputs"].contains(&*template.source) {
            return Err(self.error(template.source, "unknown source"))?;
        }

        let key = value_field
            .data
            .as_str()
            .expect("already parsed as template");

        let value = templates
            .get(key)
            .ok_or_else(|| self.error(template.name.span, "unknown name"))?;

        if value.as_type() != &r#type.inner {
            return Err(self
                .error(value_field, "has unexpected type")
                .with_second_location(r#type.span, "expected type"))?;
        }

        Ok(value.clone())
    }

    fn blueprint_target<'a>(
        &self,
        field: &'a MarkedYaml<'a>,
        templates: &BTreeMap<String, BlueprintTemplate>,
    ) -> miette::Result<BlueprintTarget> {
        let parsed = self.parse_template(field, templates)?;

        if parsed.as_type() != &DynSolType::Address {
            return Err(self.error(field, "target must be address"))?;
        }

        let target = match parsed {
            BlueprintTemplate::Input(input) => BlueprintTarget::Input(input.name),
            BlueprintTemplate::Constant(cons) => {
                BlueprintTarget::Address(cons.value.as_address().expect("already checked the type"))
            }
            BlueprintTemplate::Raw(raw) => {
                BlueprintTarget::Address(raw.value.as_address().expect("already checked the type"))
            }
            _ => return Err(self.error(field, "target must be constant, input, or raw value"))?,
        };

        Ok(target)
    }

    fn blueprint_reserved_slots<'a>(
        &self,
        field: &'a MarkedYaml<'a>,
        templates: &BTreeMap<String, BlueprintTemplate>,
    ) -> miette::Result<Vec<BlueprintReservedSlot>> {
        let Some(reserved_slots) = field.data.as_mapping_get("reserved_slots") else {
            return Ok(Vec::new());
        };

        let sequence = reserved_slots
            .data
            .as_sequence()
            .ok_or(self.error(reserved_slots.span, "must be a sequence"))?;

        let mut vec = Vec::new();
        for elem in sequence {
            let parsed = self.parse_template(elem, templates)?;

            let description = self
                .try_get_str(elem, "description")?
                .map(|d| d.inner.to_string());

            let slot = match parsed {
                BlueprintTemplate::Return(ret) => BlueprintReservedSlot::Return(ret.name),
                BlueprintTemplate::Constant(cons) => {
                    BlueprintReservedSlot::Scalar(BlueprintValue {
                        value: cons.value,
                        r#type: cons.r#type,
                        description,
                    })
                }
                BlueprintTemplate::Raw(raw) => {
                    if raw.r#type.is_dynamic() {
                        return Err(self.error(elem.span, "must be scalar type"))?;
                    }
                    BlueprintReservedSlot::Scalar(BlueprintValue {
                        value: raw.value,
                        r#type: raw.r#type,
                        description,
                    })
                }
                _ => {
                    return Err(self.error(elem.span, "must be 'returns' or 'constants'"))?;
                }
            };

            vec.push(slot);
        }

        Ok(vec)
    }

    fn blueprint_input<'a>(
        &self,
        name: &str,
        field: &'a MarkedYaml<'a>,
    ) -> miette::Result<BlueprintInput> {
        let r#type = self.get_type(field, "type")?;

        // Reject complex dynamic types (arrays, tuples) but allow:
        // - Bytes and String (dynamic but valid as inputs)
        // - CustomStructs (used for meta types like SwapData)
        let is_allowed_dynamic = matches!(r#type.inner, DynSolType::Bytes | DynSolType::String);
        if r#type.is_dynamic() && !r#type.has_custom_struct() && !is_allowed_dynamic {
            return Err(self.error(r#type.span, "must be scalar type"))?;
        }

        let description = self
            .try_get_str(field, "description")?
            .map(|d| d.inner.to_string());

        Ok(BlueprintInput {
            name: name.to_string(),
            r#type: r#type.inner,
            description,
        })
    }
}

impl Parser for BlueprintParser {
    fn named_source(&self) -> NamedSource<String> {
        NamedSource::new(self.file.to_string_lossy(), self.source.clone())
    }

    fn template_regex(&self) -> &Regex {
        &self.template_regex
    }
}

#[cfg(test)]
mod tests {
    use regex::Regex;

    use crate::core::parser::blueprint::BlueprintInputType;

    use super::BlueprintParser;

    fn parse_input_type(slot_type: &str, input_type: &str) -> miette::Result<super::Blueprint> {
        let source = format!(
            r#"protocol: test
inputs:
  token:
    type: address
actions:
  use:
    calls:
      - target: "0x0000000000000000000000000000000000000001"
        selector: "use(uint256)"
        parameters:
          - type: "{slot_type}"
            value: ${{input_slots.value}}
    input_slots:
      value:
        type: "{slot_type}"
        input_type:
{input_type}
"#
        );
        BlueprintParser {
            file: "input-type-test.yaml".into(),
            source,
            template_regex: Regex::new(r"\$\{(\w+)\.(\w+)(?:\.(\w*))?\}").unwrap(),
        }
        .parse()
    }

    #[test]
    fn test_smoke() {
        let parser = BlueprintParser::new("test_data/blueprints/account.yaml").unwrap();

        parser.parse().unwrap();
    }

    #[test]
    fn parses_amount_token_as_input_reference_or_address() {
        let referenced = parse_input_type(
            "uint256",
            "          amount:\n            token: ${inputs.token}",
        )
        .unwrap();
        assert_eq!(
            referenced.actions["use"].input_slots["value"].input_type,
            Some(BlueprintInputType::Amount {
                token: super::BlueprintTarget::Input("token".into()),
            })
        );

        let addressed = parse_input_type(
            "uint256",
            "          amount:\n            token: \"0x0000000000000000000000000000000000000002\"",
        )
        .unwrap();
        assert_eq!(
            addressed.actions["use"].input_slots["value"].input_type,
            Some(BlueprintInputType::Amount {
                token: super::BlueprintTarget::Address(
                    "0x0000000000000000000000000000000000000002"
                        .parse()
                        .unwrap()
                ),
            })
        );
    }

    #[test]
    fn rejects_invalid_input_types_and_references() {
        let cases = [
            (
                "address",
                "          amount:\n            token: ${inputs.token}",
                "requires an unsigned integer slot",
            ),
            (
                "uint256",
                "          amount:\n            token: ${inputs.missing}",
                "unknown input",
            ),
            (
                "uint256",
                "          currency:\n            symbol: USD",
                "unknown input_type variant",
            ),
        ];

        for (slot_type, input_type, expected) in cases {
            let error = parse_input_type(slot_type, input_type).unwrap_err();
            assert!(
                format!("{error:?}").contains(expected),
                "expected {expected:?} in {error:?}"
            );
        }
    }

    fn parse_abi_encode_call(
        parameters: &str,
        math_helper: &str,
    ) -> miette::Result<super::Blueprint> {
        let source = format!(
            r#"protocol: test
inputs:
  to:
    type: address
actions:
  use:
    calls:
      - target: "0x0000000000000000000000000000000000000001"
        selector: "balance()"
        parameters: []
        return:
          name: "amount"
          type: "uint256"
      - target: "0x0000000000000000000000000000000000000001"
        selector: "exec(bytes)"
        parameters:
          - type: "bytes"
            value:
              abi_encode_call:
                selector: "transfer(address,uint256)"
{math_helper}
                parameters:
{parameters}
"#
        );
        BlueprintParser {
            file: "abi-encode-call-test.yaml".into(),
            source,
            template_regex: Regex::new(r"\$\{(\w+)\.(\w+)(?:\.(\w*))?\}").unwrap(),
        }
        .parse()
    }

    const HELPER: &str =
        "                math_helper: \"0x3D623B199E290358416415eA7e05B635E442e3c0\"";
    const TO: &str = "                  - type: address\n                    value: ${inputs.to}";

    #[test]
    fn abi_encode_call_accepts_runtime_parameters_with_a_helper() {
        let params = format!(
            "{TO}\n                  - type: uint256\n                    value: ${{returns.amount}}"
        );
        parse_abi_encode_call(&params, HELPER).unwrap();
    }

    #[test]
    fn abi_encode_call_requires_a_helper_for_runtime_parameters() {
        let params = format!(
            "{TO}\n                  - type: uint256\n                    value: ${{returns.amount}}"
        );
        let err = parse_abi_encode_call(&params, "").unwrap_err();
        assert!(format!("{err:?}").contains("math_helper is required"));
    }

    #[test]
    fn abi_encode_call_compile_time_needs_no_helper() {
        let params =
            format!("{TO}\n                  - type: uint256\n                    value: \"7\"");
        parse_abi_encode_call(&params, "").unwrap();
    }

    #[test]
    fn abi_encode_call_rejects_parameters_that_do_not_match_the_signature() {
        let params =
            format!("{TO}\n                  - type: uint128\n                    value: \"7\"");
        let err = parse_abi_encode_call(&params, "").unwrap_err();
        assert!(format!("{err:?}").contains("does not match the signature"));

        let err = parse_abi_encode_call(TO, "").unwrap_err();
        assert!(format!("{err:?}").contains("signature has 2 parameters, 1 given"));
    }

    fn parse_source(source: String) -> miette::Result<super::Blueprint> {
        BlueprintParser {
            file: "test.yaml".into(),
            source,
            template_regex: Regex::new(r"\$\{(\w+)\.(\w+)(?:\.(\w*))?\}").unwrap(),
        }
        .parse()
    }

    #[test]
    fn nested_elements_must_match_the_parent_type() {
        let source = r#"protocol: test
actions:
  use:
    calls:
      - target: "0x0000000000000000000000000000000000000001"
        selector: "exec(bytes)"
        parameters:
          - type: bytes
            value:
              abi_encode_call:
                selector: f(uint256[])
                parameters:
                  - type: uint256[]
                    value:
                      - type: string
                        value: oops
"#;
        let err = parse_source(source.to_string()).unwrap_err();
        assert!(
            format!("{err:?}").contains("expected element type uint256, got string"),
            "{err:?}"
        );

        // the same holds for ordinary call parameters and tuples
        let source = r#"protocol: test
actions:
  use:
    calls:
      - target: "0x0000000000000000000000000000000000000001"
        selector: "g((uint256,address))"
        parameters:
          - type: (uint256,address)
            value:
              - type: uint256
                value: "1"
              - type: uint256
                value: "2"
"#;
        let err = parse_source(source.to_string()).unwrap_err();
        assert!(
            format!("{err:?}").contains("expected element type address, got uint256"),
            "{err:?}"
        );
    }

    #[test]
    fn selectors_are_hashed_from_the_canonical_signature() {
        let call = |selector: &str| {
            let source = format!(
                "protocol: test\nactions:\n  use:\n    calls:\n      - target: \"0x0000000000000000000000000000000000000001\"\n        selector: \"{selector}\"\n        parameters: []\n"
            );
            parse_source(source).unwrap().actions["use"].calls[0].selector
        };
        assert_eq!(call("f(uint)"), call("f(uint256)"));
        assert_eq!(call("f( uint256 )"), call("f(uint256)"));
        assert!(parse_source("protocol: test\nactions:\n  use:\n    calls:\n      - target: \"0x0000000000000000000000000000000000000001\"\n        selector: \"f(uint\"\n        parameters: []\n".to_string()).is_err());
    }
}
