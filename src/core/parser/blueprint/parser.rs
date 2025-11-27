use std::{
    collections::{BTreeMap, HashSet},
    path::PathBuf,
    str::FromStr,
};

use alloy::{
    dyn_abi::DynSolType,
    primitives::{Selector, keccak256},
};
use indexmap::IndexMap;
use miette::{NamedSource, miette};
use regex::{Match, Regex};
use saphyr::{LoadableYamlNode, MarkedYaml, Marker};
use saphyr_parser::Span;

use crate::{
    core::parser::{
        blueprint::{
            Blueprint, BlueprintAction, BlueprintCall, BlueprintInput, BlueprintInputSlot,
            BlueprintParameter, BlueprintReservedSlot, BlueprintReturn, BlueprintTarget,
            BlueprintValue,
            errors::{BlueprintError, ErrorSpan, Marked},
            types::BlueprintTemplate,
        },
        helpers,
        sol_types::{YamlSolValue, parse_sol_type_str, parse_sol_value_marked},
    },
    meta_sol_types::MetaDynSolType,
};

/// A parsed template string
struct TemplateRef<'a> {
    /// Template source: "inputs" | "constants" | "returns" | "input_slots"
    source: Marked<&'a str>,
    name: Marked<&'a str>,
    _child: Option<Marked<&'a str>>,
}

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
        let actions = self.actions(root, &mut templates)?;

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
        templates: &mut BTreeMap<String, BlueprintTemplate>,
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
        let input_slots = self.blueprint_input_slots(field)?;
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

            let str_selector: String = self
                .get_str(elem, "selector")?
                .chars()
                .filter(|c| !c.is_whitespace())
                .collect();
            let hash_selector = keccak256(str_selector);
            let selector = Selector::from(hash_selector.first_chunk().unwrap());

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

    fn blueprint_parameter<'a>(
        &self,
        field: &'a MarkedYaml<'a>,
        templates: &BTreeMap<String, BlueprintTemplate>,
    ) -> miette::Result<BlueprintParameter> {
        let value_field = field
            .data
            .as_mapping_get("value")
            .ok_or_else(|| self.error(field.span, "parameter has no value"))?;

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
                    .map(|elem| self.blueprint_parameter(elem, templates))
                    .collect::<miette::Result<Vec<_>>>()?;

                Ok(BlueprintParameter::Tuple(params))
            }
            DynSolType::FixedArray(_, length) => {
                if sequence.len() != length {
                    return Err(self
                        .error(value_field, "does not match fixed array length")
                        .with_second_location(r#type.span, "defined here"))?;
                }

                let params = sequence
                    .iter()
                    .map(|elem| self.blueprint_parameter(elem, templates))
                    .collect::<miette::Result<Vec<_>>>()?;

                Ok(BlueprintParameter::FixedArray(params, length))
            }
            DynSolType::Array(_) => {
                let params = sequence
                    .iter()
                    .map(|elem| self.blueprint_parameter(elem, templates))
                    .collect::<miette::Result<Vec<_>>>()?;

                Ok(BlueprintParameter::DynamicArray(params))
            }
            _ => Err(self.error(
                value_field,
                "only Array | FixedArray | Tuple are allowed here",
            ))?,
        }
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
            let value = parse_sol_value_marked(&r#type, value_field).ok_or_else(|| {
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

        // https://github.com/MakinaHQ/transpiler/blob/4632a90810dc0598077e4e7c8eb64c1a0bcba428/src/core/parser/blueprint/parser.rs#L292-L301
        //
        // this does not match the original implementation (linked above)
        // which i believe is broken because Bytes & String are not a scalar type
        // but allowed above
        //
        // we want to allow custom structs because they are used for the dynamic types
        if r#type.is_dynamic() && !r#type.has_custom_struct() {
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

    fn sol_value<'a>(&self, field: &'a MarkedYaml<'a>) -> miette::Result<YamlSolValue> {
        let r#type = self.get_type(field, "type")?;

        let value_field = field
            .data
            .as_mapping_get("value")
            .ok_or(self.error(field.span, "'value' is missing"))?;

        let value = parse_sol_value_marked(&r#type, value_field).ok_or(
            self.error(value_field, "could not be parsed")
                .with_second_location(r#type.span, "into specified type"),
        )?;

        Ok(YamlSolValue {
            r#type: r#type.inner,
            value,
        })
    }

    /// Gets a string at key from a mapping
    fn get_str<'a>(&self, node: &'a MarkedYaml<'a>, key: &str) -> miette::Result<Marked<&'a str>> {
        let field = node
            .data
            .as_mapping_get(key)
            .ok_or_else(|| self.error(node.span, format!("'{key}' is missing")))?;

        let str = field
            .data
            .as_str()
            .ok_or_else(|| self.error(field.span, "must be a string".to_string()))?;

        Ok(Marked::new(str, field.span))
    }

    /// Gets an optional string from a mapping
    fn try_get_str<'a>(
        &self,
        node: &'a MarkedYaml<'a>,
        key: &str,
    ) -> miette::Result<Option<Marked<&'a str>>> {
        let Some(field) = node.data.as_mapping_get(key) else {
            return Ok(None);
        };

        let str = field
            .data
            .as_str()
            .ok_or_else(|| self.error(field.span, "must be a string".to_string()))?;

        Ok(Some(Marked::new(str, field.span)))
    }

    /// Gets a `DynSolType` from a mapping
    fn get_type<'a>(
        &self,
        node: &'a MarkedYaml<'a>,
        key: &str,
    ) -> miette::Result<Marked<DynSolType>> {
        let str = self.get_str(node, key)?;
        let r#type = parse_sol_type_str(*str).or(Err(self.error(str.span, "invalid type")))?;

        Ok(Marked::new(r#type, str.span))
    }

    /// Parses a template string
    fn parse_template_str<'a>(&self, field: &'a MarkedYaml<'a>) -> miette::Result<TemplateRef<'a>> {
        let str = field
            .data
            .as_str()
            .ok_or_else(|| self.error(field.span, "must be string"))?;

        let caps = self
            .template_regex
            .captures(str)
            .ok_or_else(|| self.error(field.span, "expected template string"))?;

        let source = caps
            .get(1)
            .ok_or_else(|| self.error(field.span, "must have a source"))?;

        let name = caps
            .get(2)
            .ok_or_else(|| self.error(field.span, "must have a name"))?;

        let child = caps.get(3);

        Ok(TemplateRef {
            source: self.mark_match(source, field),
            name: self.mark_match(name, field),
            _child: child.map(|c| self.mark_match(c, field)),
        })
    }

    /// Returns `true` if the field contains a string matching template field syntax
    fn is_template<'a>(&self, field: &'a MarkedYaml<'a>) -> bool {
        let Some(str) = field.data.as_str() else {
            return false;
        };

        self.template_regex.is_match(str)
    }

    /// Build a [Marked] str from a regex [Match]
    /// Assumes the match has no line breaks which should always be true given
    /// the `template_regex`
    fn mark_match<'a>(&self, r#match: Match<'a>, field: &'a MarkedYaml<'a>) -> Marked<&'a str> {
        let start = Marker::new(
            field.span.start.index() + r#match.start(),
            field.span.start.line(),
            field.span.start.col() + r#match.start(),
        );

        let end = Marker::new(
            field.span.start.index() + r#match.end(),
            field.span.start.line(),
            field.span.start.col() + r#match.end(),
        );

        let span = Span::new(start, end);

        Marked::new(r#match.as_str(), span)
    }

    /// Return [NamedSource] for this parser
    fn named_source(&self) -> NamedSource<String> {
        NamedSource::new(self.file.to_string_lossy(), self.source.clone())
    }

    /// Helper method to generate a [BlueprintError] referencing this parsers source
    fn error<S, Str>(&self, span: S, msg: Str) -> BlueprintError
    where
        S: Into<ErrorSpan>,
        Str: Into<String>,
    {
        BlueprintError::new(self.named_source(), span, msg)
    }
}

#[cfg(test)]
mod tests {
    use super::BlueprintParser;

    #[test]
    fn test_smoke() {
        let parser = BlueprintParser::new("test_data/blueprints/account.yaml").unwrap();

        parser.parse().unwrap();
    }
}
