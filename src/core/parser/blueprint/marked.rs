use std::{collections::BTreeMap, ops::Deref, path::PathBuf, str::FromStr};

use alloy::{
    dyn_abi::{DynSolType, DynSolValue},
    hex::FromHex,
    primitives::{Address, Bytes, FixedBytes, I256, Selector, U256, keccak256},
};
use indexmap::IndexMap;
use miette::{Diagnostic, NamedSource, SourceSpan, miette};
use regex::{Match, Regex};
use saphyr::{LoadableYamlNode, MarkedYaml, Marker};
use saphyr_parser::Span;
use thiserror::Error;

use crate::{
    core::parser::{
        blueprint::{
            Blueprint, BlueprintAction, BlueprintCall, BlueprintInput, BlueprintInputSlot,
            BlueprintParameter, BlueprintReservedSlot, BlueprintReturn, BlueprintTarget,
            BlueprintValue,
        },
        helpers,
        sol_types::{YamlSolValue, parse_sol_type_str},
    },
    meta_sol_types::MetaDynSolType,
};

/// Helper type to keep track of a parsed items location in the source.
/// Source and location need to be tracked externally.
struct Marked<T> {
    inner: T,
    span: Span,
}

impl<T> Marked<T> {
    fn new(inner: T, span: Span) -> Self {
        Self { inner, span }
    }
}

impl<T> Deref for Marked<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/// A parsed template string
struct TemplateRef<'a> {
    /// Template source: "inputs" | "constants" | "returns" | "input_slots"
    source: Marked<&'a str>,
    name: Marked<&'a str>,
    child: Option<Marked<&'a str>>,
}

// NOTE: would be nice to render better help / missing value errors
#[derive(Debug, Error, Diagnostic)]
#[error("Error found in blueprint")]
struct MarkedBlueprintError {
    msg: String,
    #[source_code]
    src: NamedSource<String>,
    #[label("{msg}")]
    location: SourceSpan,
    #[label("type definition")]
    type_location: Option<SourceSpan>,
}

pub struct MarkedBlueprintParser {
    file: PathBuf,
    content: String,
    template_regex: Regex,
}

impl MarkedBlueprintParser {
    pub fn new<P>(file: P) -> Self
    where
        P: Into<PathBuf>,
    {
        let template_regex = Regex::new(
            r"\$\{(inputs|input_slots|returns|constants)\.([a-zA-Z0-9_]+)(?:\.([a-zA-Z0-9_]+))?\}",
        )
        .expect("is valid regex");

        let file = file.into();
        // TODO: handle error
        let content = std::fs::read_to_string(&file).unwrap();
        Self {
            file,
            content,
            template_regex,
        }
    }

    pub fn parse(&self) -> miette::Result<Blueprint> {
        // TODO: builtins should be parsed like all other template vars
        let source = helpers::replace_builtins(&self.content)
            .map_err(|err| miette!("could not replace builtins: {err}"))?;
        let parsed = MarkedYaml::load_from_str(&source)
            .map_err(|err| miette!("could not parse blueprint file: {err}"))?;

        // TODO: render nice error here
        assert_eq!(parsed.len(), 1);
        let root = parsed.first().unwrap();

        // TODO: move root into self
        let protocol = self.protocol(root)?;
        let inputs = self.inputs(root)?;
        let constants = self.constants(root)?;
        let actions = self.actions(root, &inputs, &constants)?;

        Ok(Blueprint {
            path: self.file.clone(),
            protocol: protocol.to_string(),
            constants: constants
                .into_iter()
                .map(|(k, v)| (k.to_string(), v))
                .collect(),
            inputs: inputs
                .into_iter()
                .map(|(k, v)| (k.to_string(), v))
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

    // ---------------------
    // HELPERS
    // ---------------------

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

    fn get_type<'a>(
        &self,
        node: &'a MarkedYaml<'a>,
        key: &str,
    ) -> miette::Result<Marked<DynSolType>> {
        let str = self.get_str(node, key)?;
        let r#type = parse_sol_type_str(*str).or(Err(self.error(str.span, "invalid type")))?;

        Ok(Marked::new(r#type, str.span))
    }

    fn parse_template<'a>(&self, field: &'a MarkedYaml<'a>) -> miette::Result<TemplateRef<'a>> {
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
            child: child.map(|c| self.mark_match(c, field)),
        })
    }

    fn is_template<'a>(&self, field: &'a MarkedYaml<'a>) -> bool {
        let Some(str) = field.data.as_str() else {
            return false;
        };

        self.template_regex.is_match(str)
    }

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

    // ---------------------
    // END HELPERS
    // ---------------------

    fn constants<'a>(
        &self,
        root: &'a MarkedYaml<'a>,
    ) -> miette::Result<BTreeMap<&'a str, YamlSolValue>> {
        let Some(constants) = root.data.as_mapping_get("constants") else {
            return Ok(BTreeMap::new());
        };

        let mapping = constants
            .data
            .as_mapping()
            .ok_or(self.error(constants.span, "Blueprint has invalid constants definition"))?;

        let mut map = BTreeMap::new();
        for (key, value) in mapping {
            let name = key
                .data
                .as_str()
                .ok_or(self.error(key.span, "Constant has invalid name"))?;
            let value = self.sol_value(value)?;
            map.insert(name, value);
        }

        Ok(map)
    }

    fn inputs<'a>(
        &self,
        root: &'a MarkedYaml<'a>,
    ) -> miette::Result<BTreeMap<&'a str, BlueprintInput>> {
        let Some(inputs) = root.data.as_mapping_get("inputs") else {
            return Ok(BTreeMap::new());
        };

        let mapping = inputs
            .data
            .as_mapping()
            .ok_or(self.error(inputs.span, "must be a mapping"))?;

        let mut map = BTreeMap::new();
        for (key, value) in mapping {
            let name = key
                .data
                .as_str()
                .ok_or(self.error(key.span, "input has invalid name"))?;
            let value = self.blueprint_input(value)?;
            map.insert(name, value);
        }

        Ok(map)
    }

    // TODO: instead of passing inputs & constants here parser could impl
    // type state pattern
    fn actions<'a>(
        &self,
        root: &'a MarkedYaml<'a>,
        inputs: &BTreeMap<&'a str, BlueprintInput>,
        constants: &BTreeMap<&'a str, YamlSolValue>,
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

            let action = self.blueprint_action(value, inputs, constants)?;

            map.insert(name, action);
        }

        Ok(map)
    }

    fn blueprint_action<'a>(
        &self,
        field: &'a MarkedYaml<'a>,
        inputs: &BTreeMap<&'a str, BlueprintInput>,
        constants: &BTreeMap<&'a str, YamlSolValue>,
    ) -> miette::Result<BlueprintAction> {
        let input_slots = self.blueprint_input_slots(field)?;
        let (calls, returns_mapping, returns_definition) = self.blueprint_calls(
            field,
            inputs,
            &input_slots
                .iter()
                .map(|(k, v)| (k.as_str(), v.clone()))
                .collect(),
            constants,
        )?;
        let reserved_slots =
            self.blueprint_reserved_slots(field, &returns_definition, constants)?;

        // TODO: check that all input slots are used

        Ok(BlueprintAction {
            calls,
            returns_mapping: returns_mapping
                .into_iter()
                .map(|(k, v)| (k.to_string(), v))
                .collect(),
            reserved_slots,
            input_slots,
        })
    }

    // TODO: using IndexMap for consistency but this could be BTreeMap i think
    // allows us to cut one dep
    fn blueprint_input_slots<'a>(
        &self,
        field: &'a MarkedYaml<'a>,
        // we have to return IndexMap<String, _> because currently the MetaDynSolType creates owned
        // keys
        // this is not great, it would be nice to avoid having to change the transpiler for
        // protocol integrations
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

            let input = self.blueprint_input(value)?;

            match input.r#type {
                DynSolType::CustomStruct {
                    name,
                    prop_names,
                    tuple,
                } => {
                    // TODO: don't love relying on the name to match the type
                    let meta_type = MetaDynSolType::from_str(&name)
                        .or(Err(self.error(key.span, "invalid meta type")))?;

                    // TODO: check this in blueprint_input to provide better error
                    if prop_names.len() != tuple.len() {
                        return Err(self.error(value.span, "properties do not match tuple"))?;
                    }

                    for (prop_name, prop_type) in prop_names.into_iter().zip(tuple) {
                        map.insert(
                            format!("{slot_name}.{prop_name}"),
                            BlueprintInputSlot {
                                r#type: prop_type,
                                description: input.description.to_owned(),
                                meta_type: Some(meta_type.to_owned()),
                                meta_type_field: Some(prop_name),
                                meta_type_name: Some(slot_name.to_owned()),
                            },
                        );
                    }
                }
                _ => {
                    map.insert(
                        slot_name.to_string(),
                        BlueprintInputSlot {
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

    #[expect(clippy::type_complexity)]
    fn blueprint_calls<'a>(
        &self,
        field: &'a MarkedYaml<'a>,
        inputs: &BTreeMap<&'a str, BlueprintInput>,
        input_slots: &BTreeMap<&'a str, BlueprintInputSlot>,
        constants: &BTreeMap<&'a str, YamlSolValue>,
    ) -> miette::Result<(
        Vec<BlueprintCall>,
        BTreeMap<&'a str, usize>,
        BTreeMap<&'a str, BlueprintReturn>,
    )> {
        let calls = field
            .data
            .as_mapping_get("calls")
            .ok_or(self.error(field.span, "'calls' is missing"))?;

        let sequence = calls
            .data
            .as_sequence()
            .ok_or(self.error(calls.span, "must be a sequence"))?;

        let mut vec = Vec::new();
        // TODO: this could be a single map
        let mut returns_mapping = BTreeMap::new();
        let mut returns_definition = BTreeMap::new();
        for (idx, elem) in sequence.iter().enumerate() {
            let description = self
                .try_get_str(elem, "description")?
                .map(|d| d.inner.to_string())
                .unwrap_or_default();

            let target_field = elem
                .data
                .as_mapping_get("target")
                .ok_or(self.error(elem.span, "'target' is missing"))?;

            let target = self.blueprint_target(target_field, inputs, constants)?;

            let str_selector = self.get_str(elem, "selector")?;
            let hash_selector = keccak256(*str_selector);
            let selector = Selector::from(hash_selector.first_chunk().unwrap());

            let parameters_field = elem
                .data
                .as_mapping_get("parameters")
                .ok_or(self.error(elem.span, "'parameters' is missing"))?;
            let parameters = self.blueprint_parameters(
                parameters_field,
                inputs,
                input_slots,
                constants,
                &returns_definition,
            )?;

            let r#return = if let Some(return_field) = elem.data.as_mapping_get("return") {
                let name = self.get_str(return_field, "name")?;

                // TODO: if we save a marked type here we could show both occurences in the error
                if returns_definition.contains_key(*name) {
                    return Err(self.error(name.span, "duplicate return name"))?;
                }

                let r#type = self.get_type(return_field, "type")?;

                let blueprint_return = BlueprintReturn {
                    name: name.to_string(),
                    r#type: r#type.inner,
                };
                returns_mapping.insert(*name, idx);
                returns_definition.insert(*name, blueprint_return.clone());

                Some(blueprint_return)
            } else {
                None
            };

            // TODO: description should be Option<String> probably
            vec.push(BlueprintCall {
                description,
                target,
                selector,
                parameters,
                r#return,
            });
        }

        Ok((vec, returns_mapping, returns_definition))
    }

    fn blueprint_parameters<'a>(
        &self,
        field: &'a MarkedYaml<'a>,
        inputs: &BTreeMap<&'a str, BlueprintInput>,
        input_slots: &BTreeMap<&'a str, BlueprintInputSlot>,
        constants: &BTreeMap<&'a str, YamlSolValue>,
        returns: &BTreeMap<&'a str, BlueprintReturn>,
    ) -> miette::Result<Vec<BlueprintParameter>> {
        let sequence = field
            .data
            .as_sequence()
            .ok_or(self.error(field.span, "must be sequence"))?;

        let mut vec = Vec::new();
        for elem in sequence.iter() {
            let parsed = self.blueprint_parameter(elem, inputs, input_slots, constants, returns)?;
            vec.push(parsed);
        }

        Ok(vec)
    }

    fn blueprint_parameter<'a>(
        &self,
        field: &'a MarkedYaml<'a>,
        inputs: &BTreeMap<&'a str, BlueprintInput>,
        input_slots: &BTreeMap<&'a str, BlueprintInputSlot>,
        constants: &BTreeMap<&'a str, YamlSolValue>,
        returns: &BTreeMap<&'a str, BlueprintReturn>,
    ) -> miette::Result<BlueprintParameter> {
        let r#type = self.get_type(field, "type")?;

        let value_field = field
            .data
            .as_mapping_get("value")
            .ok_or(self.error(field.span, "'value not found'"))?;

        if let Some(str_value) = value_field.data.as_str()
            && self.template_regex.is_match(str_value)
        {
            let param =
                self.resolve_template(value_field, inputs, input_slots, constants, returns)?;

            // TODO: remebmer to check type here or in resolve step
            //
            return Ok(param);
        }

        match r#type.inner {
            DynSolType::Tuple(types) => {
                let sequence = value_field.data.as_sequence().ok_or(self.error_with_def(
                    value_field.span,
                    r#type.span,
                    "expected sequence",
                ))?;

                if sequence.len() != types.len() {
                    return Err(self.error_with_def(
                        value_field.span,
                        r#type.span,
                        "unexpected length",
                    ))?;
                }

                let mut params = Vec::new();
                for elem in sequence {
                    let parsed =
                        self.blueprint_parameter(elem, inputs, input_slots, constants, returns)?;

                    // TODO: check type matches, prob add a type() helper to BluepParam struct

                    params.push(parsed);
                }

                Ok(BlueprintParameter::Tuple(params))
            }
            DynSolType::FixedArray(_type, size) => {
                let sequence = value_field.data.as_sequence().ok_or(self.error_with_def(
                    value_field.span,
                    r#type.span,
                    "expected sequence",
                ))?;

                if sequence.len() != size {
                    return Err(self.error_with_def(
                        value_field.span,
                        r#type.span,
                        "unexpected length",
                    ))?;
                }

                let mut params = Vec::new();
                for elem in sequence {
                    let parsed =
                        self.blueprint_parameter(elem, inputs, input_slots, constants, returns)?;

                    // TODO: check type matches, prob add a type() helper to BluepParam struct

                    params.push(parsed);
                }

                Ok(BlueprintParameter::FixedArray(params, size))
            }
            DynSolType::Array(_type) => {
                let sequence = value_field.data.as_sequence().ok_or(self.error_with_def(
                    value_field.span,
                    r#type.span,
                    "expected sequence",
                ))?;

                let mut params = Vec::new();
                for elem in sequence {
                    let parsed =
                        self.blueprint_parameter(elem, inputs, input_slots, constants, returns)?;

                    // TODO: check type matches, prob add a type() helper to BluepParam struct

                    params.push(parsed);
                }

                Ok(BlueprintParameter::DynamicArray(params))
            }
            _ => {
                // NOTE: old parser does not handle template strings here but i guess
                // we could add support for them (especially with nice helper;
                // also descriptions for non scalar types are dropped which seems odd

                let description = self
                    .try_get_str(field, "description")?
                    .map(|d| d.inner.to_string());

                // TODO: this could just support non scalar types as well?
                let value = parse_sol_value(&r#type, value_field).ok_or(self.error_with_def(
                    value_field.span,
                    r#type.span,
                    "could not pares value",
                ))?;

                Ok(BlueprintParameter::Scalar(BlueprintValue {
                    value,
                    r#type: r#type.inner,
                    description,
                }))
            }
        }
    }

    fn resolve_template<'a>(
        &self,
        field: &'a MarkedYaml<'a>,
        inputs: &BTreeMap<&'a str, BlueprintInput>,
        input_slots: &BTreeMap<&'a str, BlueprintInputSlot>,
        constants: &BTreeMap<&'a str, YamlSolValue>,
        returns: &BTreeMap<&'a str, BlueprintReturn>,
    ) -> miette::Result<BlueprintParameter> {
        // TODO: we always duplicate the logic to pares raw & template strings, instead we try
        // do TemplateResovler::try_from() and have the logic encapsulated there
        // this would need refs to all the maps tho
        let template = self.parse_template(field)?;

        match *template.source {
            "returns" => {
                let _def = returns
                    .get(*template.name)
                    .ok_or(self.error(template.name.span, "unknown return"))?;

                Ok(BlueprintParameter::Return(template.name.to_owned()))
            }
            "input_slots" => {
                let slot_name = if let Some(child) = template.child {
                    format!("{}.{}", template.name.inner, child.inner)
                } else {
                    template.name.to_owned()
                };

                let _def = input_slots
                    .get(slot_name.as_str())
                    .ok_or(self.error(template.name.span, "unknown input_slot"))?;

                Ok(BlueprintParameter::InputSlot(slot_name))
            }
            "constants" => {
                let def = constants
                    .get(*template.name)
                    .ok_or(self.error(template.name.span, "unknown constant"))?;

                Ok(BlueprintParameter::Scalar(BlueprintValue {
                    value: def.value.clone(),
                    r#type: def.r#type.clone(),
                    description: None, // TODO: we should get this here from outside context
                }))
            }
            "inputs" => {
                // TODO: change to contains key if don't check types here
                let _def = inputs
                    .get(*template.name)
                    .ok_or(self.error(template.name.span, "unknown input"))?;

                Ok(BlueprintParameter::Input {
                    name: template.name.to_owned(),
                })
            }

            _ => Err(self.error(field.span, "invalid template source"))?,
        }
    }

    fn blueprint_target<'a>(
        &self,
        field: &'a MarkedYaml<'a>,
        inputs: &BTreeMap<&'a str, BlueprintInput>,
        constants: &BTreeMap<&'a str, YamlSolValue>,
    ) -> miette::Result<BlueprintTarget> {
        if !self.is_template(field) {
            todo!("raw value")
            // return
        }

        let template = self.parse_template(field)?;

        match *template.source {
            "constants" => {
                let def = constants
                    .get(*template.name)
                    .ok_or(self.error(template.name.span, "unknown constant"))?;

                Ok(BlueprintTarget::Address(
                    def.value
                        .as_address()
                        .ok_or(self.error(field.span, "must be address"))?,
                ))
            }
            "inputs" => {
                let def = inputs
                    .get(*template.name)
                    .ok_or(self.error(template.name.span, "unknown input"))?;

                if def.r#type != DynSolType::Address {
                    return Err(self.error(field.span, "must be address"))?;
                }

                Ok(BlueprintTarget::Input(template.name.to_string()))
            }

            _ => Err(self.error(field.span, "must be 'constants' or 'inputs'"))?,
        }
    }

    fn blueprint_reserved_slots<'a>(
        &self,
        field: &'a MarkedYaml<'a>,
        returns: &BTreeMap<&'a str, BlueprintReturn>,
        constants: &BTreeMap<&'a str, YamlSolValue>,
    ) -> miette::Result<Vec<BlueprintReservedSlot>> {
        // TODO: this duplicates blueprint_input and should be consolidated
        let Some(reserved_slots) = field.data.as_mapping_get("reserved_slots") else {
            return Ok(Vec::new());
        };

        let sequence = reserved_slots
            .data
            .as_sequence()
            .ok_or(self.error(reserved_slots.span, "must be a sequence"))?;

        let mut vec = Vec::new();
        for elem in sequence {
            let r#type = self.get_type(elem, "type")?;

            // TODO: this does not match the original implementation which i think
            // is broken (ie Bytes & String are not a scalar type)
            // We might still want to exclude FixedArray / Tuple / CustomString here
            if r#type.is_dynamic() {
                return Err(self.error(r#type.span, "must be scalar type"))?;
            }

            let description = self
                .try_get_str(elem, "description")?
                .map(|d| d.inner.to_string());

            let value_field = elem
                .data
                .as_mapping_get("value")
                .ok_or(self.error(elem.span, "'value' is missing"))?;

            if self.is_template(value_field) {
                let template = self.parse_template(value_field)?;

                match *template.source {
                    "returns" => {
                        let def = returns
                            .get(*template.name)
                            .ok_or(self.error(template.name.span, "unknown return"))?;

                        // TODO: once we have spans in blueprint struct we can link to
                        // the return definition in the error
                        // also not sure why we have to specify the type twice at all
                        if def.r#type != *r#type {
                            return Err(self.error(
                                value_field.span,
                                "type does not match return definition",
                            ))?;
                        }

                        vec.push(BlueprintReservedSlot::Return(template.name.to_owned()));
                    }
                    "constants" => {
                        let def = constants
                            .get(*template.name)
                            .ok_or(self.error(template.name.span, "unknown constant"))?;

                        if def.r#type != *r#type {
                            return Err(self.error(
                                value_field.span,
                                "type does not match constant definition",
                            ))?;
                        }

                        vec.push(BlueprintReservedSlot::Scalar(BlueprintValue {
                            value: def.value.clone(),
                            r#type: r#type.inner,
                            description,
                        }));
                    }
                    _ => {
                        // TODO: get exact span from regex match
                        return Err(
                            self.error(value_field.span, "must be 'returns' or 'constants'")
                        )?;
                    }
                }

                continue;
            }

            let value = parse_sol_value(&r#type, value_field).ok_or(self.error_with_def(
                value_field.span,
                r#type.span,
                "'value' could not be parsed into 'type'",
            ))?;

            vec.push(BlueprintReservedSlot::Scalar(BlueprintValue {
                value,
                r#type: r#type.inner,
                description,
            }));
        }

        Ok(vec)
    }

    fn blueprint_input<'a>(&self, field: &'a MarkedYaml<'a>) -> miette::Result<BlueprintInput> {
        let r#type = self.get_type(field, "type")?;

        // TODO: this does not match the original implementation which i think
        // is broken (ie Bytes & String are not a scalar type)
        // We might still want to exclude FixedArray / Tuple / CustomString here
        // - ok so we def want custom struct for metatypes
        if r#type.is_dynamic() && !r#type.has_custom_struct() {
            return Err(self.error(r#type.span, "must be scalar type"))?;
        }

        let description = self
            .try_get_str(field, "description")?
            .map(|d| d.inner.to_string());

        Ok(BlueprintInput {
            r#type: r#type.inner,
            description,
        })
    }

    // TODO: eventually change blueprint to only take DynSolValue here since this has the type
    // anyways
    fn sol_value<'a>(&self, field: &'a MarkedYaml<'a>) -> miette::Result<YamlSolValue> {
        let r#type = self.get_type(field, "type")?;

        let value_field = field
            .data
            .as_mapping_get("value")
            .ok_or(self.error(field.span, "'value' is missing"))?;

        let value = parse_sol_value(&r#type, value_field).ok_or(self.error_with_def(
            value_field.span,
            r#type.span,
            "'value' could not be parsed into 'type'",
        ))?;

        Ok(YamlSolValue {
            r#type: r#type.inner,
            value,
        })
    }

    fn named_source(&self) -> NamedSource<String> {
        NamedSource::new(self.file.to_string_lossy(), self.content.clone())
    }

    fn error<S>(&self, span: Span, msg: S) -> MarkedBlueprintError
    where
        S: Into<String>,
    {
        MarkedBlueprintError {
            msg: msg.into(),
            src: self.named_source(),
            location: (span.start.index(), span.end.index() - span.start.index()).into(),
            type_location: None,
        }
    }

    fn error_with_def<S>(&self, location: Span, def: Span, msg: S) -> MarkedBlueprintError
    where
        S: Into<String>,
    {
        MarkedBlueprintError {
            msg: msg.into(),
            src: self.named_source(),
            location: (
                location.start.index(),
                location.end.index() - location.start.index(),
            )
                .into(),
            type_location: Some((def.start.index(), def.end.index() - def.start.index()).into()),
        }
    }
}

fn parse_sol_value(r#type: &DynSolType, field: &MarkedYaml) -> Option<DynSolValue> {
    match r#type {
        DynSolType::Bool => {
            if let Some(val) = field.data.as_bool() {
                return Some(DynSolValue::Bool(val));
            }

            let parsed = field.data.as_str()?.parse().ok()?;
            Some(DynSolValue::Bool(parsed))
        }
        DynSolType::Int(bits) => {
            let val = field.data.as_str()?;
            let parsed = if val.starts_with("0x") {
                I256::from_hex_str(val).ok()?
            } else {
                I256::from_dec_str(val).ok()?
            };
            Some(DynSolValue::Int(parsed, *bits))
        }
        DynSolType::Uint(bits) => {
            let val = field.data.as_str()?;
            let parsed = U256::from_str(val).ok()?;
            Some(DynSolValue::Uint(parsed, *bits))
        }
        DynSolType::Address => {
            let val = field.data.as_str()?;
            let parsed = Address::from_str(val).ok()?;
            Some(DynSolValue::Address(parsed))
        }
        DynSolType::FixedBytes(bytes) => {
            let val = field.data.as_str()?;
            let parsed = FixedBytes::from_hex(val).ok()?;
            // NOTE: refactored, this might be broken
            Some(DynSolValue::FixedBytes(parsed, *bytes))
        }
        DynSolType::String => {
            let val = field.data.as_str()?.into();
            Some(DynSolValue::String(val))
        }
        DynSolType::Bytes => {
            let val = field.data.as_str()?;
            let parsed = Bytes::from_hex(val).ok()?.into();
            Some(DynSolValue::Bytes(parsed))
        }
        DynSolType::Tuple(types) => {
            let seq = field.data.as_sequence()?;
            let parsed = types
                .iter()
                .zip(seq)
                .map(|(r#type, field)| parse_sol_value(r#type, field))
                .collect::<Option<Vec<DynSolValue>>>()?;
            Some(DynSolValue::Tuple(parsed))
        }
        DynSolType::Array(r#type) => {
            let seq = field.data.as_sequence()?;
            let parsed = seq
                .iter()
                .map(|field| parse_sol_value(r#type, field))
                .collect::<Option<Vec<DynSolValue>>>()?;
            Some(DynSolValue::Array(parsed))
        }
        DynSolType::FixedArray(r#type, size) => {
            let seq = field.data.as_sequence()?;
            if seq.len() != *size {
                return None;
            }
            let parsed = seq
                .iter()
                .map(|field| parse_sol_value(r#type, field))
                .collect::<Option<Vec<DynSolValue>>>()?;

            Some(DynSolValue::FixedArray(parsed))
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::MarkedBlueprintParser;

    #[test]
    fn test_smoke() {
        let parser = MarkedBlueprintParser::new("test_data/blueprints/account.yaml");

        parser.parse().unwrap();
    }
}
