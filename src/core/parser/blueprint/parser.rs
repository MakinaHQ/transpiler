use indexmap::IndexMap;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::read_to_string;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use alloy::{
    dyn_abi::DynSolType,
    primitives::{Address, Selector, keccak256},
};
use regex::Regex;
use saphyr::{Mapping, Yaml};

use crate::core::parser::{
    errors::ParserError,
    helpers,
    sol_types::{self, YamlSolValue},
};
use crate::meta_sol_types::MetaDynSolType;

use super::{
    errors::{BlueprintParserError, BlueprintResult},
    types::*,
};

pub struct BlueprintParser {
    template_regex: Regex,
}

impl Default for BlueprintParser {
    fn default() -> Self {
        Self::new()
    }
}

impl BlueprintParser {
    /// Create a new blueprint parser
    pub fn new() -> Self {
        Self {
            template_regex: Regex::new(
                r"\$\{(inputs|input_slots|returns|constants)\.([a-zA-Z0-9_]+)(?:\.([a-zA-Z0-9_]+))?\}",
            )
            .unwrap(),
        }
    }

    /// Parse a blueprint from a file
    pub fn parse_from_file(&self, path: &PathBuf) -> BlueprintResult<Blueprint> {
        let file = File::open(path).map_err(ParserError::Io)?;
        let s = read_to_string(file).map_err(ParserError::Io)?;
        self.parse_from_str(&s, path)
    }

    /// Parse a blueprint from a string.
    pub fn parse_from_str(&self, content: &str, path: &Path) -> BlueprintResult<Blueprint> {
        // Replace all the builtins with their values.
        let content = helpers::replace_builtins(content)?;

        // Parse the content
        let yaml = helpers::load_yaml_from_str(&content, true)
            .map_err(BlueprintParserError::FailedToLoadBlueprint)?;

        self.parse(&yaml, path)
    }

    /// Parse a blueprint from a YAML content
    pub fn parse(&self, yaml: &Yaml, path: &Path) -> BlueprintResult<Blueprint> {
        let root_map = helpers::get_mapping(yaml, "root")?;

        let protocol = helpers::get_string_field(root_map, "protocol", "root")?.to_string();
        let constants = self.parse_constants(root_map)?;
        let inputs = self.parse_inputs(root_map)?;
        let actions = self.parse_actions(root_map, &inputs, &constants)?;

        Ok(Blueprint {
            path: path.to_path_buf(),
            protocol,
            constants,
            inputs,
            actions,
        })
    }

    /// Parse the constants section.
    fn parse_constants(
        &self,
        root_map: &Mapping,
    ) -> BlueprintResult<HashMap<String, YamlSolValue>> {
        // Constants are optional.
        let Some(constants_yaml) =
            helpers::get_optional_mapping_field(root_map, "constants", "root")?
        else {
            return Ok(HashMap::new());
        };

        let mut constants = HashMap::new();
        for (key_yaml, value_yaml) in constants_yaml.iter() {
            // Get the key as a string.
            let key = helpers::get_string(key_yaml, "constants key")?;

            // Parse the constant value and type.
            let constant = sol_types::parse_yaml_sol_value(value_yaml, "root.constants")
                .map_err(|err| BlueprintParserError::InvalidSolType(Box::new(err)))?;

            constants.insert(key.to_string(), constant);
        }

        Ok(constants)
    }

    /// Parse the inputs section.
    fn parse_inputs(&self, root_map: &Mapping) -> BlueprintResult<HashMap<String, BlueprintInput>> {
        // Inputs are optional.
        let Some(inputs_yaml) = helpers::get_optional_mapping_field(root_map, "inputs", "root")?
        else {
            return Ok(HashMap::new());
        };

        let mut inputs = HashMap::new();
        for (key_yaml, value_yaml) in inputs_yaml.iter() {
            let name = helpers::get_string(key_yaml, "inputs key")?;
            let field_path = format!("inputs.{name}");

            let input_map = helpers::get_mapping(value_yaml, &field_path)?;
            let input = self.parse_input_definition(input_map, &field_path)?;
            inputs.insert(name.to_string(), input);
        }

        Ok(inputs)
    }

    /// Parse the actions section.
    fn parse_actions(
        &self,
        root_map: &Mapping,
        inputs: &HashMap<String, BlueprintInput>,
        constants: &HashMap<String, YamlSolValue>,
    ) -> BlueprintResult<HashMap<String, BlueprintAction>> {
        // Actions are required.
        let actions_yaml = helpers::get_mapping_field(root_map, "actions", "root")?;

        let mut actions = HashMap::new();
        for (key_yaml, action_yaml) in actions_yaml.iter() {
            let action_name = helpers::get_string(key_yaml, "actions key")?;
            let field_path = format!("actions.{action_name}");

            let action_map = helpers::get_mapping(action_yaml, &field_path)?;
            let action = self.parse_action(action_map, &field_path, inputs, constants)?;
            actions.insert(action_name.to_string(), action);
        }

        Ok(actions)
    }

    /// Parse a single action.
    fn parse_action(
        &self,
        action_map: &Mapping,
        action_path: &str, // Path up to this map, e.g. "actions.my_action"
        inputs: &HashMap<String, BlueprintInput>,
        constants: &HashMap<String, YamlSolValue>,
    ) -> BlueprintResult<BlueprintAction> {
        // Parse input slots first (optional).
        let input_slots = self.parse_input_slots(action_map, action_path)?;

        // Initialize structures to track return values across calls.
        let mut returns_mapping: HashMap<String, usize> = HashMap::new(); // name -> call_index
        let mut returns_definitions: HashMap<String, BlueprintReturn> = HashMap::new(); // name -> definition

        // Parse calls (required).
        let calls_yaml = helpers::get_sequence_field(action_map, "calls", action_path)?;
        let mut calls = Vec::new();
        for (index, call_yaml) in calls_yaml.iter().enumerate() {
            let call_path = format!("{action_path}.calls[{index}]");
            let call_map = helpers::get_mapping(call_yaml, &call_path)?;

            let call = self.parse_call(
                index,
                call_map,
                &call_path,
                &mut returns_mapping,
                &mut returns_definitions,
                constants,
                inputs,
                &input_slots,
            )?;
            calls.push(call);
        }

        // Parse reserved slots (optional)
        let reserved_slots =
            self.parse_reserved_slots(action_map, action_path, &returns_definitions, constants)?;

        // Gather all the used input slots in the calls' parameters.
        let used_input_slots: HashSet<String> = calls
            .iter()
            .flat_map(|c| &c.parameters)
            .flat_map(|p| p.input_slots())
            .collect();

        // only keep the input slots that are actually used in the calls
        let input_slots = input_slots
            .into_iter()
            .filter(|(name, _)| used_input_slots.contains(name))
            .collect();

        Ok(BlueprintAction {
            calls,
            returns_mapping,
            reserved_slots,
            input_slots,
        })
    }

    /// Parse input slots.
    fn parse_input_slots(
        &self,
        action_map: &Mapping,
        action_path: &str, // Path up to this map, e.g. "actions.my_action"
    ) -> BlueprintResult<IndexMap<String, BlueprintInputSlot>> {
        let Some(input_slots_yaml) =
            helpers::get_optional_mapping_field(action_map, "input_slots", action_path)?
        else {
            return Ok(IndexMap::new());
        };

        let mut input_slots = IndexMap::new();
        for (key_yaml, value_yaml) in input_slots_yaml.iter() {
            let input_slot_name = helpers::get_string(key_yaml, "input slots key")?;
            let field_path = format!("{action_path}.input_slots.{input_slot_name}");

            let input_slot_map = helpers::get_mapping(value_yaml, &field_path)?;
            let input = self.parse_input_definition(input_slot_map, &field_path)?;

            match input.r#type {
                DynSolType::CustomStruct {
                    name,
                    prop_names,
                    tuple,
                } => {
                    let meta_type = MetaDynSolType::from_str(&name).unwrap();
                    // Then insert an element for each property.
                    if prop_names.len() != tuple.len() {
                        return Err(BlueprintParserError::InvalidCustomStruct {
                            field_path: field_path.to_string(),
                            struct_name: name,
                            properties_length: prop_names.len(),
                            types_length: tuple.len(),
                        });
                    }
                    for (prop_name, prop_type) in prop_names.into_iter().zip(tuple.into_iter()) {
                        input_slots.insert(
                            format!("{input_slot_name}.{prop_name}"),
                            BlueprintInputSlot {
                                r#type: prop_type,
                                description: input.description.clone(),
                                meta_type: Some(meta_type.clone()),
                                meta_type_field: Some(prop_name.to_string()),
                                meta_type_name: Some(input_slot_name.to_string()),
                            },
                        );
                    }
                }
                _ => {
                    input_slots.insert(
                        input_slot_name.to_string(),
                        BlueprintInputSlot {
                            r#type: input.r#type,
                            description: input.description.clone(),
                            meta_type: None,
                            meta_type_field: None,
                            meta_type_name: None,
                        },
                    );
                }
            }
        }

        Ok(input_slots)
    }

    /// Parse the definition of an input or input_slot.
    fn parse_input_definition(
        &self,
        input_map: &Mapping,
        field_path: &str,
    ) -> BlueprintResult<BlueprintInput> {
        let type_str = helpers::get_string_field(input_map, "type", field_path)?;
        let r#type = sol_types::parse_sol_type_str(type_str)?;

        // Check if the type is scalar.
        if matches!(
            r#type,
            DynSolType::Tuple(_) | DynSolType::FixedArray(_, _) | DynSolType::Array(_)
        ) {
            return Err(BlueprintParserError::InputTypeMustBeScalar {
                field_path: field_path.to_string(),
                found_type: r#type.to_string(),
            });
        }

        let description = helpers::get_optional_string_field(input_map, "description", field_path)?;

        Ok(BlueprintInput {
            r#type,
            description: description.map(String::from),
        })
    }

    /// Parse a single call definition.
    #[allow(clippy::too_many_arguments)]
    fn parse_call(
        &self,
        index: usize, // Index of the call within the action's calls array
        call_map: &Mapping,
        call_path: &str, // e.g. "actions.my_action.calls[0]"
        returns_mapping: &mut HashMap<String, usize>,
        returns_definitions: &mut HashMap<String, BlueprintReturn>,
        constants: &HashMap<String, YamlSolValue>,
        inputs: &HashMap<String, BlueprintInput>,
        input_slots: &IndexMap<String, BlueprintInputSlot>,
    ) -> BlueprintResult<BlueprintCall> {
        // Parse the description first. Optional.
        let description = helpers::get_optional_string_field(call_map, "description", call_path)?
            .unwrap_or_default()
            .to_string();

        // Parse the target. Required.
        let target_yaml = helpers::get_field(call_map, "target", call_path)?;
        let target = self.parse_target(target_yaml, call_path, constants, inputs)?;

        // Parse the selector. Required.
        let selector_str = helpers::get_string_field(call_map, "selector", call_path)?;
        let selector = self.parse_selector(selector_str)?;

        // Parse the parameters.
        let parameters_yaml = helpers::get_sequence_field(call_map, "parameters", call_path)?;
        let mut parameters = Vec::new();

        for (index, parameter_yaml) in parameters_yaml.iter().enumerate() {
            let param_path = format!("{call_path}.parameters[{index}]");
            let param_map = helpers::get_mapping(parameter_yaml, &param_path)?;
            let parameter = self.parse_parameter(
                param_map,
                &param_path,
                returns_definitions,
                constants,
                inputs,
                input_slots,
            )?;
            parameters.push(parameter);
        }

        // Return: Optional mapping { name: string, type: string }
        let return_def = if let Some(return_yaml) =
            call_map.get(&Yaml::scalar_from_string("return".to_string()))
        {
            let return_path = format!("{call_path}.return");
            let return_map = helpers::get_mapping(return_yaml, &return_path)?;

            let name = helpers::get_string_field(return_map, "name", &return_path)?.to_string();
            let type_str = helpers::get_string_field(return_map, "type", &return_path)?;
            let r#type = sol_types::parse_sol_type_str(type_str)?;

            // Check for duplicate return names within the action
            if returns_definitions.contains_key(&name) {
                return Err(BlueprintParserError::DuplicateReturnName {
                    name,
                    call_path: call_path.to_string(),
                });
            }

            let blueprint_return = BlueprintReturn {
                name: name.clone(),
                r#type,
            };

            // Store the mapping and definition
            returns_definitions.insert(name.clone(), blueprint_return.clone());
            returns_mapping.insert(name, index); // Map name to the index of *this* call

            Some(blueprint_return)
        } else {
            None
        };

        Ok(BlueprintCall {
            description,
            target,
            selector,
            parameters,
            r#return: return_def,
        })
    }

    /// Parse the target of a call. This can be a direct address, an input or a constant.
    fn parse_target(
        &self,
        target_yaml: &Yaml,
        field_path: &str,
        constants: &HashMap<String, YamlSolValue>,
        inputs: &HashMap<String, BlueprintInput>,
    ) -> BlueprintResult<BlueprintTarget> {
        let target_str = helpers::get_string(target_yaml, field_path)?;

        // Check if it's a template variable first
        if let Some(caps) = self.template_regex.captures(target_str) {
            let source = caps.get(1).map_or("", |m| m.as_str());
            let name = caps.get(2).map_or("", |m| m.as_str());

            match source {
                "constants" => {
                    let constant = constants.get(name).ok_or_else(|| {
                        BlueprintParserError::TemplateVariableNotFound {
                            source_path: "constants".to_string(),
                            variable: name.to_string(),
                        }
                    })?;
                    if constant.r#type != DynSolType::Address {
                        return Err(BlueprintParserError::TypeMismatch {
                            expected: DynSolType::Address.to_string(),
                            actual: constant.r#type.to_string(),
                        });
                    }
                    Ok(BlueprintTarget::Address(
                        constant.value.as_address().unwrap(),
                    )) // Safe unwrap due to type check
                }
                "inputs" => {
                    let input = inputs.get(name).ok_or_else(|| {
                        BlueprintParserError::TemplateVariableNotFound {
                            source_path: "inputs".to_string(),
                            variable: name.to_string(),
                        }
                    })?;
                    if input.r#type != DynSolType::Address {
                        return Err(BlueprintParserError::TypeMismatch {
                            expected: DynSolType::Address.to_string(),
                            actual: input.r#type.to_string(),
                        });
                    }
                    // For inputs used as target, we store the name, resolution happens later
                    Ok(BlueprintTarget::Input(name.to_string()))
                }
                _ => Err(BlueprintParserError::InvalidTemplateSource {
                    source_path: source.to_string(),
                    context: field_path.to_string(),
                }),
            }
        } else {
            // Not a template, treat as a direct address string
            Address::from_str(target_str)
                .map(BlueprintTarget::Address)
                .map_err(|_| BlueprintParserError::InvalidAddressFormat {
                    field_path: field_path.to_string(),
                    value: target_str.to_string(),
                })
        }
    }

    /// Parses a selector string (e.g., "transfer(address,uint256)") into an alloy `Selector`.
    fn parse_selector(&self, selector_str: &str) -> BlueprintResult<Selector> {
        // This function was already quite clean, just ensure the input is &str
        let cleaned_str: String = selector_str
            .chars()
            .filter(|c| !c.is_whitespace())
            .collect();
        let selector_hash: [u8; 4] = keccak256(cleaned_str.as_bytes())[0..4].try_into().unwrap();
        Ok(Selector::from(selector_hash))
    }

    /// Parse the reserved_slots section within an action (optional)
    fn parse_reserved_slots(
        &self,
        action_map: &Mapping,
        action_path: &str,
        returns: &HashMap<String, BlueprintReturn>,
        constants: &HashMap<String, YamlSolValue>,
    ) -> BlueprintResult<Vec<BlueprintReservedSlot>> {
        let Some(reserved_slots_yaml) =
            helpers::get_optional_sequence_field(action_map, "reserved_slots", action_path)?
        else {
            // If the field is missing, it's valid (empty list)
            return Ok(Vec::new());
        };

        let mut reserved_slots = Vec::new();
        for (index, slot_yaml) in reserved_slots_yaml.iter().enumerate() {
            let slot_path = format!("{action_path}.reserved_slots[{index}]");
            let slot_map = helpers::get_mapping(slot_yaml, &slot_path)?;

            // Parse the type string
            let type_str = helpers::get_string_field(slot_map, "type", &slot_path)?;
            let expected_type = sol_types::parse_sol_type_str(type_str)?;

            // Validate that the type is scalar
            if matches!(
                expected_type,
                DynSolType::Array(_) | DynSolType::Tuple(_) | DynSolType::FixedArray(_, _)
            ) {
                return Err(BlueprintParserError::ReservedSlotMustBeScalar {
                    field_path: slot_path,
                    found_type: expected_type.to_string(),
                });
            }

            // Get the value YAML node
            let value_yaml = helpers::get_field(slot_map, "value", &slot_path)?;

            // Get the optional description.
            let description =
                helpers::get_optional_string_field(slot_map, "description", &slot_path)?;

            // Check if the value is a template string or a literal scalar
            let reserved_slot = if let Some(value_str) = value_yaml
                .as_str()
                .filter(|s| self.template_regex.is_match(s))
            {
                // It's a template string, resolve it
                self.resolve_reserved_slot_template(
                    value_str,
                    &expected_type,
                    &slot_path, // Pass path to the slot map for context
                    description,
                    returns,
                    constants,
                )?
            } else {
                // Not a template, attempt to parse as a literal scalar value
                let parsed_value =
                    sol_types::parse_sol_value(&expected_type, value_yaml, &slot_path)
                        .map_err(|err| BlueprintParserError::InvalidSolType(Box::new(err)))?;

                BlueprintReservedSlot::Scalar(BlueprintValue {
                    value: parsed_value,
                    r#type: expected_type.clone(),
                    description: description.map(String::from),
                })
            };

            reserved_slots.push(reserved_slot);
        }

        Ok(reserved_slots)
    }

    /// Parses a single parameter definition (used in calls and potentially reserved_slots)
    fn parse_parameter(
        &self,
        param_map: &Mapping,
        param_path: &str, // e.g., "actions.my_action.calls[0].parameters[1]"
        returns: &HashMap<String, BlueprintReturn>,
        constants: &HashMap<String, YamlSolValue>,
        inputs: &HashMap<String, BlueprintInput>,
        input_slots: &IndexMap<String, BlueprintInputSlot>,
    ) -> BlueprintResult<BlueprintParameter> {
        // Get the declared parameter type.
        let type_str = helpers::get_string_field(param_map, "type", param_path)?;
        let param_type = sol_types::parse_sol_type_str(type_str)
            .map_err(|err| BlueprintParserError::InvalidSolType(Box::new(err)))?;

        // Get the value field.
        let value_yaml = helpers::get_field(param_map, "value", param_path)?;
        let value_path = format!("{param_path}.value");

        // If the value Yaml is a string, check for template variables.
        if value_yaml.is_string() {
            // Value for scalar should be a string (either literal or template)
            let value_str = helpers::get_string(value_yaml, &value_path)?;

            // Get the optional description.
            let description =
                helpers::get_optional_string_field(param_map, "description", param_path)?;

            if self.template_regex.is_match(value_str) {
                // Use the central resolver.
                return self.resolve_template_variable(
                    value_str,
                    &param_type, // Pass the expected scalar type
                    description,
                    param_path, // Pass the path to the parameter map {type: S, value: template}
                    returns,
                    constants,
                    inputs,
                    input_slots,
                );
            }
        }

        // Parse the value based on the declared type.
        match param_type {
            DynSolType::Tuple(ref inner_types) => {
                let value_seq = helpers::get_sequence(value_yaml, &value_path)?;

                // Check if the tuple length matches the type signature
                if value_seq.len() != inner_types.len() {
                    return Err(BlueprintParserError::TupleLengthMismatch {
                        field_path: param_path.to_string(),
                        expected: inner_types.len(),
                        actual: value_seq.len(),
                    });
                }

                // Recursively parse elements into BlueprintParameter variants
                let mut element_params = Vec::with_capacity(inner_types.len());
                for (i, (elem_yaml, expected_elem_type)) in
                    value_seq.iter().zip(inner_types.iter()).enumerate()
                {
                    let elem_path = format!("{value_path}[{i}]");
                    // Expect each element to be a map {type: T, value: V}
                    let elem_map = helpers::get_mapping(elem_yaml, &elem_path)?;

                    // We still check the 'type' field within the element's map for consistency,
                    // though the primary type check happens during recursion.
                    let elem_type_str = helpers::get_string_field(elem_map, "type", &elem_path)?;
                    let elem_type = sol_types::parse_sol_type_str(elem_type_str)?;
                    if elem_type != *expected_elem_type {
                        return Err(BlueprintParserError::TypeMismatch {
                            expected: expected_elem_type.to_string(),
                            actual: elem_type.to_string(), // Use the type string from the element map
                        });
                    }

                    // Recursive call
                    let parsed_elem = self.parse_parameter(
                        elem_map,
                        &elem_path, // Pass the path to the element's map
                        returns,
                        constants,
                        inputs,
                        input_slots,
                    )?;
                    element_params.push(parsed_elem);
                }

                // Return the sequence variant containing potentially unresolved parameters
                Ok(BlueprintParameter::Tuple(element_params))
            }
            DynSolType::FixedArray(ref inner_type, size) => {
                let value_seq = helpers::get_sequence(value_yaml, &value_path)?;

                // Check fixed array length
                if value_seq.len() != size {
                    return Err(BlueprintParserError::FixedArrayLengthMismatch {
                        field_path: param_path.to_string(),
                        expected: size,
                        actual: value_seq.len(),
                    });
                }

                // Recursively parse elements
                let mut element_params = Vec::with_capacity(size);
                for (i, elem_yaml) in value_seq.iter().enumerate() {
                    let elem_path = format!("{value_path}[{i}]");
                    let elem_map = helpers::get_mapping(elem_yaml, &elem_path)?; // Expect map {type: T, value: V}

                    // Validate inner type from element's map
                    let elem_type_str = helpers::get_string_field(elem_map, "type", &elem_path)?;
                    let elem_type = sol_types::parse_sol_type_str(elem_type_str)?;
                    if elem_type != **inner_type {
                        // Deref Box<DynSolType>
                        return Err(BlueprintParserError::TypeMismatch {
                            expected: inner_type.to_string(),
                            actual: elem_type.to_string(),
                        });
                    }

                    let parsed_elem = self.parse_parameter(
                        elem_map,
                        &elem_path,
                        returns,
                        constants,
                        inputs,
                        input_slots,
                    )?;
                    element_params.push(parsed_elem);
                }
                Ok(BlueprintParameter::FixedArray(element_params, size))
            }
            DynSolType::Array(ref inner_type) => {
                let value_seq = helpers::get_sequence(value_yaml, &value_path)?;

                // Recursively parse elements (dynamic length)
                let mut element_params = Vec::with_capacity(value_seq.len());
                for (i, elem_yaml) in value_seq.iter().enumerate() {
                    let elem_path = format!("{value_path}[{i}]");
                    let elem_map = helpers::get_mapping(elem_yaml, &elem_path)?; // Expect map {type: T, value: V}

                    // Validate inner type from element's map
                    let elem_type_str = helpers::get_string_field(elem_map, "type", &elem_path)?;
                    let elem_type = sol_types::parse_sol_type_str(elem_type_str)?;
                    if elem_type != **inner_type {
                        return Err(BlueprintParserError::TypeMismatch {
                            expected: inner_type.to_string(),
                            actual: elem_type.to_string(),
                        });
                    }

                    let parsed_elem = self.parse_parameter(
                        elem_map,
                        &elem_path,
                        returns,
                        constants,
                        inputs,
                        input_slots,
                    )?;
                    element_params.push(parsed_elem);
                }
                Ok(BlueprintParameter::DynamicArray(element_params))
            }
            // Scalar types (handle potential templates or direct values)
            _ => {
                // Get the optional description.
                let description =
                    helpers::get_optional_string_field(param_map, "description", param_path)?;

                // Not a template, parse as a direct scalar value
                Ok(BlueprintParameter::Scalar(BlueprintValue {
                    value: sol_types::parse_sol_value(&param_type, value_yaml, &value_path)?,
                    r#type: param_type, // The parsed scalar type
                    description: description.map(String::from),
                }))
            }
        }
    }

    /// Resolves a template string like "${source.name}" into a BlueprintParameter.
    /// Performs lookup and type checking.
    #[allow(clippy::too_many_arguments)]
    fn resolve_template_variable(
        &self,
        template_str: &str,
        expected_type: &DynSolType,
        description: Option<&str>,
        field_path: &str, // For error reporting
        returns: &HashMap<String, BlueprintReturn>,
        constants: &HashMap<String, YamlSolValue>,
        inputs: &HashMap<String, BlueprintInput>,
        input_slots: &IndexMap<String, BlueprintInputSlot>,
    ) -> BlueprintResult<BlueprintParameter> {
        let caps = self.template_regex.captures(template_str).ok_or_else(|| {
            // This shouldn't be hit if called correctly after checking is_match
            BlueprintParserError::InvalidTemplateVariable(format!(
                "Not a valid template format: {template_str}"
            ))
        })?;

        let source = caps.get(1).map_or("", |m| m.as_str());
        let name = caps.get(2).map_or("", |m| m.as_str());
        let field = caps.get(3).map(|m| m.as_str());

        match source {
            "returns" => {
                let return_def = returns.get(name).ok_or_else(|| {
                    BlueprintParserError::TemplateVariableNotFound {
                        source_path: "returns".to_string(),
                        variable: name.to_string(),
                    }
                })?;
                if return_def.r#type != *expected_type {
                    return Err(BlueprintParserError::TypeMismatch {
                        expected: expected_type.to_string(),
                        actual: return_def.r#type.to_string(),
                    });
                }

                Ok(BlueprintParameter::Return(name.to_string()))
            }
            "input_slots" => {
                let input_slot_name = if let Some(field_name) = field {
                    format!("{name}.{field_name}")
                } else {
                    name.to_string()
                };
                let slot_def = input_slots.get(&input_slot_name).ok_or_else(|| {
                    BlueprintParserError::TemplateVariableNotFound {
                        source_path: "input_slots".to_string(),
                        variable: input_slot_name.to_string(),
                    }
                })?;
                if slot_def.r#type != *expected_type {
                    return Err(BlueprintParserError::TypeMismatch {
                        expected: expected_type.to_string(),
                        actual: slot_def.r#type.to_string(),
                    });
                }

                Ok(BlueprintParameter::InputSlot(input_slot_name.to_string()))
            }
            "inputs" => {
                let input_def = inputs.get(name).ok_or_else(|| {
                    BlueprintParserError::TemplateVariableNotFound {
                        source_path: "inputs".to_string(),
                        variable: name.to_string(),
                    }
                })?;
                if input_def.r#type != *expected_type {
                    return Err(BlueprintParserError::TypeMismatch {
                        expected: expected_type.to_string(),
                        actual: input_def.r#type.to_string(),
                    });
                }
                Ok(BlueprintParameter::Input {
                    name: name.to_string(),
                })
            }
            "constants" => {
                let const_def = constants.get(name).ok_or_else(|| {
                    BlueprintParserError::TemplateVariableNotFound {
                        source_path: "constants".to_string(),
                        variable: name.to_string(),
                    }
                })?;
                if const_def.r#type != *expected_type {
                    return Err(BlueprintParserError::TypeMismatch {
                        expected: expected_type.to_string(),
                        actual: const_def.r#type.to_string(),
                    });
                }
                // Constants resolve directly to a Value(Scalar)
                Ok(BlueprintParameter::Scalar(BlueprintValue {
                    value: const_def.value.clone(),
                    r#type: const_def.r#type.clone(),
                    description: description.map(String::from),
                }))
            }
            _ => Err(BlueprintParserError::InvalidTemplateSource {
                source_path: source.to_string(),
                context: format!("parameter at {field_path}"),
            }),
        }
    }

    /// Resolves a template string for a reserved slot value.
    /// Handles 'returns' and 'constants' sources.
    /// Ensures the type matches the expected scalar type.
    #[allow(clippy::too_many_arguments)]
    fn resolve_reserved_slot_template(
        &self,
        template_str: &str,
        expected_type: &DynSolType, // Must be scalar, validated before calling
        field_path: &str,           // For error reporting
        description: Option<&str>,
        returns: &HashMap<String, BlueprintReturn>,
        constants: &HashMap<String, YamlSolValue>,
    ) -> BlueprintResult<BlueprintReservedSlot> {
        let caps = self.template_regex.captures(template_str).ok_or_else(|| {
            // Should not happen due to is_match check before calling
            BlueprintParserError::ReservedSlotValueMustBeTemplate {
                field_path: field_path.to_string(),
                value: template_str.to_string(),
            }
        })?;

        let source = caps.get(1).map_or("", |m| m.as_str());
        let name = caps.get(2).map_or("", |m| m.as_str());

        match source {
            "returns" => {
                let return_def = returns.get(name).ok_or_else(|| {
                    BlueprintParserError::TemplateVariableNotFound {
                        source_path: "returns".to_string(),
                        variable: name.to_string(),
                    }
                })?;
                // Check type match
                if return_def.r#type != *expected_type {
                    return Err(BlueprintParserError::TypeMismatch {
                        expected: expected_type.to_string(),
                        actual: return_def.r#type.to_string(),
                    });
                }

                // Return the reference name
                Ok(BlueprintReservedSlot::Return(name.to_string()))
            }
            "constants" => {
                let const_def = constants.get(name).ok_or_else(|| {
                    BlueprintParserError::TemplateVariableNotFound {
                        source_path: "constants".to_string(),
                        variable: name.to_string(),
                    }
                })?;
                // Check type match
                if const_def.r#type != *expected_type {
                    return Err(BlueprintParserError::TypeMismatch {
                        expected: expected_type.to_string(),
                        actual: const_def.r#type.to_string(),
                    });
                }

                // Return the concrete BlueprintValue
                Ok(BlueprintReservedSlot::Scalar(BlueprintValue {
                    value: const_def.value.clone(),
                    r#type: const_def.r#type.clone(),
                    description: description.map(String::from),
                }))
            }
            // Any other source (inputs, input_slots) is invalid for reserved slots
            _ => Err(BlueprintParserError::InvalidReservedSlotSource {
                field_path: field_path.to_string(),
                source_path: source.to_string(),
            }),
        }
    }
}
