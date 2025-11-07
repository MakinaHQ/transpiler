use std::{collections::HashMap, fs::canonicalize, path::PathBuf, str::FromStr};

use alloy::{
    dyn_abi::DynSolType,
    primitives::{Address, U256},
};
use regex::Regex;
use saphyr::{Mapping, Yaml};

use crate::{
    core::parser::{YamlInclude, helpers, sol_types},
    types::InstructionType,
};

use super::{
    errors::{PositionParserError, PositionParserResult},
    types::{Instruction, InstructionDefinition, Position, Root, SolValue},
};

pub struct PositionParser {
    root_path: PathBuf,
    template_regex: Regex,
}

impl PositionParser {
    /// Create a new position parser.
    pub fn new(root_path: PathBuf) -> Self {
        Self {
            root_path,
            template_regex: Regex::new(r"\$\{(config)\.([a-zA-Z0-9_]+)\}").unwrap(),
        }
    }

    /// Parse a position from a file.
    pub fn parse(&self) -> PositionParserResult<Root> {
        // Load the file and replace the builtins.
        let includes = YamlInclude::new(self.root_path.clone())?;
        let includes_fmt = format!("{includes}"); // This will parse the includes and output the final content.
        let content = helpers::replace_builtins(&includes_fmt)?;

        // Parse the YAML.
        let yaml = helpers::load_yaml_from_str(&content, true)
            .map_err(PositionParserError::FailedToLoadPosition)?;

        // Parse the YAML.
        self.parse_yaml(&yaml)
    }

    /// Parse positions from YAML.
    fn parse_yaml(&self, yaml: &Yaml) -> PositionParserResult<Root> {
        // Get the root map.
        let root_map = helpers::get_mapping(yaml, "root")?;

        // Parse the config and positions.
        let config = self.parse_config(root_map)?;

        // Parse the positions.
        let positions = self.parse_positions(root_map, &config)?;

        Ok(Root { config, positions })
    }

    /// Parse the config map.
    fn parse_config(&self, root_map: &Mapping) -> PositionParserResult<HashMap<String, SolValue>> {
        let config_map = helpers::get_mapping_field(root_map, "config", "root.config")?;

        let mut config = HashMap::new();
        for (key, value) in config_map.iter() {
            let key = helpers::get_string(key, "root.config")?;
            let sol_value = sol_types::parse_yaml_sol_value(value, "root.config")?;

            let description = helpers::get_optional_string_field(
                helpers::get_mapping(value, &format!("root.config.{key}"))?,
                "description",
                &format!("root.config.{key}"),
            )?;

            config.insert(
                key.to_string(),
                SolValue {
                    r#type: sol_value.r#type,
                    value: sol_value.value,
                    description: description.map(|s| s.to_string()),
                },
            );
        }

        Ok(config)
    }

    /// Parse the positions.
    fn parse_positions(
        &self,
        root_map: &Mapping,
        config: &HashMap<String, SolValue>,
    ) -> PositionParserResult<Vec<Position>> {
        let positions_seq = helpers::get_sequence_field(root_map, "positions", "root.positions")?;

        let mut positions = Vec::new();
        for (index, position) in positions_seq.iter().enumerate() {
            let position_map = helpers::get_mapping(position, &format!("root.positions[{index}]"))?;

            let id_str =
                helpers::get_string_field(position_map, "id", &format!("root.positions[{index}]"))?;
            let Ok(id) = U256::from_str(id_str) else {
                return Err(PositionParserError::TypeMismatch {
                    expected: "U256".into(),
                    actual: id_str.into(),
                });
            };

            let group_id_str = helpers::get_string_field(
                position_map,
                "group_id",
                &format!("root.positions[{index}]"),
            )?;
            let Ok(group_id) = U256::from_str(group_id_str) else {
                return Err(PositionParserError::TypeMismatch {
                    expected: "U256".into(),
                    actual: group_id_str.into(),
                });
            };

            let description = helpers::get_optional_string_field(
                helpers::get_mapping(position, &format!("root.positions[{index}]"))?,
                "description",
                &format!("root.positions[{index}]"),
            )?;

            let instructions = self.parse_instructions(position_map, config)?;

            let position_tags = self.parse_position_tags(position_map)?;

            positions.push(Position {
                id,
                group_id,
                description: description.map(|s| s.to_string()),
                instructions,
                global_tags: position_tags,
            });
        }

        Ok(positions)
    }

    /// Parse the instructions.
    fn parse_instructions(
        &self,
        position_map: &Mapping,
        configs: &HashMap<String, SolValue>,
    ) -> PositionParserResult<Vec<Instruction>> {
        let instructions_seq = helpers::get_sequence_field(
            position_map,
            "instructions",
            "root.positions.instructions",
        )?;

        let mut instructions = Vec::new();
        for (index, instruction) in instructions_seq.iter().enumerate() {
            let instruction_map = helpers::get_mapping(
                instruction,
                &format!("root.positions.instructions[{index}]"),
            )?;

            // Parse the is_debt flag.
            let is_debt = helpers::get_bool_field(
                instruction_map,
                "is_debt",
                &format!("root.positions.instructions[{index}]"),
            )?;

            // Parse the instruction type.
            let instruction_type = helpers::get_string_field(
                instruction_map,
                "instruction_type",
                &format!("root.positions.instructions[{index}]"),
            )?;

            let Ok(instruction_type) = InstructionType::from_str(instruction_type) else {
                return Err(PositionParserError::InvalidInstructionType(
                    instruction_type.to_string(),
                ));
            };

            // Parse the description.
            let description = helpers::get_optional_string_field(
                instruction_map,
                "description",
                &format!("root.positions.instructions[{index}]"),
            )?;

            // Parse the affected tokens.
            let affected_tokens = self.parse_affected_tokens(index, instruction_map)?;

            // Parse the instruction.
            let instruction = self.parse_instruction_dir(index, instruction_map, configs)?;

            instructions.push(Instruction {
                description: description.map(|s| s.to_string()),
                is_debt,
                instruction_type,
                affected_tokens,
                definition: instruction,
            });
        }

        Ok(instructions)
    }

    /// Gets all tags defined for this position
    fn parse_position_tags(
        &self,
        position_map: &Mapping,
    ) -> PositionParserResult<Vec<(String, String)>> {
        let Some(position_tags) =
            helpers::get_optional_sequence_field(position_map, "tags", "root.positions")?
        else {
            return Ok(Vec::new());
        };

        let mut result = Vec::new();
        for (index, elem) in position_tags.iter().enumerate() {
            let raw = helpers::get_string(elem, &format!("root.positions.tags[{index}]"))?;

            let (tag, action) = raw
                .split_once(':')
                .ok_or(PositionParserError::InvalidTag { raw: raw.into() })?;

            result.push((action.into(), tag.into()));
        }

        Ok(result)
    }

    /// Parse the instruction.
    fn parse_instruction_dir(
        &self,
        index: usize,
        instruction_map: &Mapping,
        configs: &HashMap<String, SolValue>,
    ) -> PositionParserResult<InstructionDefinition> {
        let instruction = helpers::get_mapping_field(
            instruction_map,
            "instruction",
            &format!("root.positions.instructions[{index}]"),
        )?;

        // Parse the override name, if any.
        let override_name = helpers::get_optional_string_field(
            instruction,
            "name",
            &format!("root.positions.instructions[{index}]"),
        )?;

        // Parse the label.
        let label = helpers::get_string_field(
            instruction,
            "label",
            &format!("root.positions.instructions[{index}]"),
        )?;

        // Parse the blueprint path.
        let path = helpers::get_string_field(
            instruction,
            "path",
            &format!("root.positions.instructions[{index}]"),
        )?;

        let (blueprint_path, action_name) = path
            .split_once(":")
            .ok_or(PositionParserError::InvalidPath {
                field_path: format!("root.positions.instructions[{index}].path"),
                value: path.to_string(),
            })
            .and_then(|(blueprint_path, action_name)| {
                let blueprint_path =
                    self.process_blueprint_path(index, &PathBuf::from(blueprint_path))?;

                Ok((blueprint_path, action_name.to_string()))
            })?;

        let inputs = self.parse_inputs(index, instruction, configs)?;

        Ok(InstructionDefinition {
            inputs,
            blueprint_path,
            label: label.to_string(),
            name: action_name.to_string(),
            override_name: override_name.map(|s| s.to_string()),
        })
    }

    /// Parse the inputs.
    fn parse_inputs(
        &self,
        index: usize,
        instruction: &Mapping,
        configs: &HashMap<String, SolValue>,
    ) -> PositionParserResult<HashMap<String, SolValue>> {
        let inputs_map = helpers::get_mapping_field(
            instruction,
            "inputs",
            &format!("root.positions.instructions[{index}]"),
        )?;

        let mut inputs = HashMap::new();
        for (key, value) in inputs_map.iter() {
            let key = helpers::get_string(key, &format!("root.positions.instructions[{index}]"))?;

            // Parse the input.
            let input_map = helpers::get_mapping(
                value,
                &format!("root.positions.instructions[{index}].{key}"),
            )?;

            let sol_value = self.parse_input(index, key, input_map, configs)?;

            inputs.insert(key.to_string(), sol_value);
        }

        Ok(inputs)
    }

    /// Parse a single input.
    fn parse_input(
        &self,
        index: usize,
        key: &str,
        input_map: &Mapping,
        configs: &HashMap<String, SolValue>,
    ) -> PositionParserResult<SolValue> {
        let type_yaml = helpers::get_string_field(
            input_map,
            "type",
            &format!("root.positions.instructions[{index}].{key}"),
        )?;

        let sol_type = sol_types::parse_sol_type_str(type_yaml)?;

        if matches!(
            sol_type,
            DynSolType::Array(_) | DynSolType::FixedArray(_, _) | DynSolType::Tuple(_)
        ) {
            return Err(PositionParserError::InputTypeMustBeScalar {
                field_path: format!("root.positions.instructions[{index}].{key}"),
                found_type: sol_type.to_string(),
            });
        }

        let description = helpers::get_optional_string_field(
            input_map,
            "description",
            &format!("root.positions.instructions[{index}].{key}"),
        )?;

        let value_str = helpers::get_string_field(
            input_map,
            "value",
            &format!("root.positions.instructions[{index}].{key}"),
        )?;

        if self.template_regex.is_match(value_str) {
            self.resolve_template_variable(value_str, &sol_type, description, configs)
        } else {
            let value_yaml = helpers::get_field(input_map, "value", "root.positions.instructions")?;

            let sol_value =
                sol_types::parse_sol_value(&sol_type, value_yaml, "root.positions.instructions")?;

            Ok(SolValue {
                r#type: sol_type,
                value: sol_value,
                description: description.map(|s| s.to_string()),
            })
        }
    }

    /// Resolve a template variable.
    fn resolve_template_variable(
        &self,
        value_str: &str,
        sol_type: &DynSolType,
        description: Option<&str>,
        configs: &HashMap<String, SolValue>,
    ) -> PositionParserResult<SolValue> {
        let caps = self.template_regex.captures(value_str).ok_or_else(|| {
            PositionParserError::InvalidTemplateVariable(format!(
                "Not a valid template format: {value_str}"
            ))
        })?;

        let source = caps.get(1).map_or("", |m| m.as_str());
        let name = caps.get(2).map_or("", |m| m.as_str());

        match source {
            "config" => {
                // Get the value from the config.
                let value = configs.get(name).ok_or_else(|| {
                    PositionParserError::TemplateVariableNotFound {
                        source_path: "config".to_string(),
                        variable: name.to_string(),
                    }
                })?;

                // Check if the type matches.
                if value.r#type != *sol_type {
                    return Err(PositionParserError::TypeMismatch {
                        expected: sol_type.to_string(),
                        actual: value.r#type.to_string(),
                    });
                }

                // Return the value.
                Ok(SolValue {
                    r#type: value.r#type.clone(),
                    value: value.value.clone(),
                    description: description.map(|s| s.to_string()),
                })
            }
            _ => Err(PositionParserError::InvalidTemplateSource {
                source_path: source.to_string(),
                context: "config".to_string(),
            }),
        }
    }

    /// Parse affected tokens list.
    fn parse_affected_tokens(
        &self,
        index: usize,
        instruction_map: &Mapping,
    ) -> PositionParserResult<Vec<Address>> {
        let affected_tokens_yaml = helpers::get_sequence_field(
            instruction_map,
            "affected_tokens",
            &format!("root.positions.instructions[{index}]"),
        )?;

        let mut affected_tokens = Vec::new();
        for (t_index, token) in affected_tokens_yaml.iter().enumerate() {
            let token_str = helpers::get_string(
                token,
                &format!("root.positions.instructions[{index}].affected_tokens[{t_index}]"),
            )?;

            let token_address = Address::from_str(token_str).map_err(|_| {
                PositionParserError::InvalidAddressFormat {
                    field_path: format!(
                        "root.positions.instructions[{index}].affected_tokens[{t_index}]"
                    ),
                    value: token_str.to_string(),
                }
            })?;

            affected_tokens.push(token_address);
        }

        Ok(affected_tokens)
    }

    /// Process the blueprint path.
    fn process_blueprint_path(
        &self,
        index: usize,
        blueprint_path: &PathBuf,
    ) -> PositionParserResult<PathBuf> {
        if blueprint_path.is_absolute() {
            return Ok(blueprint_path.clone());
        }

        let mut path = self.root_path.clone();

        path = path
            .parent()
            .ok_or(PositionParserError::InvalidPath {
                field_path: format!("root.positions.instructions[{index}].path"),
                value: blueprint_path.to_string_lossy().to_string(),
            })?
            .join(blueprint_path);

        if !path.is_file() {
            return Err(PositionParserError::InvalidPath {
                field_path: format!("root.positions.instructions[{index}].path"),
                value: blueprint_path.to_string_lossy().to_string(),
            });
        }

        let canonicalized = canonicalize(&path).map_err(|_| PositionParserError::InvalidPath {
            field_path: format!("root.positions.instructions[{index}].path"),
            value: blueprint_path.to_string_lossy().to_string(),
        })?;

        Ok(canonicalized)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_process_blueprint_path() {
        let parser = PositionParser::new(PathBuf::from("test_data/caliber.yaml"));

        let root = parser.parse().unwrap();

        println!("{root:?}");
    }

    #[test]
    fn test_invalid_tag() {
        let parser = PositionParser::new(PathBuf::from("test_data/caliber_invalid_tag.yaml"));

        let err = parser.parse().unwrap_err().to_string();
        err.find("Invalid tag").unwrap();
    }
}
