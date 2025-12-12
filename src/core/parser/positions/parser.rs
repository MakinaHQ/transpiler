use std::{
    collections::{BTreeMap, HashMap},
    fs::canonicalize,
    path::PathBuf,
    str::FromStr,
};

use alloy::{
    dyn_abi::DynSolType,
    primitives::{Address, U256},
};
use miette::{NamedSource, miette};
use regex::Regex;
use saphyr::{LoadableYamlNode, MarkedYaml, Marker};
use saphyr_parser::Span;

use crate::{
    core::parser::{
        YamlInclude,
        common::{Marked, Parser},
        helpers,
        positions::types::{InstructionTemplate, InstructionTemplateEnum},
        sol_types::{self, YamlSolValue},
    },
    token_list::{TokenInfo, TokenList},
    types::InstructionType,
};

use super::types::{Instruction, InstructionDefinition, Position, Root, SolValue};

#[derive(Debug)]
pub struct PositionParser {
    root_path: PathBuf,
    token_list: TokenList,
    _source: String,
    template_regex: Regex,
    source_with_includes: String,
}

impl PositionParser {
    /// Create a new position parser.
    pub fn new(root_path: PathBuf, token_list_path: Option<PathBuf>) -> miette::Result<Self> {
        let token_list = if let Some(path) = token_list_path {
            TokenList::new(path.clone())
                .map_err(|err| miette!("Could not load token list from {:?}: {}", path, err))?
        } else {
            TokenList::default()
        };

        let content = std::fs::read_to_string(&root_path)
            .map_err(|err| miette!("could not open blueprint: {}", err))?;

        let source = helpers::replace_builtins(&content)
            .map_err(|err| miette!("could not replace builtins: {err}"))?;

        // Load the source position file, and add instructions using `!include`.
        let includes = YamlInclude::new(root_path.clone())
            .map_err(|err| miette!("could not load includes: {}", err))?;
        let includes_fmt = format!("{includes}"); // This will parse the includes and output the final content.
        let content = helpers::replace_builtins(&includes_fmt)
            .map_err(|err| miette!("could not replace builtins: {}", err))?;

        Ok(Self {
            root_path,
            token_list,
            _source: source,
            template_regex: Regex::new(r"\$\{(\w+)\.(\w+)(?:\.(\w*))?\}").expect("is valid regex"),
            source_with_includes: content,
        })
    }

    /// Parse a position from a file.
    pub fn parse(&self) -> miette::Result<Root> {
        let marked_yaml = MarkedYaml::load_from_str(&self.source_with_includes)
            .map_err(|err| miette!("could not parse positions file: {err}"))?;

        // Parse the YAML.
        self.parse_yaml(&marked_yaml)
    }

    /// Parse positions from YAML.
    fn parse_yaml(&self, marked_yaml: &Vec<MarkedYaml>) -> miette::Result<Root> {
        let root = marked_yaml.first().expect("parsed.len() == 1");
        let config = self.config(root)?;
        let mut templates = config.clone();
        templates.append(&mut self.tokens().map_err(|err| miette!("{err}"))?);
        let positions = self.positions(root, &mut templates)?;
        let tokens_used: BTreeMap<String, TokenInfo> = templates
            .iter()
            .filter(|(_, v)| v.is_used)
            .filter_map(|(k, v)| v.as_token().map(|token| (k.clone(), token.clone())))
            .collect();
        Ok(Root {
            tokens: tokens_used,
            config,
            positions,
        })
    }

    fn tokens(&self) -> eyre::Result<BTreeMap<String, InstructionTemplate>> {
        let mut map = BTreeMap::new();

        for token in &self.token_list.tokens {
            let chain = token.chain;
            let symbol = &token.symbol;

            let key = format!("${{token_list.{chain}.{symbol}}}");

            if map.contains_key(&key) {
                return Err(eyre::eyre!(
                    "Duplicated token entry: \"{}\" - token entry is duplicated",
                    key
                ));
            }

            map.insert(
                key,
                InstructionTemplate::new(InstructionTemplateEnum::TokenInfo(token.clone())),
            );
        }

        Ok(map)
    }

    fn config<'a>(
        &self,
        root: &'a MarkedYaml<'a>,
    ) -> miette::Result<BTreeMap<String, InstructionTemplate>> {
        let Some(config) = root.data.as_mapping_get("config") else {
            return Ok(BTreeMap::new());
        };

        let mapping = config
            .data
            .as_mapping()
            .ok_or(self.error(config.span, "config must be mapping"))?;

        let mut map = BTreeMap::new();
        for (key, value) in mapping {
            let name = key
                .data
                .as_str()
                .ok_or(self.error(key.span, "Config has invalid name"))?;

            let key = format!("${{config.{name}}}");
            let value = self.sol_value(value)?;
            map.insert(
                key,
                InstructionTemplate::new(InstructionTemplateEnum::Config(value)),
            );
        }

        Ok(map)
    }

    fn positions<'a>(
        &self,
        root: &'a MarkedYaml<'a>,
        templates: &mut BTreeMap<String, InstructionTemplate>,
    ) -> miette::Result<Vec<Position>> {
        let positions = root
            .data
            .as_mapping_get("positions")
            .ok_or(self.error(root.span, "positions are required"))?;

        let positions_seq = positions
            .data
            .as_sequence()
            .ok_or(self.error(root.span, "positions must be a sequence"))?;

        let mut vec = Vec::new();
        for position in positions_seq {
            // Parse position-level variables and add them to the template context
            // Pass templates so position vars can reference token_list or config variables
            let position_vars = self.position_vars(position, templates)?;
            let position_var_keys: Vec<String> = position_vars.keys().cloned().collect();

            // Inject position vars into templates for this position
            templates.extend(position_vars);

            let instructions = self.instructions(position, templates)?;
            vec.push(Position {
                id: self.position_id(position)?,
                group_id: self.position_group_id(position)?,
                description: self.position_description(position)?,
                instructions,
                global_tags: self.position_tags(position)?,
            });

            // Remove position vars after processing this position
            // so they don't leak to the next position
            for key in position_var_keys {
                templates.remove(&key);
            }
        }
        Ok(vec)
    }

    /// Parse position-level variables from the `vars` field.
    /// Type is inferred from the instruction where the variable is used.
    ///
    /// Supports:
    /// - Raw values: `token_address: "0x..."`
    /// - Token list references: `token_address: "${token_list.mainnet.USDC}"`
    ///
    /// Example:
    /// ```yaml
    /// vars:
    ///   token_address: "${token_list.mainnet.USDC}"
    ///   vault_label: "My Vault"
    /// ```
    fn position_vars<'a>(
        &self,
        position: &'a MarkedYaml<'a>,
        templates: &mut BTreeMap<String, InstructionTemplate>,
    ) -> miette::Result<BTreeMap<String, InstructionTemplate>> {
        let Some(vars) = position.data.as_mapping_get("vars") else {
            return Ok(BTreeMap::new());
        };

        let mapping = vars
            .data
            .as_mapping()
            .ok_or(self.error(vars.span, "position vars must be a mapping"))?;

        let mut map = BTreeMap::new();
        for (key, value) in mapping {
            let name = key
                .data
                .as_str()
                .ok_or(self.error(key.span, "position var has invalid name"))?;

            let raw_str = value
                .data
                .as_str()
                .ok_or(self.error(value.span, "position var value must be a string"))?;

            let template_key = format!("${{position.{name}}}");

            // Check if this is a template variable reference (e.g., ${token_list.mainnet.USDC})
            if self.is_template(value) {
                // Look up the template (e.g., token_list or config)
                if let Some(template) = templates.get_mut(raw_str) {
                    // Mark the original template as used (for tracking in root.tokens)
                    template.is_used = true;
                    map.insert(template_key, template.clone());
                } else {
                    return Err(
                        self.error(value.span, "unknown template variable in position var")
                    )?;
                }
            } else {
                // Raw value: store as PositionVar with type inferred at usage
                map.insert(
                    template_key,
                    InstructionTemplate::new(InstructionTemplateEnum::PositionVar(
                        raw_str.to_string(),
                    )),
                );
            }
        }

        Ok(map)
    }

    fn instruction_description<'a>(
        &self,
        instruction: &'a MarkedYaml<'a>,
    ) -> miette::Result<Option<String>> {
        let description = instruction.data.as_mapping_get("description");

        if let Some(desc) = description {
            let desc_str = desc
                .data
                .as_str()
                .ok_or(self.error(desc.span, "instruction description must be a string"))?;
            Ok(Some(desc_str.to_string()))
        } else {
            Ok(None)
        }
    }

    fn instruction_is_debt<'a>(&self, instruction: &'a MarkedYaml<'a>) -> miette::Result<bool> {
        let is_debt = instruction
            .data
            .as_mapping_get("is_debt")
            .ok_or(self.error(instruction.span, "is_debt boolean flag is required"))?;

        let is_debt_bool = is_debt
            .data
            .as_bool()
            .ok_or(self.error(is_debt.span, "is_debt must be a boolean"))?;
        Ok(is_debt_bool)
    }

    fn instruction_type<'a>(
        &self,
        instruction: &'a MarkedYaml<'a>,
    ) -> miette::Result<InstructionType> {
        let instr_type = instruction
            .data
            .as_mapping_get("instruction_type")
            .ok_or(self.error(instruction.span, "instruction_type is required"))?;

        let instr_type_str = instr_type
            .data
            .as_str()
            .ok_or(self.error(instr_type.span, "instruction_type must be a string"))?;

        let instruction_type = InstructionType::from_str(instr_type_str).map_err(|_| {
            self.error(
                instr_type.span,
                "instruction_type must be a valid InstructionType",
            )
        })?;

        Ok(instruction_type)
    }

    fn instruction_affected_tokens<'a>(
        &self,
        instruction: &'a MarkedYaml<'a>,
        templates: &mut BTreeMap<String, InstructionTemplate>,
    ) -> miette::Result<Vec<Address>> {
        let affected_tokens = instruction
            .data
            .as_mapping_get("affected_tokens")
            .ok_or(self.error(instruction.span, "affected_tokens is required"))?;

        let tokens_seq = affected_tokens
            .data
            .as_sequence()
            .ok_or(self.error(affected_tokens.span, "affected_tokens must be a sequence"))?;

        let mut tokens = Vec::new();
        for token in tokens_seq {
            let parsed = self.parse_template(token, templates, true)?;

            if parsed.as_type() != &DynSolType::Address {
                return Err(self.error(
                    token.span,
                    "affected_token template must resolve to an Address",
                ))?;
            }

            let affected_token = match parsed.template {
                InstructionTemplateEnum::Config(ref config) => {
                    config.value.as_address().expect("already checked the type")
                }
                InstructionTemplateEnum::Raw(ref raw) => {
                    raw.value.as_address().expect("already checked the type")
                }
                InstructionTemplateEnum::TokenInfo(ref token_info) => token_info.address,
                InstructionTemplateEnum::PositionVar(_) => {
                    unreachable!("PositionVar is resolved to Raw in parse_template")
                }
            };

            tokens.push(affected_token);
        }

        Ok(tokens)
    }

    fn instruction_definition_path_and_name<'a>(
        &self,
        instruction: &'a MarkedYaml<'a>,
    ) -> miette::Result<(PathBuf, String)> {
        let path = instruction
            .data
            .as_mapping_get("path")
            .ok_or(self.error(instruction.span, "instruction path is required"))?;

        let path_str = path
            .data
            .as_str()
            .ok_or(self.error(path.span, "instruction path must be a string"))?;

        let (blueprint_path_str, name) = path_str.split_once(":").ok_or(self.error(
            path.span,
            "instruction path must be in format 'path:action_name'",
        ))?;

        // break path span into blueprint path span and action name span
        let path_span = Span {
            start: path.span.start,
            end: Marker::new(
                path.span.start.index() + blueprint_path_str.len() + 1,
                path.span.start.line(),
                path.span.start.col() + blueprint_path_str.len() + 1,
            ),
        };

        let blueprint_path =
            self.process_blueprint_path(&PathBuf::from(blueprint_path_str), path_span)?;

        Ok((blueprint_path, name.to_string()))
    }

    fn instruction_label<'a>(
        &self,
        instruction: &'a MarkedYaml<'a>,
        templates: &mut BTreeMap<String, InstructionTemplate>,
    ) -> miette::Result<String> {
        let label = instruction
            .data
            .as_mapping_get("label")
            .ok_or(self.error(instruction.span, "instruction label is required"))?;

        let label_str = label
            .data
            .as_str()
            .ok_or(self.error(label.span, "instruction label must be a string"))?;

        // Check if the label is a template variable
        if self.is_template(label) {
            let template = self.parse_template_str(label)?;
            if !["config", "token_list", "position"].contains(&*template.source) {
                return Err(self.error(template.source, "unknown source"))?;
            }

            let value = templates
                .get_mut(label_str)
                .ok_or_else(|| self.error(label.span, "unknown template variable"))?;

            // Mark the template as used
            value.is_used = true;

            // Extract the string value from the template
            let resolved = match &value.template {
                InstructionTemplateEnum::Config(config) => {
                    config.value.as_str().ok_or_else(|| {
                        self.error(label.span, "label template must resolve to a string")
                    })?
                }
                InstructionTemplateEnum::PositionVar(raw_str) => raw_str.as_str(),
                InstructionTemplateEnum::Raw(raw) => raw.value.as_str().ok_or_else(|| {
                    self.error(label.span, "label template must resolve to a string")
                })?,
                InstructionTemplateEnum::TokenInfo(_) => {
                    return Err(self.error(label.span, "token_list cannot be used for labels"))?;
                }
            };

            Ok(resolved.to_string())
        } else {
            Ok(label_str.to_string())
        }
    }

    fn instruction_input<'a>(
        &self,
        input: &'a MarkedYaml<'a>,
        templates: &mut BTreeMap<String, InstructionTemplate>,
    ) -> miette::Result<SolValue> {
        let parsed = self.parse_template(input, templates, false)?;

        let type_span = input
            .data
            .as_mapping_get("type")
            .ok_or(self.error(input.span, "instruction input type is required"))?
            .span;

        // Ensure it is a scalar type
        if parsed.as_type().is_dynamic() {
            return Err(self.error(type_span, "dynamic types not supported"))?;
        }

        let value = match parsed.template.clone() {
            InstructionTemplateEnum::Config(config) => config.value,
            InstructionTemplateEnum::Raw(raw) => raw.value,
            InstructionTemplateEnum::TokenInfo(token_info) => token_info.address.into(),
            InstructionTemplateEnum::PositionVar(_) => {
                unreachable!("PositionVar is resolved to Raw in parse_template")
            }
        };

        Ok(SolValue {
            r#type: parsed.as_type().clone(),
            value,
            description: None,
        })
    }

    fn instruction_inputs<'a>(
        &self,
        instruction: &'a MarkedYaml<'a>,
        templates: &mut BTreeMap<String, InstructionTemplate>,
    ) -> miette::Result<HashMap<String, SolValue>> {
        let inputs = instruction
            .data
            .as_mapping_get("inputs")
            .ok_or(self.error(instruction.span, "instruction inputs are required"))?;

        let inputs_map = inputs
            .data
            .as_mapping()
            .ok_or(self.error(inputs.span, "instruction inputs must be a mapping"))?;

        let mut map = HashMap::new();
        for (key, value) in inputs_map {
            let input = self.instruction_input(value, templates)?;
            let key_str = key
                .data
                .as_str()
                .ok_or(self.error(key.span, "instruction input key must be a string"))?;
            map.insert(key_str.to_string(), input);
        }

        Ok(map)
    }

    fn instruction_definition<'a>(
        &self,
        instruction: &'a MarkedYaml<'a>,
        templates: &mut BTreeMap<String, InstructionTemplate>,
    ) -> miette::Result<InstructionDefinition> {
        let instruction_definition = instruction
            .data
            .as_mapping_get("instruction")
            .ok_or(self.error(instruction.span, "instruction definition is required"))?;

        let (blueprint_path, name) =
            self.instruction_definition_path_and_name(instruction_definition)?;

        let label = self.instruction_label(instruction_definition, templates)?;

        let inputs = self.instruction_inputs(instruction_definition, templates)?;

        Ok(InstructionDefinition {
            blueprint_path,
            name,
            label,
            override_name: None,
            inputs,
        })
    }

    fn instruction<'a>(
        &self,
        instruction: &'a MarkedYaml<'a>,
        templates: &mut BTreeMap<String, InstructionTemplate>,
    ) -> miette::Result<Instruction> {
        let description = self.instruction_description(instruction)?;
        let is_debt = self.instruction_is_debt(instruction)?;
        let instruction_type = self.instruction_type(instruction)?;
        let affected_tokens = self.instruction_affected_tokens(instruction, templates)?;
        let instruction_definition = self.instruction_definition(instruction, templates)?;

        Ok(Instruction {
            description,
            is_debt,
            instruction_type,
            affected_tokens,
            definition: instruction_definition,
        })
    }

    fn instructions<'a>(
        &self,
        position: &'a MarkedYaml<'a>,
        templates: &mut BTreeMap<String, InstructionTemplate>,
    ) -> miette::Result<Vec<Instruction>> {
        let instructions = position
            .data
            .as_mapping_get("instructions")
            .ok_or(self.error(position.span, "instructions are required"))?;

        let instructions_seq = instructions
            .data
            .as_sequence()
            .ok_or(self.error(instructions.span, "instructions must be a sequence"))?;

        let mut vec = Vec::new();
        for instruction in instructions_seq {
            let instr = self.instruction(instruction, templates)?;
            vec.push(instr);
        }

        Ok(vec)
    }

    fn position_id<'a>(&self, position: &'a MarkedYaml<'a>) -> miette::Result<U256> {
        let id = position
            .data
            .as_mapping_get("id")
            .ok_or(self.error(position.span, "position id is required"))?;

        let id_str = id
            .data
            .as_str()
            .ok_or(self.error(id.span, "position id must be a string"))?;

        let id = U256::from_str(id_str)
            .map_err(|_| self.error(id.span, "position id must be a valid U256 string"))?;

        Ok(id)
    }

    fn position_group_id<'a>(&self, position: &'a MarkedYaml<'a>) -> miette::Result<U256> {
        let group_id = position
            .data
            .as_mapping_get("group_id")
            .ok_or(self.error(position.span, "position group_id is required"))?;

        let group_id_str = group_id
            .data
            .as_str()
            .ok_or(self.error(group_id.span, "position group_id must be a string"))?;

        let group_id = U256::from_str(group_id_str).map_err(|_| {
            self.error(
                group_id.span,
                "position group_id must be a valid U256 string",
            )
        })?;

        Ok(group_id)
    }

    fn position_description<'a>(
        &self,
        position: &'a MarkedYaml<'a>,
    ) -> miette::Result<Option<String>> {
        let description = position.data.as_mapping_get("description");

        if let Some(desc) = description {
            let desc_str = desc
                .data
                .as_str()
                .ok_or(self.error(desc.span, "position description must be a string"))?;
            Ok(Some(desc_str.to_string()))
        } else {
            Ok(None)
        }
    }

    fn position_tags<'a>(
        &self,
        position: &'a MarkedYaml<'a>,
    ) -> miette::Result<Vec<(String, String)>> {
        let tags = position.data.as_mapping_get("tags");

        let mut vec = Vec::new();
        if let Some(tags) = tags {
            let tags_seq = tags
                .data
                .as_sequence()
                .ok_or(self.error(tags.span, "position tags must be a sequence"))?;

            for tag in tags_seq {
                let tag_str = tag
                    .data
                    .as_str()
                    .ok_or(self.error(tag.span, "position tag must be a string"))?;

                let (tag, action) = tag_str
                    .split_once(':')
                    .ok_or(self.error(tag.span, "invalid tag format, expected 'tag:action'"))?;

                vec.push((action.to_string(), tag.to_string()));
            }
        }

        Ok(vec)
    }

    /// Process the blueprint path.
    fn process_blueprint_path(
        &self,
        blueprint_path: &PathBuf,
        span: Span,
    ) -> miette::Result<PathBuf> {
        if blueprint_path.is_absolute() {
            return Ok(blueprint_path.clone());
        }

        let mut path = self.root_path.clone();

        path = path
            .parent()
            .ok_or(self.error(span, "invalid path"))?
            .join(blueprint_path);

        if !path.is_file() {
            return Err(self.error(span, "file does not exist"))?;
        }

        let canonicalized = canonicalize(&path).map_err(|err| self.error(span, err.to_string()))?;

        Ok(canonicalized)
    }

    /// Parses a field that can hold a raw value or a template variable
    /// Expects `field` to hold `type` and `value` as direct children
    /// or `value` directly if `is_affected_token` is true, expecting type Address
    fn parse_template<'a>(
        &self,
        field: &'a MarkedYaml<'a>,
        templates: &mut BTreeMap<String, InstructionTemplate>,
        is_affected_token: bool,
    ) -> miette::Result<InstructionTemplate> {
        let (value_field, r#type) = if let Some(value_field) = field.data.as_mapping_get("value") {
            let r#type = self.get_type(field, "type")?;
            (value_field, r#type)
        } else if is_affected_token {
            let r#type = Marked::new(DynSolType::Address, field.span);
            (field, r#type)
        } else {
            return Err(self.error(field.span, "unsupported field format"))?;
        };

        debug_assert!(!field.is_sequence());

        // if value is not a template string
        // try parsing as raw value and return the result
        if !self.is_template(value_field) {
            let value =
                sol_types::parse_sol_value_marked(&r#type, value_field).ok_or_else(|| {
                    self.error(value_field, "could not be parsed...")
                        .with_second_location(r#type.span, "...into specified type")
                })?;

            return Ok(InstructionTemplate::new(InstructionTemplateEnum::Raw(
                YamlSolValue {
                    r#type: r#type.inner,
                    value,
                },
            )));
        }

        let template = self.parse_template_str(value_field)?;
        if !["config", "token_list", "position"].contains(&*template.source) {
            return Err(self.error(template.source, "unknown source"))?;
        }

        let key: &str = value_field
            .data
            .as_str()
            .expect("already parsed as template");

        let value = templates
            .get_mut(key)
            .ok_or_else(|| self.error(value_field.span, "unknown template variable"))?;

        // Handle PositionVar: parse the raw string using the expected type from the instruction
        if let InstructionTemplateEnum::PositionVar(raw_str) = &value.template {
            use saphyr::Yaml;
            let yaml_value = Yaml::scalar_from_string(raw_str.clone());
            let parsed_value =
                sol_types::parse_sol_value(&r#type.inner, &yaml_value, "position var").map_err(
                    |_| {
                        self.error(value_field, "could not be parsed...")
                            .with_second_location(r#type.span, "...into specified type")
                    },
                )?;

            // Mark as used
            value.is_used = true;

            // Return as Raw since we've now resolved the type
            return Ok(InstructionTemplate::new(InstructionTemplateEnum::Raw(
                YamlSolValue {
                    r#type: r#type.inner,
                    value: parsed_value,
                },
            )));
        }

        if value.as_type() != &r#type.inner {
            return Err(self
                .error(value_field, "has unexpected type")
                .with_second_location(r#type.span, "expected type"))?;
        }

        // mark the template as used
        value.is_used = true;

        Ok(value.clone())
    }
}

impl Parser for PositionParser {
    fn named_source(&self) -> NamedSource<String> {
        NamedSource::new(
            self.root_path.to_string_lossy(),
            self.source_with_includes.clone(),
        )
    }

    fn template_regex(&self) -> &Regex {
        &self.template_regex
    }
}

#[cfg(test)]
mod tests {
    use alloy::primitives::address;

    use super::*;

    #[test]
    fn test_process_blueprint_path() {
        let parser = PositionParser::new(PathBuf::from("test_data/caliber.yaml"), None).unwrap();

        let root = parser.parse().unwrap();

        println!("{root:?}");
    }

    #[test]
    fn test_invalid_tag() {
        let parser =
            PositionParser::new(PathBuf::from("test_data/caliber_invalid_tag.yaml"), None).unwrap();

        // expect revert
        parser.parse().unwrap_err();
    }

    #[test]
    fn test_position_vars() {
        let parser = PositionParser::new(
            PathBuf::from("test_data/caliber_with_position_vars.yaml"),
            None,
        )
        .unwrap();

        let root = parser.parse().unwrap();

        // Should have 2 positions
        assert_eq!(root.positions.len(), 2);

        // Position 1: WETH vault
        let pos1 = &root.positions[0];
        assert_eq!(
            pos1.id,
            U256::from_str("241963968685402655309198654686298022078").unwrap()
        );
        assert_eq!(pos1.instructions.len(), 2);

        // Check that the position vars were correctly substituted
        let deposit_instr = &pos1.instructions[0];
        assert_eq!(deposit_instr.definition.label, "weth");
        assert_eq!(
            deposit_instr.affected_tokens[0],
            address!("0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2")
        );

        // Check the input was substituted with position var
        let token_input = deposit_instr.definition.inputs.get("token").unwrap();
        assert_eq!(
            token_input.value.as_address().unwrap(),
            address!("0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2")
        );

        // Check aave_token_address was substituted correctly in accounting instruction
        let account_instr = &pos1.instructions[1];
        assert_eq!(account_instr.definition.label, "weth");
        let aave_token_input = account_instr.definition.inputs.get("aave_token").unwrap();
        assert_eq!(
            aave_token_input.value.as_address().unwrap(),
            address!("0x4d5F47FA6A74757f35C14fD3a6Ef8E3C9BC514E8")
        );

        // Position 2: USDC vault
        let pos2 = &root.positions[1];
        assert_eq!(
            pos2.id,
            U256::from_str("123456789012345678901234567890123456789").unwrap()
        );
        assert_eq!(pos2.instructions.len(), 2);

        // Check that the position vars were correctly substituted for position 2
        let deposit_instr2 = &pos2.instructions[0];
        assert_eq!(deposit_instr2.definition.label, "usdc");
        assert_eq!(
            deposit_instr2.affected_tokens[0],
            address!("0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48")
        );

        // Check the input was substituted with position var
        let token_input2 = deposit_instr2.definition.inputs.get("token").unwrap();
        assert_eq!(
            token_input2.value.as_address().unwrap(),
            address!("0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48")
        );

        // Check aave_token_address was substituted correctly for position 2
        let account_instr2 = &pos2.instructions[1];
        assert_eq!(account_instr2.definition.label, "usdc");
        let aave_token_input2 = account_instr2.definition.inputs.get("aave_token").unwrap();
        assert_eq!(
            aave_token_input2.value.as_address().unwrap(),
            address!("0x98C23E9d8f34FEFb1B7BD6a91B7FF122F4e16F5c")
        );
    }

    #[test]
    fn test_position_vars_isolation() {
        // This test ensures position vars don't leak between positions
        let parser = PositionParser::new(
            PathBuf::from("test_data/caliber_with_position_vars.yaml"),
            None,
        )
        .unwrap();

        let root = parser.parse().unwrap();

        // Position 1 has WETH token address
        let pos1_token = root.positions[0].instructions[0]
            .definition
            .inputs
            .get("token")
            .unwrap();
        // Position 2 has USDC token address
        let pos2_token = root.positions[1].instructions[0]
            .definition
            .inputs
            .get("token")
            .unwrap();

        // They should be different (position vars should not leak)
        assert_ne!(
            pos1_token.value.as_address().unwrap(),
            pos2_token.value.as_address().unwrap()
        );
    }

    #[test]
    fn test_position_vars_with_token_list() {
        // Test that position vars can reference token_list variables
        let parser = PositionParser::new(
            PathBuf::from("test_data/caliber_with_token_list_vars.yaml"),
            Some(PathBuf::from("test_data/token_lists/test.json")),
        )
        .unwrap();

        let root = parser.parse().unwrap();

        // Should have 2 positions
        assert_eq!(root.positions.len(), 2);

        // Position 1: DUSD vault (token from token_list)
        let pos1 = &root.positions[0];
        let deposit_instr = &pos1.instructions[0];

        // Check that token_address was resolved from token_list
        assert_eq!(
            deposit_instr.affected_tokens[0],
            address!("0x1e33E98aF620F1D563fcD3cfd3C75acE841204ef") // DUSD address
        );

        // Check the input was substituted with token_list value
        let token_input = deposit_instr.definition.inputs.get("token").unwrap();
        assert_eq!(
            token_input.value.as_address().unwrap(),
            address!("0x1e33E98aF620F1D563fcD3cfd3C75acE841204ef") // DUSD address
        );

        // Position 2: DETH vault (token from token_list)
        let pos2 = &root.positions[1];
        let deposit_instr2 = &pos2.instructions[0];

        // Check that token_address was resolved from token_list
        assert_eq!(
            deposit_instr2.affected_tokens[0],
            address!("0x871aB8E36CaE9AF35c6A3488B049965233DeB7ed") // DETH address
        );

        // Check the input was substituted with token_list value
        let token_input2 = deposit_instr2.definition.inputs.get("token").unwrap();
        assert_eq!(
            token_input2.value.as_address().unwrap(),
            address!("0x871aB8E36CaE9AF35c6A3488B049965233DeB7ed") // DETH address
        );

        // Verify that tokens from token_list are included in the output
        assert!(root.tokens.contains_key("${token_list.mainnet.DUSD}"));
        assert!(root.tokens.contains_key("${token_list.mainnet.DETH}"));
    }
}
