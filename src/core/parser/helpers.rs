use regex::Regex;
use saphyr::{Mapping, Sequence, Yaml, YamlLoader};
use saphyr_parser::{BufferedInput, Parser};

use std::{fs::File, io::read_to_string, path::PathBuf};

use super::errors::{ParserError, ParserResult};

pub fn load_yaml_from_str<'a>(s: &str, early: bool) -> ParserResult<Yaml<'a>> {
    let iter = s.chars();
    let mut parser = Parser::new(BufferedInput::new(iter));
    let mut loader: YamlLoader<Yaml> = YamlLoader::default();
    loader.early_parse(early);

    parser.load(&mut loader, false)?;

    let docs = loader.into_documents();
    let doc = docs[0].clone();

    Ok(doc)
}

pub fn load_yaml<'a>(file_path: PathBuf, early: bool) -> ParserResult<Yaml<'a>> {
    // Read the file.
    let file = File::open(file_path)?;
    let s = read_to_string(file)?;

    load_yaml_from_str(&s, early)
}

pub fn get_builtin(name: &str) -> Option<String> {
    match name {
        "UINT256_MAX" => {
            Some("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff".to_string())
        }
        "UINT256_0" => Some("0".to_string()),
        "UINT256_1" => Some("1".to_string()),
        "ADDRESS_0" => Some("0x0000000000000000000000000000000000000000".to_string()),
        "UINT128_MAX" => Some("0xffffffffffffffffffffffffffffffff".to_string()),
        _ => None,
    }
}

/// Replace the builtins with their values.
pub fn replace_builtins(content: &str) -> ParserResult<String> {
    let template_regex = Regex::new(r"\$\{(builtins)\.([a-zA-Z0-9_]+)\}").unwrap();

    // Check if all used builtins are valid.
    for cap in template_regex.captures_iter(content) {
        if let (Some(source), Some(name)) = (cap.get(1), cap.get(2))
            && source.as_str() == "builtins"
            && get_builtin(name.as_str()).is_none()
        {
            return Err(ParserError::InvalidBuiltin(name.as_str().to_string()));
        }
    }

    // Perform the replacement.
    let result = template_regex.replace_all(content, |caps: &regex::Captures| {
        match caps.get(1).map(|m| m.as_str()) {
            Some("builtins") => {
                get_builtin(caps.get(2).map(|m| m.as_str()).unwrap()).unwrap_or_default()
            }
            _ => caps
                .get(0)
                .map(|m| m.as_str())
                .unwrap_or_default()
                .to_string(),
        }
    });

    Ok(result.to_string())
}

// --- YAML Helper Functions ---

/// Get a mapping from a YAML node.
pub fn get_mapping<'a>(yaml: &'a Yaml, field_path: &str) -> ParserResult<&'a Mapping<'a>> {
    yaml.as_mapping()
        .ok_or_else(|| ParserError::InvalidYamlFormat {
            field_path: field_path.to_string(),
            expected_type: "mapping".to_string(),
            found_yaml: format!("{yaml:?}"),
        })
}

/// Get a sequence from a YAML node.
pub fn get_sequence<'a>(yaml: &'a Yaml, field_path: &str) -> ParserResult<&'a Sequence<'a>> {
    yaml.as_sequence()
        .ok_or_else(|| ParserError::InvalidYamlFormat {
            field_path: field_path.to_string(),
            expected_type: "sequence".to_string(),
            found_yaml: format!("{yaml:?}"),
        })
}

/// Get a string from a YAML node.
pub fn get_string<'a>(yaml: &'a Yaml, field_path: &str) -> ParserResult<&'a str> {
    yaml.as_str().ok_or_else(|| ParserError::InvalidYamlFormat {
        field_path: field_path.to_string(),
        expected_type: "string".to_string(),
        found_yaml: format!("{yaml:?}"),
    })
}

/// Get a boolean from a YAML node.
pub fn get_bool(yaml: &Yaml, field_path: &str) -> ParserResult<bool> {
    yaml.as_bool()
        .ok_or_else(|| ParserError::InvalidYamlFormat {
            field_path: field_path.to_string(),
            expected_type: "boolean".to_string(),
            found_yaml: format!("{yaml:?}"),
        })
}

/// Get a field from a YAML mapping.
pub fn get_field<'a>(
    yaml_map: &'a Mapping<'a>,
    key: &str,
    field_path: &str, // Path up to this map
) -> ParserResult<&'a Yaml<'a>> {
    yaml_map
        .get(&Yaml::scalar_from_string(key.to_string()))
        .ok_or_else(|| ParserError::MissingRequiredField {
            field_path: format!("{field_path}.{key}"),
        })
}

/// Get a mapping from a YAML node.
pub fn get_mapping_field<'a>(
    yaml_map: &'a Mapping<'a>,
    key: &str,
    field_path: &str,
) -> ParserResult<&'a Mapping<'a>> {
    let field = get_field(yaml_map, key, field_path)?;
    get_mapping(field, &format!("{field_path}.{key}"))
}

/// Get an optional mapping from a YAML node.
pub fn get_optional_mapping_field<'a>(
    yaml_map: &'a Mapping<'a>,
    key: &str,
    field_path: &str,
) -> ParserResult<Option<&'a Mapping<'a>>> {
    match yaml_map.get(&Yaml::scalar_from_string(key.to_string())) {
        Some(field) => get_mapping(field, &format!("{field_path}.{key}")).map(Some),
        None => Ok(None),
    }
}

/// Get a sequence from a YAML node.
pub fn get_sequence_field<'a>(
    yaml_map: &'a Mapping<'a>,
    key: &str,
    field_path: &str,
) -> ParserResult<&'a Sequence<'a>> {
    let field = get_field(yaml_map, key, field_path)?;
    get_sequence(field, &format!("{field_path}.{key}"))
}

/// Get an optional sequence from a YAML node.
pub fn get_optional_sequence_field<'a>(
    yaml_map: &'a Mapping<'a>,
    key: &str,
    field_path: &str,
) -> ParserResult<Option<&'a Sequence<'a>>> {
    match yaml_map.get(&Yaml::scalar_from_string(key.to_string())) {
        Some(field) => get_sequence(field, &format!("{field_path}.{key}")).map(Some),
        None => Ok(None),
    }
}

/// Get a string field from a YAML mapping.
pub fn get_string_field<'a>(
    yaml_map: &'a Mapping<'a>,
    key: &str,
    field_path: &str,
) -> ParserResult<&'a str> {
    let field = get_field(yaml_map, key, field_path)?;
    get_string(field, &format!("{field_path}.{key}"))
}

/// Get a boolean field from a YAML mapping.
pub fn get_bool_field<'a>(
    yaml_map: &'a Mapping<'a>,
    key: &str,
    field_path: &str,
) -> ParserResult<bool> {
    let field = get_field(yaml_map, key, field_path)?;
    get_bool(field, &format!("{field_path}.{key}"))
}

/// Get an optional string field from a YAML mapping.
pub fn get_optional_string_field<'a>(
    yaml_map: &'a Mapping<'a>,
    key: &str,
    field_path: &str,
) -> ParserResult<Option<&'a str>> {
    match yaml_map.get(&Yaml::scalar_from_string(key.to_string())) {
        Some(field) => get_string(field, &format!("{field_path}.{key}")).map(Some),
        None => Ok(None),
    }
}

/// Get an optional boolean field from a YAML mapping.
pub fn get_optional_bool_field<'a>(
    yaml_map: &'a Mapping<'a>,
    key: &str,
    field_path: &str,
) -> ParserResult<Option<bool>> {
    match yaml_map.get(&Yaml::scalar_from_string(key.to_string())) {
        Some(field) => get_bool(field, &format!("{field_path}.{key}")).map(Some),
        None => Ok(None),
    }
}
