use saphyr::{Mapping, Yaml, YamlEmitter};

use std::{collections::HashSet, fmt, fs::canonicalize, path::PathBuf};

use super::errors::ParserError;
use super::errors::ParserResult;
use super::helpers::load_yaml;

/// Processing YAML files with including other YAML documents through the `!include` tag.
#[derive(Debug, Clone)]
pub struct YamlInclude {
    root_path: PathBuf,
    seen_paths: HashSet<PathBuf>,
}

impl YamlInclude {
    /// Create a new preprocessor for the given root path.
    pub fn new(root_path: PathBuf) -> ParserResult<Self> {
        Self::new_node(root_path, None)
    }

    /// Create a new preprocessor for the given root path.
    ///
    /// If the `seen_paths` is provided, it will be used to check for circular references.
    fn new_node(root_path: PathBuf, seen_paths: Option<HashSet<PathBuf>>) -> ParserResult<Self> {
        // Initialize the set of seen paths.
        let mut seen_paths = seen_paths.unwrap_or_default();

        // Normalize the path.
        let normalized_path = canonicalize(&root_path)?;

        // Check for circular references.
        if seen_paths.contains(&normalized_path) {
            return Err(ParserError::CircularInclude(
                root_path.to_string_lossy().to_string(),
            ));
        }

        // Add the path to the set of seen paths.
        seen_paths.insert(normalized_path);

        // Return the preprocessor.
        Ok(Self {
            root_path,
            seen_paths,
        })
    }

    pub fn parse<'a>(&self) -> ParserResult<Yaml<'a>> {
        // Load the file.
        let file_path = self.root_path.clone();
        let doc = load_yaml(file_path, false)?;

        // Process the document.
        self.clone().recursive_parse(doc)
    }

    fn recursive_parse(self, input: Yaml) -> ParserResult<Yaml> {
        // Start the processing of the document.
        match input {
            // If the input is a sequence, process each item.
            Yaml::Sequence(sequence) => {
                let results: ParserResult<Vec<Yaml>> = sequence
                    .iter()
                    .map(|item| self.clone().recursive_parse(item.clone()))
                    .collect();

                let new_seq = results?;

                Ok(Yaml::Sequence(new_seq))
            }
            // If the input is a mapping, process each key-value pair.
            Yaml::Mapping(mapping) => {
                let new_map: ParserResult<Mapping> = mapping
                    .iter()
                    .map(|(k, v)| {
                        let v = self.clone().recursive_parse(v.clone())?;
                        Ok((k.clone(), v))
                    })
                    .collect();

                let new_map = new_map?;

                Ok(Yaml::Mapping(new_map))
            }
            Yaml::Representation(value, style, tag) => {
                if let Some(tag) = tag {
                    let tag = format!("{}{}", tag.handle, tag.suffix);
                    match tag.as_str() {
                        "!include" => {
                            let file_path = PathBuf::from(value.into_owned());
                            self.handle_include(file_path)
                        }
                        _ => Err(ParserError::UnsupportedTag(tag)),
                    }
                } else {
                    Ok(Yaml::value_from_cow_and_metadata(value, style, None))
                }
            }
            _ => Ok(input),
        }
    }

    fn handle_include<'a>(self, file_path: PathBuf) -> ParserResult<Yaml<'a>> {
        let normalized_path = self.process_path(&file_path)?;

        let result = match normalized_path.extension() {
            Some(ext) if ext == "yaml" || ext == "yml" => {
                match YamlInclude::new_node(normalized_path, Some(self.seen_paths.clone())) {
                    Ok(preprocessor) => preprocessor.parse()?,
                    Err(e) => return Err(e),
                }
            }
            _ => {
                return Err(ParserError::UnsupportedFileExtension(
                    file_path.to_string_lossy().to_string(),
                ));
            }
        };

        Ok(result)
    }

    fn process_path(&self, file_path: &PathBuf) -> ParserResult<PathBuf> {
        if file_path.is_absolute() {
            return Ok(file_path.clone());
        }

        let mut path = self.root_path.clone();

        path = path
            .parent()
            .ok_or(ParserError::InvalidIncludePath(
                file_path.to_string_lossy().to_string(),
            ))?
            .join(file_path);

        if !path.is_file() {
            return Err(ParserError::InvalidIncludePath(
                file_path.to_string_lossy().to_string(),
            ));
        }

        let canonicalized = canonicalize(&path)?;

        Ok(canonicalized)
    }
}

impl fmt::Display for YamlInclude {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out_str = String::new();
        let mut emitter = YamlEmitter::new(&mut out_str);
        emitter.dump(&self.parse().unwrap()).unwrap();

        write!(f, "{out_str}")
    }
}
