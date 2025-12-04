use std::{collections::HashSet, path::PathBuf};

use alloy::primitives::{Address, U256};
use color_eyre::owo_colors::OwoColorize;
use eyre::eyre;

use crate::{
    core::parser::{
        blueprint::{Blueprint, BlueprintParser},
        positions::types::{InstructionTemplateEnum, Root},
    },
    etherscan::Etherscan,
};

pub struct Check {
    root_path: PathBuf,
    root: Root,
    etherscan: Etherscan,
    /// [Check] emits github annotation syntax when `git_root` is set
    git_root: Option<PathBuf>,
}

struct AddressInfo {
    address: Address,
    path: PathBuf,
    label: String,
}

impl Check {
    pub fn new(root_path: PathBuf, root: Root, github_errors: bool) -> Self {
        let chain = if let Some(val) = root.config.get("chain_id") {
            let id: U256 = match &val.template {
                InstructionTemplateEnum::Config(yaml_sol_val) => {
                    yaml_sol_val
                        .value
                        .as_uint()
                        .expect("chain_id must be a uint")
                        .0
                }
                _ => {
                    panic!("chain_id must be a config value");
                }
            };
            tracing::info!("read chain_id: {id} from input file config");
            id
        } else {
            tracing::info!("defaulting to chain_id: 1");
            U256::ONE
        };

        let git_root = if github_errors {
            Some(
                std::env::var("GITHUB_WORKSPACE")
                    .expect("GITHUB_WORKSPACE must be set when running with `--github-errors` flag")
                    .into(),
            )
        } else {
            None
        };

        Self {
            root,
            root_path: std::fs::canonicalize(root_path).expect("could not canonicalize root_path"),
            etherscan: Etherscan::new(chain),
            git_root,
        }
    }

    pub async fn all_addresses_verified(&self) -> eyre::Result<()> {
        let mut checked = HashSet::new();

        for addr in self.labeled_addresses() {
            if checked.contains(&addr.address) {
                continue;
            }

            let is_verified = self.is_address_verified(addr.address).await?;
            checked.insert(addr.address);

            if is_verified {
                println!(
                    "verified address: {} - {} - {}",
                    addr.address,
                    addr.label,
                    addr.path.to_string_lossy()
                );
                continue;
            }

            if let Some(git_root) = self.git_root.clone() {
                let path = addr
                    .path
                    .strip_prefix(git_root)
                    .expect("path is not in repo")
                    .to_string_lossy();

                println!(
                    "::warning title=unverified address,file={},line=1::{} - {}",
                    path, addr.label, addr.address
                );
            }

            println!(
                "{}",
                format!(
                    "unverified address: {} - {} - {}",
                    addr.address,
                    addr.label,
                    addr.path.to_string_lossy()
                )
                .yellow()
            );
        }

        Ok(())
    }

    async fn is_address_verified(&self, addr: Address) -> eyre::Result<bool> {
        let resp = self.etherscan.get_abi(&addr).await?;

        if resp.status == 1 {
            return Ok(true);
        }

        if resp.status != 1 && resp.result == "Contract source code not verified" {
            return Ok(false);
        }

        Err(eyre!("etherscan API call failed: {}", resp.result))
    }

    fn labeled_addresses(&self) -> Vec<AddressInfo> {
        let mut result = Vec::new();
        result.append(&mut self.config_addresses());
        result.append(&mut self.token_addresses());
        result.append(&mut self.blueprint_addresses());
        result
    }

    fn config_addresses(&self) -> Vec<AddressInfo> {
        let mut result = Vec::new();

        for (k, v) in self.root.config.iter() {
            if let InstructionTemplateEnum::Config(ref yaml_sol_val) = v.template
                && let Some(address) = yaml_sol_val.value.as_address() {
                    result.push(AddressInfo {
                        address,
                        label: format!("config ({k})"),
                        path: self.root_path.clone(),
                    });
                    continue;
                }
        }

        result
    }

    fn token_addresses(&self) -> Vec<AddressInfo> {
        self.root
            .positions
            .iter()
            .flat_map(|p| &p.instructions)
            .flat_map(|i| &i.affected_tokens)
            .cloned()
            .map(|address| AddressInfo {
                address,
                label: "affected token".to_string(),
                path: self.root_path.clone(),
            })
            .collect()
    }

    /// Returns deduplicated and resolved blueprints of the current [Root].
    fn blueprints(&self) -> Vec<Blueprint> {
        let paths: HashSet<PathBuf> = self
            .root
            .positions
            .iter()
            .flat_map(|p| &p.instructions)
            .map(|i| &i.definition.blueprint_path)
            .cloned()
            .collect();

        paths
            .iter()
            .filter_map(|path| BlueprintParser::new(path).ok())
            .filter_map(|parser| parser.parse().ok())
            .collect()
    }

    fn blueprint_addresses(&self) -> Vec<AddressInfo> {
        let mut result = Vec::new();

        for blueprint in self.blueprints() {
            let path =
                std::fs::canonicalize(&blueprint.path).expect("could not get blueprint path");

            let mut addresses: Vec<AddressInfo> = blueprint
                .constants
                .values()
                .filter_map(|v| v.value.as_address())
                .map(|address| AddressInfo {
                    address,
                    path: path.clone(),
                    label: "blueprint".to_string(),
                })
                .collect();

            result.append(&mut addresses);
        }

        result
    }
}
