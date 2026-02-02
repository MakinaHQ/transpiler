use std::path::PathBuf;

use alloy::primitives::Address;
use alloy_chains::Chain;

use serde::Deserialize;

#[derive(Debug, Deserialize, Clone, PartialEq)]
pub struct HelperInfo {
    #[serde(rename = "chainId")]
    pub chain: Chain,
    pub address: Address,
    pub name: String,
}

#[derive(Debug, Deserialize, Clone)]
pub struct Version {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
}

#[derive(Debug, Deserialize, Clone, Default)]
pub struct HelpersList {
    pub name: String,
    pub timestamp: String,
    pub version: Option<Version>,
    pub helpers: Vec<HelperInfo>,
}

impl HelpersList {
    /// Create HelpersList from json path
    pub fn new(json_path: PathBuf) -> eyre::Result<Self> {
        let data = std::fs::read_to_string(json_path)?;
        let helpers_list = serde_json::from_str(&data)?;
        Ok(helpers_list)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloy::primitives::address;

    #[test]
    fn test_helpers_list_deserialization() {
        let path = PathBuf::from("test_data/helpers/test.json");
        let helpers_list = HelpersList::new(path).expect("Failed to deserialize helpers list");
        assert_eq!(helpers_list.name, "Test Helper Contracts List");
        assert_eq!(helpers_list.helpers.len(), 2);

        // Verify field mappings work correctly (chainId -> chain, address parsing)
        assert_eq!(helpers_list.helpers[0].name, "ContextHelper");
        assert_eq!(helpers_list.helpers[0].chain, Chain::mainnet());
        assert_eq!(
            helpers_list.helpers[0].address,
            address!("0x0f431322E1fF2500D4C5a4E090A7Da7344F953BE")
        );
    }

    #[test]
    fn test_helpers_list_default() {
        let helpers_list = HelpersList::default();
        assert_eq!(helpers_list.name, "");
        assert_eq!(helpers_list.timestamp, "");
        assert_eq!(helpers_list.helpers.len(), 0);
    }

    #[test]
    fn test_invalid_chain_name_fails() {
        // chainId must be a number, not a string like "arbitrum_doesnt_exist"
        let path = PathBuf::from("test_data/helpers/invalid_chain_name.json");
        let result = HelpersList::new(path);
        assert!(
            result.is_err(),
            "Expected error for invalid chain name string"
        );
    }

    #[test]
    fn test_unknown_chain_id_succeeds() {
        // Unknown chain IDs are allowed by alloy_chains (forward compatibility)
        // They become "unnamed" chains with just the ID
        let path = PathBuf::from("test_data/helpers/unknown_chain_id.json");
        let helpers_list = HelpersList::new(path).expect("Unknown chain IDs should be allowed");
        assert_eq!(helpers_list.helpers[0].chain.id(), 67676767);
    }
}
