use std::path::PathBuf;

use alloy::primitives::Address;
use alloy_chains::Chain;

use serde::{Deserialize, Serialize};
use serde_json::Value;

pub type ExtensionValue = Value;

fn serialize_chain_as_u64<S>(chain: &Chain, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    serializer.serialize_u64(chain.id())
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct TokenInfo {
    #[serde(rename = "chainId", serialize_with = "serialize_chain_as_u64")]
    pub chain: Chain,
    pub address: Address,
    pub name: String,
    pub decimals: u8,
    pub symbol: String,
}

#[derive(Debug, Deserialize, Clone)]
pub struct Version {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
}

#[derive(Debug, Deserialize, Clone, Default)]
pub struct TokenList {
    pub name: String,
    pub timestamp: String,
    pub version: Option<Version>,
    pub tokens: Vec<TokenInfo>,
}

impl TokenList {
    /// Create TokenList from json path
    pub fn new(json_path: PathBuf) -> eyre::Result<Self> {
        let data = std::fs::read_to_string(json_path)?;
        let token_list = serde_json::from_str(&data)?;
        Ok(token_list)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloy::primitives::address;

    #[test]
    fn test_token_list_deserialization() {
        let path = PathBuf::from("test_data/token_lists/test.json");
        let token_list = TokenList::new(path).expect("Failed to deserialize token list");
        assert_eq!(token_list.name, "TEST-TOKEN-LIST");
        assert_eq!(token_list.tokens.len(), 2);
        assert_eq!(token_list.tokens[0].name, "Dialectic USD");
        assert_eq!(
            token_list.tokens[0].address,
            address!("0x1e33E98aF620F1D563fcD3cfd3C75acE841204ef")
        );
        assert_eq!(token_list.tokens[0].chain.id(), 1);
        assert_eq!(token_list.tokens[0].chain, Chain::mainnet());
    }

    #[test]
    fn test_token_info_serialization() {
        let token_info = TokenInfo {
            chain: Chain::mainnet(),
            address: address!("0x1e33E98aF620F1D563fcD3cfd3C75acE841204ef"),
            name: "Dialectic USD".to_string(),
            decimals: 18,
            symbol: "DUSD".to_string(),
        };

        let serialized = serde_json::to_string(&token_info).expect("Failed to serialize TokenInfo");
        let expected = r#"{"chainId":1,"address":"0x1e33e98af620f1d563fcd3cfd3c75ace841204ef","name":"Dialectic USD","decimals":18,"symbol":"DUSD"}"#;
        assert_eq!(serialized, expected);
    }

    #[test]
    fn test_token_list_default() {
        let token_list = TokenList::default();
        assert_eq!(token_list.name, "");
        assert_eq!(token_list.timestamp, "");
        assert_eq!(token_list.tokens.len(), 0);
    }
}
