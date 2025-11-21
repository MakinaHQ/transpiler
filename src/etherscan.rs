use std::{collections::HashMap, env, num::NonZero};

use alloy::{
    primitives::{Address, U256},
    transports::http::reqwest,
};
use governor::{DefaultDirectRateLimiter, Jitter, Quota, RateLimiter};
use serde::{Deserialize, de::DeserializeOwned};

const ETHERSCAN_API: &str = "https://api.etherscan.io/v2/api";

#[derive(Deserialize, Debug)]
pub struct EtherscanResponse<T> {
    #[serde(with = "alloy::serde::displayfromstr")]
    pub status: u64,
    pub message: String,
    pub result: T,
}

#[derive(Debug)]
pub struct Etherscan {
    url: String,
    chain: String,
    api_key: String,
    client: reqwest::Client,
    governor: DefaultDirectRateLimiter,
}

impl Etherscan {
    pub fn new(chain: U256) -> Self {
        let client = reqwest::Client::new();
        // docs say 3 / second but anything > 1 gets limited
        let governor =
            RateLimiter::direct(Quota::per_second(NonZero::new(1).expect("we know 1 != 0")));

        let api_key = env::var("ETHERSCAN_API_KEY").expect("ETHERSCAN_API_KEY must be set");

        Self {
            url: ETHERSCAN_API.into(),
            chain: format!("{chain}"),
            api_key,
            client,
            governor,
        }
    }

    pub async fn get_abi(&self, contract: &Address) -> eyre::Result<EtherscanResponse<String>> {
        let mut params = HashMap::new();
        params.insert("module", "contract".to_string());
        params.insert("action", "getabi".to_string());
        params.insert("chainid", self.chain.clone());
        params.insert("address", contract.to_string());

        self.get(params).await
    }

    async fn get<T>(
        &self,
        params: HashMap<&'static str, String>,
    ) -> eyre::Result<EtherscanResponse<T>>
    where
        T: DeserializeOwned,
    {
        self.governor
            .until_ready_with_jitter(Jitter::default())
            .await;

        let mut params = params;
        params.insert("apikey", self.api_key.clone());

        let resp = self.client.get(&self.url).query(&params).send().await?;
        resp.json().await.map_err(Into::into)
    }
}
