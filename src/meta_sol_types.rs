use alloy::dyn_abi::DynSolType;
use std::collections::HashMap;

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum MetaDynSolType {
    SwapData(SwapDataDef),
    PendleSwapData(PendleSwapDataDef),
    ApproveData(ApproveDataDef),
    MerklClaimData(MerklClaimDataDef),
    FxSaveData(FxSaveDataDef),
}

/// Trait to be implemented by each meta type definition
pub trait MetaTypeDef {
    fn name(&self) -> &'static str;
    fn properties(&self) -> Vec<(String, DynSolType)>;

    fn to_dyn_sol_type(&self) -> DynSolType {
        let props = self.properties();
        let (names, types): (Vec<_>, Vec<_>) = props.into_iter().unzip();

        DynSolType::CustomStruct {
            name: self.name().to_string(),
            prop_names: names,
            tuple: types,
        }
    }

    fn property_map(&self) -> HashMap<String, DynSolType> {
        self.properties().into_iter().collect()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SwapDataDef;

impl MetaTypeDef for SwapDataDef {
    fn name(&self) -> &'static str {
        "SwapData"
    }

    fn properties(&self) -> Vec<(String, DynSolType)> {
        vec![
            ("swapperId".into(), DynSolType::Uint(16)),
            ("data".into(), DynSolType::Bytes),
            ("inputToken".into(), DynSolType::Address),
            ("outputToken".into(), DynSolType::Address),
            ("inputAmount".into(), DynSolType::Uint(256)),
            ("minOutputAmount".into(), DynSolType::Uint(256)),
        ]
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PendleSwapDataDef;

impl MetaTypeDef for PendleSwapDataDef {
    fn name(&self) -> &'static str {
        "PendleSwapData"
    }

    fn properties(&self) -> Vec<(String, DynSolType)> {
        vec![
            ("minPtOut".into(), DynSolType::Uint(256)),
            ("guessPtOut".into(), DynSolType::Bytes),
            ("input".into(), DynSolType::Bytes),
            ("limit".into(), DynSolType::Bytes),
        ]
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ApproveDataDef;

impl MetaTypeDef for ApproveDataDef {
    fn name(&self) -> &'static str {
        "ApproveData"
    }

    fn properties(&self) -> Vec<(String, DynSolType)> {
        vec![
            ("guy".into(), DynSolType::Address),
            ("amount".into(), DynSolType::Uint(256)),
        ]
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MerklClaimDataDef;

impl MetaTypeDef for MerklClaimDataDef {
    fn name(&self) -> &'static str {
        "MerklClaimData"
    }

    fn properties(&self) -> Vec<(String, DynSolType)> {
        vec![
            (
                "users".into(),
                DynSolType::Array(Box::new(DynSolType::Address)),
            ),
            (
                "tokens".into(),
                DynSolType::Array(Box::new(DynSolType::Address)),
            ),
            (
                "amounts".into(),
                DynSolType::Array(Box::new(DynSolType::Uint(256))),
            ),
            (
                "proofs".into(),
                DynSolType::Array(Box::new(DynSolType::Array(Box::new(
                    DynSolType::FixedBytes(32),
                )))),
            ),
        ]
    }
}

// ---------------- FxSaveData ---------------- //

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FxSaveDataDef;
impl MetaTypeDef for FxSaveDataDef {
    fn name(&self) -> &'static str {
        "FxSaveData"
    }

    fn properties(&self) -> Vec<(String, DynSolType)> {
        vec![
            ("tokenIn".into(), DynSolType::Address),
            ("amountToDeposit".into(), DynSolType::Uint(256)),
            ("minAmountOut".into(), DynSolType::Uint(256)),
            ("abiEncodeConvert".into(), DynSolType::Bytes),
        ]
    }

    // Override the property_map because only two fields are needed for user input
    fn property_map(&self) -> HashMap<String, DynSolType> {
        let mut map = HashMap::new();
        map.insert("tokenIn".into(), DynSolType::Address);
        map.insert("amountToDeposit".into(), DynSolType::Uint(256));
        map
    }
}

impl MetaDynSolType {
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "SwapData" => Some(Self::SwapData(SwapDataDef)),
            "PendleSwapData" => Some(Self::PendleSwapData(PendleSwapDataDef)),
            "ApproveData" => Some(Self::ApproveData(ApproveDataDef)),
            "MerklClaimData" => Some(Self::MerklClaimData(MerklClaimDataDef)),
            "FxSaveData" => Some(Self::FxSaveData(FxSaveDataDef)),
            _ => None,
        }
    }

    pub fn name(&self) -> String {
        match self {
            Self::SwapData(_) => "SwapData".to_string(),
            Self::PendleSwapData(_) => "PendleSwapData".to_string(),
            Self::ApproveData(_) => "ApproveData".to_string(),
            Self::MerklClaimData(_) => "MerklClaimData".to_string(),
            Self::FxSaveData(_) => "FxSaveData".to_string(),
        }
    }

    pub fn to_dyn_sol_type(&self) -> DynSolType {
        match self {
            Self::SwapData(def) => def.to_dyn_sol_type(),
            Self::PendleSwapData(def) => def.to_dyn_sol_type(),
            Self::ApproveData(def) => def.to_dyn_sol_type(),
            Self::MerklClaimData(def) => def.to_dyn_sol_type(),
            Self::FxSaveData(def) => def.to_dyn_sol_type(),
        }
    }

    pub fn property_map(&self) -> HashMap<String, DynSolType> {
        match self {
            Self::SwapData(def) => def.property_map(),
            Self::PendleSwapData(def) => def.property_map(),
            Self::ApproveData(def) => def.property_map(),
            Self::MerklClaimData(def) => def.property_map(),
            Self::FxSaveData(def) => def.property_map(),
        }
    }
}

impl std::fmt::Debug for MetaDynSolType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl std::fmt::Display for MetaDynSolType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl std::str::FromStr for MetaDynSolType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        MetaDynSolType::from_name(s).ok_or_else(|| format!("Unknown meta type: {s}"))
    }
}
