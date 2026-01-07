use alloy::dyn_abi::{DynSolType, DynSolValue};
use std::collections::HashMap;

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

    /// Create a DynSolValue from a map of field names to values.
    /// Returns an error if required fields are missing.
    fn create_value(&self, values: HashMap<String, DynSolValue>) -> Result<DynSolValue, String> {
        let props = self.properties();
        let mut tuple_values = Vec::with_capacity(props.len());
        let mut prop_names = Vec::with_capacity(props.len());

        for (prop_name, _prop_type) in &props {
            let value = values
                .get(prop_name)
                .ok_or_else(|| format!("Missing field: {prop_name}"))?;
            prop_names.push(prop_name.clone());
            tuple_values.push(value.clone());
        }

        Ok(DynSolValue::CustomStruct {
            name: self.name().to_string(),
            prop_names,
            tuple: tuple_values,
        })
    }

    /// Encode values to a map of field names to ABI-encoded bytes.
    /// This handles dynamic types (Bytes, String) by stripping the length prefix.
    fn encode_values(
        &self,
        values: HashMap<String, DynSolValue>,
    ) -> Result<HashMap<String, Vec<u8>>, String> {
        use alloy::sol_types::SolValue;

        let props = self.properties();
        let mut result = HashMap::with_capacity(props.len());

        for (prop_name, prop_type) in &props {
            let value = values
                .get(prop_name)
                .ok_or_else(|| format!("Missing field: {prop_name}"))?;

            let encoded = if prop_type.is_dynamic() {
                // Dynamic types (bytes, string, arrays) include a 32-byte offset pointer
                // as the first word when ABI-encoded. We skip it to get length + data.
                value.abi_encode().into_iter().skip(32).collect()
            } else {
                value.abi_encode()
            };

            result.insert(prop_name.clone(), encoded);
        }

        Ok(result)
    }
}

/// Macro to define meta types with minimal boilerplate.
///
/// # Usage
///
/// ```ignore
/// define_meta_types! {
///     TypeName(TypeNameDef) {
///         properties: [
///             ("fieldName", DynSolType::Address),
///             ("otherField", DynSolType::Uint(256)),
///         ]
///     },
///     // For types that need a custom property_map (subset of fields for user input):
///     SpecialType(SpecialTypeDef) {
///         properties: [
///             ("field1", DynSolType::Address),
///             ("field2", DynSolType::Uint(256)),
///             ("field3", DynSolType::Bytes),
///         ],
///         property_map: [
///             ("field1", DynSolType::Address),
///         ]
///     },
/// }
/// ```
macro_rules! define_meta_types {
    (
        $(
            $name:ident($struct_name:ident) {
                properties: [
                    $( ($prop_name:expr, $prop_type:expr) ),* $(,)?
                ]
                $(, property_map: [
                    $( ($map_name:expr, $map_type:expr) ),* $(,)?
                ])?
            }
        ),* $(,)?
    ) => {
        // Part 1: Generate the enum
        #[derive(Clone, PartialEq, Eq, Hash)]
        pub enum MetaDynSolType {
            $( $name($struct_name), )*
        }

        // Part 2: Generate struct definitions
        $(
            #[derive(Clone, Debug, PartialEq, Eq, Hash)]
            pub struct $struct_name;
        )*

        // Part 3: Generate MetaTypeDef implementations
        $(
            impl MetaTypeDef for $struct_name {
                fn name(&self) -> &'static str {
                    stringify!($name)
                }

                fn properties(&self) -> Vec<(String, DynSolType)> {
                    vec![
                        $( ($prop_name.into(), $prop_type), )*
                    ]
                }

                // Handle optional property_map override
                define_meta_types!(@property_map_impl $( $( ($map_name, $map_type) )* )?);
            }
        )*

        // Part 4: Generate MetaDynSolType impl
        impl MetaDynSolType {
            pub fn from_name(name: &str) -> Option<Self> {
                match name {
                    $( stringify!($name) => Some(Self::$name($struct_name)), )*
                    _ => None,
                }
            }

            pub fn name(&self) -> String {
                match self {
                    $( Self::$name(_) => stringify!($name).to_string(), )*
                }
            }

            pub fn to_dyn_sol_type(&self) -> DynSolType {
                match self {
                    $( Self::$name(def) => def.to_dyn_sol_type(), )*
                }
            }

            pub fn property_map(&self) -> HashMap<String, DynSolType> {
                match self {
                    $( Self::$name(def) => def.property_map(), )*
                }
            }

            pub fn create_value(
                &self,
                values: HashMap<String, DynSolValue>,
            ) -> Result<DynSolValue, String> {
                match self {
                    $( Self::$name(def) => def.create_value(values), )*
                }
            }

            pub fn encode_values(
                &self,
                values: HashMap<String, DynSolValue>,
            ) -> Result<HashMap<String, Vec<u8>>, String> {
                match self {
                    $( Self::$name(def) => def.encode_values(values), )*
                }
            }
        }

        // Part 5: Generate Display, Debug, FromStr implementations
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
    };

    // Internal rule: no property_map override - use default from trait
    (@property_map_impl) => {};

    // Internal rule: generate property_map override
    (@property_map_impl $( ($map_name:expr, $map_type:expr) )+) => {
        fn property_map(&self) -> HashMap<String, DynSolType> {
            let mut map = HashMap::new();
            $( map.insert($map_name.into(), $map_type); )*
            map
        }
    };
}

// Define all meta types using the macro
define_meta_types! {
    SwapData(SwapDataDef) {
        properties: [
            ("swapperId", DynSolType::Uint(16)),
            ("data", DynSolType::Bytes),
            ("inputToken", DynSolType::Address),
            ("outputToken", DynSolType::Address),
            ("inputAmount", DynSolType::Uint(256)),
            ("minOutputAmount", DynSolType::Uint(256)),
        ]
    },
    PendleSwapData(PendleSwapDataDef) {
        properties: [
            ("minPtOut", DynSolType::Uint(256)),
            ("guessPtOut", DynSolType::Bytes),
            ("input", DynSolType::Bytes),
            ("limit", DynSolType::Bytes),
        ]
    },
    ApproveData(ApproveDataDef) {
        properties: [
            ("guy", DynSolType::Address),
            ("amount", DynSolType::Uint(256)),
        ]
    },
    MerklClaimData(MerklClaimDataDef) {
        properties: [
            ("users", DynSolType::Array(Box::new(DynSolType::Address))),
            ("tokens", DynSolType::Array(Box::new(DynSolType::Address))),
            ("amounts", DynSolType::Array(Box::new(DynSolType::Uint(256)))),
            ("proofs", DynSolType::Array(Box::new(DynSolType::Array(Box::new(DynSolType::FixedBytes(32)))))),
        ]
    },
    FxSaveData(FxSaveDataDef) {
        properties: [
            ("tokenIn", DynSolType::Address),
            ("amountToDeposit", DynSolType::Uint(256)),
            ("minAmountOut", DynSolType::Uint(256)),
            ("abiEncodeConvert", DynSolType::Bytes),
        ],
        // Override property_map: only expose 2 fields for user input
        property_map: [
            ("tokenIn", DynSolType::Address),
            ("amountToDeposit", DynSolType::Uint(256)),
        ]
    },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_name_all_types() {
        assert!(MetaDynSolType::from_name("SwapData").is_some());
        assert!(MetaDynSolType::from_name("PendleSwapData").is_some());
        assert!(MetaDynSolType::from_name("ApproveData").is_some());
        assert!(MetaDynSolType::from_name("MerklClaimData").is_some());
        assert!(MetaDynSolType::from_name("FxSaveData").is_some());
        assert!(MetaDynSolType::from_name("NonExistent").is_none());
    }

    #[test]
    fn test_name_method() {
        assert_eq!(MetaDynSolType::SwapData(SwapDataDef).name(), "SwapData");
        assert_eq!(
            MetaDynSolType::PendleSwapData(PendleSwapDataDef).name(),
            "PendleSwapData"
        );
        assert_eq!(
            MetaDynSolType::ApproveData(ApproveDataDef).name(),
            "ApproveData"
        );
        assert_eq!(
            MetaDynSolType::MerklClaimData(MerklClaimDataDef).name(),
            "MerklClaimData"
        );
        assert_eq!(
            MetaDynSolType::FxSaveData(FxSaveDataDef).name(),
            "FxSaveData"
        );
    }

    #[test]
    fn test_from_str() {
        let swap: MetaDynSolType = "SwapData".parse().unwrap();
        assert!(matches!(swap, MetaDynSolType::SwapData(_)));

        let pendle: MetaDynSolType = "PendleSwapData".parse().unwrap();
        assert!(matches!(pendle, MetaDynSolType::PendleSwapData(_)));

        let err = "Invalid".parse::<MetaDynSolType>();
        assert!(err.is_err());
        assert_eq!(err.unwrap_err(), "Unknown meta type: Invalid");
    }

    #[test]
    fn test_properties_count() {
        assert_eq!(SwapDataDef.properties().len(), 6);
        assert_eq!(PendleSwapDataDef.properties().len(), 4);
        assert_eq!(ApproveDataDef.properties().len(), 2);
        assert_eq!(MerklClaimDataDef.properties().len(), 4);
        assert_eq!(FxSaveDataDef.properties().len(), 4);
    }

    #[test]
    fn test_fx_save_data_property_map_override() {
        let fx_props = FxSaveDataDef.property_map();
        // FxSaveData should only expose 2 fields in property_map
        assert_eq!(fx_props.len(), 2);
        assert!(fx_props.contains_key("tokenIn"));
        assert!(fx_props.contains_key("amountToDeposit"));
        // These should NOT be in property_map
        assert!(!fx_props.contains_key("minAmountOut"));
        assert!(!fx_props.contains_key("abiEncodeConvert"));
    }

    #[test]
    fn test_default_property_map() {
        let swap_props = SwapDataDef.property_map();
        // Should contain all properties
        assert_eq!(swap_props.len(), 6);
        assert!(swap_props.contains_key("swapperId"));
        assert!(swap_props.contains_key("data"));
        assert!(swap_props.contains_key("inputToken"));
        assert!(swap_props.contains_key("outputToken"));
        assert!(swap_props.contains_key("inputAmount"));
        assert!(swap_props.contains_key("minOutputAmount"));
    }

    #[test]
    fn test_to_dyn_sol_type() {
        let sol_type = SwapDataDef.to_dyn_sol_type();
        match sol_type {
            DynSolType::CustomStruct {
                name,
                prop_names,
                tuple,
            } => {
                assert_eq!(name, "SwapData");
                assert_eq!(prop_names.len(), 6);
                assert_eq!(tuple.len(), 6);
                assert_eq!(prop_names[0], "swapperId");
                assert_eq!(prop_names[1], "data");
            }
            _ => panic!("Expected CustomStruct"),
        }
    }

    #[test]
    fn test_display() {
        let swap = MetaDynSolType::SwapData(SwapDataDef);
        assert_eq!(format!("{}", swap), "SwapData");

        let fx = MetaDynSolType::FxSaveData(FxSaveDataDef);
        assert_eq!(format!("{}", fx), "FxSaveData");
    }

    #[test]
    fn test_debug() {
        let swap = MetaDynSolType::SwapData(SwapDataDef);
        assert_eq!(format!("{:?}", swap), "SwapData");

        let merkl = MetaDynSolType::MerklClaimData(MerklClaimDataDef);
        assert_eq!(format!("{:?}", merkl), "MerklClaimData");
    }

    #[test]
    fn test_derives_clone_eq_hash() {
        let swap1 = MetaDynSolType::SwapData(SwapDataDef);
        let swap2 = swap1.clone();
        assert_eq!(swap1, swap2);

        use std::collections::HashSet;
        let mut set = HashSet::new();
        set.insert(swap1.clone());
        assert!(set.contains(&swap2));

        // Test inequality
        let pendle = MetaDynSolType::PendleSwapData(PendleSwapDataDef);
        assert_ne!(swap1, pendle);
    }

    #[test]
    fn test_property_types() {
        let props = SwapDataDef.properties();
        let prop_map: HashMap<_, _> = props.into_iter().collect();

        assert_eq!(prop_map.get("swapperId"), Some(&DynSolType::Uint(16)));
        assert_eq!(prop_map.get("data"), Some(&DynSolType::Bytes));
        assert_eq!(prop_map.get("inputToken"), Some(&DynSolType::Address));
        assert_eq!(prop_map.get("outputToken"), Some(&DynSolType::Address));
        assert_eq!(prop_map.get("inputAmount"), Some(&DynSolType::Uint(256)));
        assert_eq!(
            prop_map.get("minOutputAmount"),
            Some(&DynSolType::Uint(256))
        );
    }

    #[test]
    fn test_complex_array_types() {
        let props = MerklClaimDataDef.properties();
        let prop_map: HashMap<_, _> = props.into_iter().collect();

        // Check users: address[]
        let users_type = prop_map.get("users").unwrap();
        match users_type {
            DynSolType::Array(inner) => {
                assert_eq!(**inner, DynSolType::Address);
            }
            _ => panic!("Expected array for users"),
        }

        // Check amounts: uint256[]
        let amounts_type = prop_map.get("amounts").unwrap();
        match amounts_type {
            DynSolType::Array(inner) => {
                assert_eq!(**inner, DynSolType::Uint(256));
            }
            _ => panic!("Expected array for amounts"),
        }

        // Check proofs: bytes32[][]
        let proofs_type = prop_map.get("proofs").unwrap();
        match proofs_type {
            DynSolType::Array(inner) => match inner.as_ref() {
                DynSolType::Array(inner2) => {
                    assert_eq!(**inner2, DynSolType::FixedBytes(32));
                }
                _ => panic!("Expected nested array for proofs"),
            },
            _ => panic!("Expected array for proofs"),
        }
    }

    #[test]
    fn test_meta_type_def_trait_name() {
        assert_eq!(SwapDataDef.name(), "SwapData");
        assert_eq!(PendleSwapDataDef.name(), "PendleSwapData");
        assert_eq!(ApproveDataDef.name(), "ApproveData");
        assert_eq!(MerklClaimDataDef.name(), "MerklClaimData");
        assert_eq!(FxSaveDataDef.name(), "FxSaveData");
    }

    #[test]
    fn test_create_value_approve_data() {
        use alloy::primitives::{Address, U256};

        let mut values = HashMap::new();
        values.insert("guy".to_string(), DynSolValue::Address(Address::ZERO));
        values.insert(
            "amount".to_string(),
            DynSolValue::Uint(U256::from(1000), 256),
        );

        let result = ApproveDataDef.create_value(values).unwrap();

        match result {
            DynSolValue::CustomStruct {
                name,
                prop_names,
                tuple,
            } => {
                assert_eq!(name, "ApproveData");
                assert_eq!(prop_names, vec!["guy", "amount"]);
                assert_eq!(tuple.len(), 2);
            }
            _ => panic!("Expected CustomStruct"),
        }
    }

    #[test]
    fn test_create_value_missing_field() {
        use alloy::primitives::Address;

        let mut values = HashMap::new();
        values.insert("guy".to_string(), DynSolValue::Address(Address::ZERO));
        // Missing "amount" field

        let result = ApproveDataDef.create_value(values);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "Missing field: amount");
    }

    #[test]
    fn test_create_value_via_enum() {
        use alloy::primitives::{Address, U256};

        let meta_type = MetaDynSolType::ApproveData(ApproveDataDef);

        let mut values = HashMap::new();
        values.insert("guy".to_string(), DynSolValue::Address(Address::ZERO));
        values.insert(
            "amount".to_string(),
            DynSolValue::Uint(U256::from(500), 256),
        );

        let result = meta_type.create_value(values).unwrap();

        match result {
            DynSolValue::CustomStruct { name, .. } => {
                assert_eq!(name, "ApproveData");
            }
            _ => panic!("Expected CustomStruct"),
        }
    }

    #[test]
    fn test_create_value_fx_save_data() {
        use alloy::primitives::{Address, U256};

        let mut values = HashMap::new();
        values.insert("tokenIn".to_string(), DynSolValue::Address(Address::ZERO));
        values.insert(
            "amountToDeposit".to_string(),
            DynSolValue::Uint(U256::from(1000), 256),
        );
        values.insert(
            "minAmountOut".to_string(),
            DynSolValue::Uint(U256::from(900), 256),
        );
        values.insert(
            "abiEncodeConvert".to_string(),
            DynSolValue::Bytes(vec![0x01, 0x02, 0x03]),
        );

        let result = FxSaveDataDef.create_value(values).unwrap();

        match result {
            DynSolValue::CustomStruct {
                name,
                prop_names,
                tuple,
            } => {
                assert_eq!(name, "FxSaveData");
                assert_eq!(prop_names.len(), 4);
                assert_eq!(tuple.len(), 4);
            }
            _ => panic!("Expected CustomStruct"),
        }
    }
}
