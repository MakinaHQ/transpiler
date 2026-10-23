//! Instruction inputs vs the blueprint's declared inputs (review on MakinaHQ/transpiler#42).

use std::fs;

use crate::core::{
    parser::{blueprint::BlueprintParser, positions::parser::PositionParser},
    transpiler::get_rootfile_from_positions,
};

const BLUEPRINT: &str = r#"protocol: "test"
inputs:
  x:
    type: "uint256"
actions:
  use:
    calls:
      - target: "0x0000000000000000000000000000000000000001"
        selector: "use(uint256)"
        parameters:
          - type: "uint256"
            value: "${inputs.x}"
"#;

/// Transpiles a one-position caliber whose single instruction supplies `inputs`.
fn transpile(inputs: &str) -> String {
    let dir = std::env::temp_dir().join(format!(
        "transpiler-inputs-{}-{}",
        std::process::id(),
        rand_suffix()
    ));
    fs::create_dir_all(&dir).unwrap();
    fs::write(dir.join("blueprint.yaml"), BLUEPRINT).unwrap();
    let leg = |kind: &str| {
        format!(
            "      - is_debt: false\n        instruction_type: \"{kind}\"\n        affected_tokens: []\n        instruction:\n          label: \"l\"\n          path: \"./blueprint.yaml:use\"\n          inputs:\n{inputs}\n"
        )
    };
    fs::write(
        dir.join("caliber.yaml"),
        format!(
            "config: {{}}\npositions:\n  - id: \"1\"\n    group_id: \"0\"\n    instructions:\n{}{}",
            leg("MANAGEMENT"),
            leg("ACCOUNTING")
        ),
    )
    .unwrap();
    let result = PositionParser::new(dir.join("caliber.yaml"), None, None)
        .and_then(|p| p.parse())
        .and_then(|root| get_rootfile_from_positions(&root.positions, &root.tokens));
    fs::remove_dir_all(&dir).unwrap();
    match result {
        Ok(_) => "ok".into(),
        Err(err) => format!("{err:?}"),
    }
}

fn rand_suffix() -> u64 {
    use std::sync::atomic::{AtomicU64, Ordering};
    static N: AtomicU64 = AtomicU64::new(0);
    N.fetch_add(1, Ordering::Relaxed)
}

fn input(ty: &str, value: &str) -> String {
    format!("            x:\n              type: \"{ty}\"\n              value: {value}")
}

#[test]
fn supplied_input_must_have_the_declared_type() {
    assert_eq!(transpile(&input("uint256", "\"1\"")), "ok");
    let err = transpile(&input(
        "address",
        "\"0x0000000000000000000000000000000000000002\"",
    ));
    assert!(
        err.contains("input \"x\" is address, but blueprint") && err.contains("declares uint256"),
        "{err}"
    );
}

#[test]
fn inputs_must_be_one_word() {
    // the review's case: a uint256[2] for a uint256 input
    for (ty, value) in [
        ("uint256[2]", "[\"1\", \"2\"]"),
        ("string", "\"key\""),
        ("bytes", "\"0x1234\""),
    ] {
        let err = transpile(&input(ty, value));
        assert!(err.contains("inputs must be one-word types"), "{ty}: {err}");
        assert!(
            err.contains("keccak256") && err.contains("input slot"),
            "{ty}: hint missing: {err}"
        );
    }
}

#[test]
fn undeclared_inputs_are_rejected() {
    let both = format!(
        "{}\n            y:\n              type: \"uint256\"\n              value: \"2\"",
        input("uint256", "\"1\"")
    );
    let err = transpile(&both);
    assert!(
        err.contains("supplies input \"y\", which blueprint") && err.contains("does not declare"),
        "{err}"
    );
}

#[test]
fn blueprint_inputs_must_be_one_word() {
    for ty in ["string", "bytes", "uint256[2]", "(uint256,address)"] {
        let source = BLUEPRINT.replacen(
            "type: \"uint256\"\nactions",
            &format!("type: \"{ty}\"\nactions"),
            1,
        );
        let err = format!(
            "{:?}",
            BlueprintParser::from_source(source).parse().unwrap_err()
        );
        assert!(err.contains("inputs must be one-word types"), "{ty}: {err}");
    }
}
