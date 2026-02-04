## Overview

Makina Transpiler is a Rust compiler that converts YAML-based DSL files into TOML rootfiles for the Makina Protocol. The output is used by Makina CLI tool that generates transactions compatible with Weiroll VM (https://github.com/EnsoBuild/enso-weiroll) found in the Caliber contract. 

## Build Commands

```bash
# Format check
cargo fmt --all --check

# Run all tests
cargo test --locked

# Lint with clippy
cargo clippy --all-targets --all-features --locked --frozen

# Build release
cargo build --release
```

## Running the Transpiler

```bash
# Basic transpilation
cargo run -p transpiler -- transpile -i input.yaml -o rootfile.toml 

# With token list
cargo run -p transpiler -- transpile -i input.yaml -o rootfile.toml -t token_list.json

# Basic check of addresses in resulting root file (requires ETHERSCAN_API_KEY)
cargo run -p transpiler -- check -i input.yaml

# Calculate merkle root (the actual hash)
cargo run -p transpiler -- root -i input.yaml -t token_list.json
```

## General notes

* Use miette for rich error handling
* Prefer using crate:: to super::;
* Only add comments when code has a non obvious nuance or it's not clear what the context is
* Before handoff run format and tests
* Keep files small < ~1050 LOC; suggest to split/refactor as needed
* Don't use 3rd party APIs in tests or mock important functionality
* Prefer strong types over strings
* “Make a note” => update this AGENTS.md (shortcut; not a blocker). Ignore CLAUDE.md.

## Critical Thinking
* Fix root cause (not band-aid)
* Unsure: read more code; if still stuck, ask w/ short options
* Conflicts: call out; pick safer path
* Unrecognized changes: assume other agent; keep going; focus your changes. If it causes issues, stop + ask user.
* Leave breadcrumb notes in thread.

### Architecture

The transpiler has a three-stage pipeline:

1. **Parsing** (`src/core/parser/`) - Parses YAML position files, resolves `!include` directives, and validates Solidity types
2. **Transpilation** (`src/core/transpiler.rs`) - Converts parsed positions into MakinaInstructions using weiroll's Planner for command encoding
3. **Output** (`src/merkletree.rs`) - Builds merkle tree from instructions and generates formatted TOML rootfile

### Key Modules

- `src/cli.rs` - CLI with three subcommands: `transpile`, `check`, `root`
- `src/core/parser/positions/parser.rs` - Main YAML parser for position definitions
- `src/core/parser/blueprint/parser.rs` - Blueprint template resolution
- `src/core/parser/sol_types.rs` - Solidity type parsing via alloy's DynSolType
- `src/types.rs` - Core types: MakinaInstruction, Rootfile, InputSlotType, InstructionType
- `src/check.rs` - Contract address verification via Etherscan API
- `src/token_list.rs` - Token metadata management

## DSL Structure

**Position files** define positions with instructions. Positions can have variables that instructions can refer to.
**Instruction files** define per protocol (or protocol variation) templates for entering, exiting and accounting for positions.
**Blueprint files** are reusable templates included via `!include` directives. Template variables use `${config.NAME}`, `${inputs.NAME}`, `${returns.NAME}`, and `${builtins.UINT256_MAX}` syntax.

Test data in `test_data/` contains working examples of all DSL features.

## Notable dependencies

- Uses MakinaHQ fork of weiroll-rs (not upstream)
- alloy for Ethereum types and ABI encoding
- saphyr for YAML parsing with source location preservation
