# Transpiler

A compiler that takes machine instructions defined in a YAML-based DSL and transpiles them into rootfiles for machines. The compiled configuration is stored in the [MakinaHQ/config](https://github.com/MakinaHQ/config) repository. Generated commands and states are based on [weiroll](https://github.com/EnsoBuild/enso-weiroll).

## Architecture

The transpiler processes:

1. **Position Files**: Define positions with instructions, affected tokens, and configuration
2. **Blueprint Files**: Reusable templates for common operations (deposit, account, etc.)
3. **Instruction Files**: Specific protocol interactions with parameterized calls

The compilation pipeline parses YAML DSL files, resolves blueprint includes, and generates TOML rootfiles with bytecode and metadata.

## Usage

Compile a YAML instruction file to a rootfile:

```bash
cargo run -p transpiler -- -i input.yaml -o rootfile.toml
```

When using token lists:

```bash
cargo run -p transpiler -- -i input.yaml -o rootfile.toml -t token_list.json
```

Example with test data:

```bash
cargo run -p transpiler -- -i test_data/caliber.yaml -o output.toml -t test_data/token_lists/test.json
```

## DSL Format

Input files use a YAML-based domain-specific language with:

- **Config**: Caliber addresses and global configuration
- **Positions**: Individual position definitions with management/accounting instructions
- **Blueprints**: Reusable templates via `!include` directives
- **Parameters**: Type-safe inputs with address, uint256, and other Solidity types

The transpiler outputs TOML rootfiles that can be used to interact with the caliber contracts.
