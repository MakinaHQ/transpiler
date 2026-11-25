# Transpiler

A compiler that takes machine instructions defined in a YAML-based DSL and transpiles them into rootfiles for machines. The compiled configuration is stored in the [MakinaHQ/makina-integrations](https://github.com/MakinaHQ/makina-integrations) repository. Generated commands and states are based on [weiroll](https://github.com/EnsoBuild/enso-weiroll).

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

When using helper contracts:

```bash
cargo run -p transpiler -- -i input.yaml -o rootfile.toml -h helpers.json
```

Example with test data:

```bash
cargo run -p transpiler -- -i test_data/caliber.yaml -o output.toml -t test_data/token_lists/test.json -h test_data/helpers_list/helpers.json
```

## DSL Format

Input files use a YAML-based domain-specific language with:

- **Config**: Caliber addresses and global configuration
- **Positions**: Individual position definitions with management/accounting instructions
- **Blueprints**: Reusable templates via `!include` directives
- **Parameters**: Type-safe inputs with address, uint256, and other Solidity types

### Template Variables

The transpiler supports template variables that can be used throughout YAML files:

- `${config.<name>}` - Reference config values
- `${token_list.<chain>.<symbol>}` - Reference token addresses from a token list
- `${helpers.<chain>.<name>}` - Reference helper contract addresses from a helpers list
- `${position.<var>}` - Reference position-level variables

The transpiler outputs TOML rootfiles that can be used to interact with the caliber contracts.

## Integration roots check

CI (`.github/workflows/integration-roots.yaml`) fails a PR when it changes the Merkle root or the
file hash of any rootfile in [makina-integrations](https://github.com/MakinaHQ/makina-integrations)
at the commit pinned in `tests/integration-roots.snapshot`, and summarizes the differences in a PR
comment. To accept an intended change, optionally bump `integrations-ref` and regenerate:

```bash
git clone https://github.com/MakinaHQ/makina-integrations /tmp/integrations
git -C /tmp/integrations checkout <integrations-ref>
cargo build --release --locked
{ echo "integrations-ref: $(git -C /tmp/integrations rev-parse HEAD)"
  scripts/integration-roots.sh target/release/transpiler /tmp/integrations; } > tests/integration-roots.snapshot
```
