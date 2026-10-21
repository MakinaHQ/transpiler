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

### Encoding calldata: `abi_encode_call`

A `bytes` parameter can be the calldata of another call, built from typed parameters.
Typical use: `execTransactionFromModule(to, value, data, operation)` on a Safe the caliber is
a module of.

```yaml
- type: "bytes"
  value:
    abi_encode_call:
      selector: "transfer(address,uint256)"
      math_helper: ${constants.math_helper}   # only needed for runtime parameters
      parameters:
        - type: "address"
          value: ${inputs.recipient}
        - type: "uint256"
          value: ${returns.balance}
```

- **Compile-time:** when every parameter is a literal, constant or input, the calldata is
  folded into a single `bytes` literal, committed in the leaf like any other constant.
  Any ABI type works, including arrays, tuples and nested `abi_encode_call`.
- **Runtime:** parameters may also be `${returns.*}` or `${input_slots.*}`. Weiroll cannot
  build `bytes`, so the transpiler emits it as a dynamic tuple `[length, w0, .., wn]`. Its
  tail is byte-identical to the `bytes` argument (see weiroll-rs `Value::DynamicTuple`).
  The 4-byte selector shifts every parameter across a word boundary, so each runtime word
  is split on-chain with `math_helper`: `div`, `mul`, `sub`, `mul`, then one `add` to join
  it with its neighbour. Constant words and the shift constants are committed in the leaf.
  Restrictions in this mode:
  - every parameter must be a one-word static type (`address`, `bool`, `uintN`, `intN`,
    `bytesN`);
  - runtime values must be passed directly, not nested;
  - `math_helper` must implement `add`, `sub`, `mul` and `div` on `uint256` with checked
    arithmetic and `sub(a, b) = a - b` (makina-periphery `MathHelper`). A helper with
    reversed `sub` underflows and reverts rather than producing wrong calldata.

See `test_data/blueprints/safe-module.yaml`.
