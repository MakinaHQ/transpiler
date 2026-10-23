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

### Inputs

A blueprint's `inputs` and the values an instruction supplies for them are one-word
types: `address`, `bool`, `uintN`, `intN`, `bytesN`. An input is a literal fixed per
instruction, and weiroll passes a static literal as exactly one 32-byte word. An
instruction must supply each input with exactly the declared type, and only inputs the
blueprint declares. For a string key use a `bytes32` `${keccak256(...)}`. For fixed
bytes, strings or arrays use a constant or an inline value in the blueprint; for data
chosen at execution time, an input slot; to build calldata, `abi_encode_call`. (Input
slots keep their wider set of types.)

### Call signatures

Every `selector` (and every `abi_encode_call` selector) must be the canonical Solidity
signature, the exact string the selector is hashed from: `transfer(address,uint256)`.
Whitespace is tolerated only around `,` `(` `)` `[` `]`. Anything else is a compile error
naming the canonical form when there is one: aliases (`uint`), `tuple(...)`, trailing
commas, parameter names, struct names, and types Solidity does not have (`uint7`,
`bytes33`, `fixed128x18`).

The declared parameters must fit the signature: same count, and each declared type equal
to the signature's, element by element inside tuples and arrays. The only exceptions are
encoding-identical idioms:
- a one-word value into `bytes32`, an untyped word (key-value store, helpers);
- an integer into an integer of the same signedness and another width (the callee's ABI
  decoder rejects a value that does not fit);
- a multi-value return captured raw (a tuple-typed return) into `bytes`;
- a runtime `bytes` already holding an encoded struct, into that dynamic struct.

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
  - `math_helper` must implement `add`, `sub`, `mul` and `div` on `uint256` with the usual
    semantics, `sub(a, b) = a - b` (makina-periphery `MathHelper`). No operation can
    overflow with such a helper, so checked arithmetic is not needed. A helper with reversed
    `sub` reverts if its arithmetic is checked, and produces wrong calldata if it is not:
    pin a known, immutable helper. Its address is a call target, so it is committed in the
    leaf like any other.
  - A weiroll command holds at most 32 argument slots. The bytes argument alone takes the
    number of words + 3 (tuple start and end, length), so runtime-built calldata is limited
    to about 28 words (≈ 27 parameters), minus the other arguments of the call. Exceeding
    this is a compile error.

See `test_data/blueprints/safe-module.yaml`.
