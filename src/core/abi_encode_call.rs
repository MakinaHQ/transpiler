//! Runtime lowering of `abi_encode_call` into weiroll commands.
//!
//! Weiroll cannot build a `bytes` value: a dynamic argument is copied verbatim from a
//! state element. But a dynamic tuple of static words is encoded as an offset plus the
//! words in the tail, which is byte-identical to a `bytes` argument whose content is
//! those words. So the calldata `selector ‖ a_0 ‖ .. ‖ a_{n-1}` (4 + 32n bytes) is emitted
//! as `DynamicTuple([4 + 32n, w_0, .., w_n])`.
//!
//! The 4-byte selector shifts every argument across a word boundary:
//!
//! ```text
//! w_0 = selector << 224          | a_0 >> 32
//! w_j = (a_{j-1} & 0xffffffff) << 224 | a_j >> 32        (0 < j < n)
//! w_n = (a_{n-1} & 0xffffffff) << 224
//! ```
//!
//! Constant arguments are split at compile time. A runtime argument `v` is split on-chain
//! with a math helper (`hi = v / 2^32`, `lo = (v - hi * 2^32) * 2^224`), and the two
//! halves of a word are joined with `add` (their bits never overlap). The helper must
//! implement `add`, `sub`, `mul` and `div` on `uint256` with checked arithmetic and the
//! usual operand order (`sub(a, b) = a - b`), like makina-periphery's `MathHelper`.

use alloy::primitives::{B256, Selector, U256, keccak256};

/// A 32-byte argument of the encoded call.
#[derive(Debug, Clone)]
pub enum ArgWord<V> {
    Const(B256),
    Runtime(V),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MathOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl MathOp {
    pub fn selector(self) -> Selector {
        let signature = match self {
            MathOp::Add => "add(uint256,uint256)",
            MathOp::Sub => "sub(uint256,uint256)",
            MathOp::Mul => "mul(uint256,uint256)",
            MathOp::Div => "div(uint256,uint256)",
        };
        Selector::from_slice(&keccak256(signature)[..4])
    }
}

/// Where the lowering emits its values: weiroll commands in the transpiler, plain
/// arithmetic in tests.
pub trait Emitter {
    type Value: Clone;

    fn literal(&mut self, word: U256) -> Self::Value;
    fn op(&mut self, op: MathOp, a: Self::Value, b: Self::Value) -> Self::Value;
}

/// The `(hi, lo)` halves of an argument: `a >> 32` and `(a & 0xffffffff) << 224`.
type Halves<V> = (Term<V>, Term<V>);

enum Term<V> {
    Const(U256),
    Runtime(V),
}

const SHIFT_32: U256 = U256::from_limbs([1 << 32, 0, 0, 0]);
const SHIFT_224: U256 = U256::from_limbs([0, 0, 0, 1 << 32]);

/// Returns the encoded length in bytes and the words of `selector ‖ args`, ready to be
/// wrapped as `DynamicTuple([length, words..])`.
pub fn lower<E: Emitter>(
    emitter: &mut E,
    selector: Selector,
    args: Vec<ArgWord<E::Value>>,
) -> (usize, Vec<E::Value>) {
    let length = 4 + 32 * args.len();

    let halves: Vec<Halves<E::Value>> = args
        .into_iter()
        .map(|arg| match arg {
            ArgWord::Const(word) => {
                let word = U256::from_be_bytes(word.0);
                let hi = word >> 32;
                let lo = (word - (hi << 32)) << 224;
                (Term::Const(hi), Term::Const(lo))
            }
            ArgWord::Runtime(value) => {
                let shift_32 = emitter.literal(SHIFT_32);
                let hi = emitter.op(MathOp::Div, value.clone(), shift_32.clone());
                let hi_shifted_back = emitter.op(MathOp::Mul, hi.clone(), shift_32);
                let lo = emitter.op(MathOp::Sub, value, hi_shifted_back);
                let shift_224 = emitter.literal(SHIFT_224);
                let lo = emitter.op(MathOp::Mul, lo, shift_224);
                (Term::Runtime(hi), Term::Runtime(lo))
            }
        })
        .collect();

    let mut words = Vec::with_capacity(halves.len() + 1);
    let mut previous_lo = Term::Const(U256::from_be_slice(selector.as_slice()) << 224);
    for (hi, lo) in halves {
        words.push(join(emitter, previous_lo, hi));
        previous_lo = lo;
    }
    words.push(join(emitter, previous_lo, Term::Const(U256::ZERO)));

    (length, words)
}

/// `a + b`, where `a` and `b` occupy disjoint bits.
fn join<E: Emitter>(emitter: &mut E, a: Term<E::Value>, b: Term<E::Value>) -> E::Value {
    match (a, b) {
        (Term::Const(a), Term::Const(b)) => emitter.literal(a | b),
        (Term::Const(c), Term::Runtime(v)) | (Term::Runtime(v), Term::Const(c)) if c.is_zero() => v,
        (Term::Const(c), Term::Runtime(v)) => {
            let c = emitter.literal(c);
            emitter.op(MathOp::Add, c, v)
        }
        (Term::Runtime(v), Term::Const(c)) => {
            let c = emitter.literal(c);
            emitter.op(MathOp::Add, v, c)
        }
        (Term::Runtime(a), Term::Runtime(b)) => emitter.op(MathOp::Add, a, b),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use alloy::{
        primitives::{Address, address},
        sol,
        sol_types::SolCall,
    };

    sol! {
        function transfer(address to, uint256 amount);
        function mixed(uint256 a, address b, bool c, bytes32 d, int256 e);
    }

    /// Evaluates the lowering with checked arithmetic, as the helper does on-chain.
    struct Numeric {
        ops: Vec<MathOp>,
    }

    impl Emitter for Numeric {
        type Value = U256;

        fn literal(&mut self, word: U256) -> U256 {
            word
        }

        fn op(&mut self, op: MathOp, a: U256, b: U256) -> U256 {
            self.ops.push(op);
            match op {
                MathOp::Add => a.checked_add(b).expect("add overflow"),
                MathOp::Sub => a.checked_sub(b).expect("sub underflow"),
                MathOp::Mul => a.checked_mul(b).expect("mul overflow"),
                MathOp::Div => a.checked_div(b).expect("div by zero"),
            }
        }
    }

    fn word(value: U256) -> B256 {
        B256::from(value.to_be_bytes::<32>())
    }

    fn concat(words: &[U256], length: usize) -> Vec<u8> {
        let bytes: Vec<u8> = words.iter().flat_map(|w| w.to_be_bytes::<32>()).collect();
        assert!(
            bytes[length..].iter().all(|b| *b == 0),
            "padding must be zero"
        );
        bytes[..length].to_vec()
    }

    const CALIBER: Address = address!("dc385c31c322351dac9ff4d7ded24e29b5d94768");

    #[test]
    fn transfer_with_runtime_amount_matches_abi_encoding() {
        // Low 4 bytes non-zero: the split is really exercised.
        for amount in [
            U256::ZERO,
            U256::from(1),
            U256::from(40_000_000_000_123_456_789u128),
            U256::MAX,
        ] {
            let mut emitter = Numeric { ops: vec![] };
            let (length, words) = lower(
                &mut emitter,
                transferCall::SELECTOR.into(),
                vec![
                    ArgWord::Const(CALIBER.into_word()),
                    ArgWord::Runtime(amount),
                ],
            );
            let expected = transferCall {
                to: CALIBER,
                amount,
            }
            .abi_encode();
            assert_eq!(length, expected.len());
            assert_eq!(concat(&words, length), expected);
            // div, mul, sub, mul to split the amount; one add to join it to the caliber
            assert_eq!(emitter.ops.len(), 5);
        }
    }

    #[test]
    fn every_argument_at_runtime_matches_abi_encoding() {
        let a = U256::from(0xdead_beef_u64) << 100;
        let b = address!("00000000000000000000000000000000000e1c0d");
        let d = B256::repeat_byte(0xab);
        let e = U256::MAX - U256::from(41); // int256(-42) as bits
        let mut emitter = Numeric { ops: vec![] };
        let (length, words) = lower(
            &mut emitter,
            mixedCall::SELECTOR.into(),
            vec![
                ArgWord::Runtime(a),
                ArgWord::Runtime(U256::from_be_slice(b.into_word().as_slice())),
                ArgWord::Const(word(U256::from(1))),
                ArgWord::Runtime(U256::from_be_bytes(d.0)),
                ArgWord::Runtime(e),
            ],
        );
        let expected = mixedCall {
            a,
            b,
            c: true,
            d,
            e: alloy::primitives::I256::from_raw(e),
        }
        .abi_encode();
        assert_eq!(concat(&words, length), expected);
    }

    #[test]
    fn constant_arguments_emit_no_operations() {
        let mut emitter = Numeric { ops: vec![] };
        let (length, words) = lower(
            &mut emitter,
            transferCall::SELECTOR.into(),
            vec![
                ArgWord::Const(CALIBER.into_word()),
                ArgWord::Const(word(U256::from(123))),
            ],
        );
        let expected = transferCall {
            to: CALIBER,
            amount: U256::from(123),
        }
        .abi_encode();
        assert_eq!(concat(&words, length), expected);
        assert!(emitter.ops.is_empty());
    }

    #[test]
    fn shift_constants() {
        assert_eq!(SHIFT_32, U256::from(1) << 32);
        assert_eq!(SHIFT_224, U256::from(1) << 224);
        assert_eq!(
            MathOp::Div.selector(),
            Selector::from_slice(&keccak256("div(uint256,uint256)")[..4])
        );
    }
}
