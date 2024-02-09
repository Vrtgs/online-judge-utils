use std::cell::Cell;
use std::num::Wrapping;

// reqwest xor-shift
#[inline(always)]
fn xor_shift(rng: &Cell<Wrapping<u64>>) -> u64 {
    let mut n = rng.get();
    debug_assert_ne!(n.0, 0);
    n ^= n >> 12;
    n ^= n << 25;
    n ^= n >> 27;
    rng.set(n);
    n.0.wrapping_mul(0x2545_f491_4f6c_dd1d)
}


/// Truly random auto seeded integers
pub fn rand() -> u64 {
    use std::collections::hash_map::RandomState;
    use std::hash::{BuildHasher};

    thread_local! {
        static RNG: Cell<Wrapping<u64>> = Cell::new(Wrapping(seed()));
    }

    fn seed() -> u64 {
        let seed = RandomState::new();

        let mut out = 0;
        let mut cnt = 0;
        while out == 0 {
            cnt += 1;
            out = seed.hash_one(cnt);
        }
        out
    }

    RNG.with(xor_shift)
}

/// Consistently produce the same pseudo-random numbers (unrelated)
pub fn const_rand() -> u64 {
    thread_local! {
        // 6684531970241121646 is random, because I said so
        static RNG: Cell<Wrapping<u64>> = Cell::new(Wrapping(6684531970241121646));
    }

    RNG.with(xor_shift)
}