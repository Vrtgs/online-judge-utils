use std::ops::{
    Add, AddAssign, Div, DivAssign, Mul, MulAssign, Rem, RemAssign, Shl, ShlAssign, Shr, ShrAssign,
    Sub, SubAssign,
};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default, Hash)]
#[repr(transparent)]
pub struct Modding<T, const N: u64>(pub T);

// from std
// implements binary operators "&T op U", "T op &U", "&T op &U"
// based on "T op U" where T and U are expected to be `Copy`able
macro_rules! forward_ref_binop {
    ($imp:ident, $method:ident, for$(<{$($generic:tt)*}>)?, $t:ty, $u:ty) => {
        impl<'a $(, $($generic)*)?> $imp<$u> for &'a $t {
            type Output = <$t as $imp<$u>>::Output;

            #[inline(always)]
            fn $method(self, other: $u) -> <$t as $imp<$u>>::Output {
                $imp::$method(*self, other)
            }
        }

        impl$(<$($generic)*>)? $imp<&$u> for $t {
            type Output = <$t as $imp<$u>>::Output;

            #[inline(always)]
            fn $method(self, other: &$u) -> <$t as $imp<$u>>::Output {
                $imp::$method(self, *other)
            }
        }

        impl$(<$($generic)*>)? $imp<&$u> for &$t {
            type Output = <$t as $imp<$u>>::Output;

            #[inline(always)]
            fn $method(self, other: &$u) -> <$t as $imp<$u>>::Output {
                $imp::$method(*self, *other)
            }
        }
    }
}

trait GetRemainder<const N: u64>: Sized + Copy + Rem<Self, Output = Self> {
    const MOD_BY: Self;
}

macro_rules! impl_prim_remainder {
    ($($T: ty)+) => {$(
        impl<const N: u64> GetRemainder<N> for $T {
            const MOD_BY: $T = {
                #[allow(unused_comparisons)]
                let can_mod_by: bool = (N as $T) as u64 == N && (N as $T) >= 0;
                assert!(can_mod_by);
                N as $T
            };
        }
    )+};
}

impl_prim_remainder! {
    u64 u32 u16 u8
    i64 i32 i16 i8

    usize isize
}

macro_rules! impl_binop {
    ($(($trait: ident, $meth: ident))*) => {$(
        impl<T: GetRemainder<N> + $trait<Output=T>, const N: u64> $trait for Modding<T, N> {
            type Output = Self;

            #[inline(always)]
            fn $meth(self, rhs: Modding<T, N>) -> Self::Output {
                // we intentionally mod
                #[allow(clippy::suspicious_arithmetic_impl)]
                Modding::<T, N>(self.0.$meth(rhs.0) % <T as GetRemainder<N>>::MOD_BY)
            }
        }

        impl<T: GetRemainder<N> + $trait<Output=T>, const N: u64> $trait<T> for Modding<T, N> {
            type Output = Self;

            #[inline(always)]
            fn $meth(self, rhs: T) -> Self::Output {
                // we intentionally mod
                #[allow(clippy::suspicious_arithmetic_impl)]
                Modding::<T, N>(self.0.$meth(rhs) % <T as GetRemainder<N>>::MOD_BY)
            }
        }

        forward_ref_binop!($trait, $meth, for<{
            T: GetRemainder<N> + $trait<Output=T>,
            const N: u64
        }>, Modding<T, N>, Modding<T, N>);

        forward_ref_binop!($trait, $meth, for<{
            T: GetRemainder<N> + $trait<Output=T>,
            const N: u64
        }>, Modding<T, N>, T);
    )*};
}

macro_rules! impl_assign_binop {
    ($(($trait: ident, $meth: ident = $other_trait: ident, $other_meth: ident))*) => {$(
        impl<T: GetRemainder<N> + $other_trait<Output=T> + $trait, const N: u64> $trait for Modding<T, N> {
            #[inline(always)]
            fn $meth(&mut self, rhs: Self) {
                *self = $other_trait::$other_meth(*self, rhs)
            }
        }

        impl<T: GetRemainder<N> + $other_trait<Output=T> + $trait, const N: u64> $trait<&Self> for Modding<T, N> {
            #[inline(always)]
            fn $meth(&mut self, rhs: &Self) {
                *self = $other_trait::$other_meth(*self, *rhs)
            }
        }

        impl<T: GetRemainder<N> + $other_trait<Output=T> + $trait, const N: u64> $trait<T> for Modding<T, N> {
            #[inline(always)]
            fn $meth(&mut self, rhs: T) {
                *self = $other_trait::$other_meth(*self, rhs)
            }
        }

        impl<T: GetRemainder<N> + $other_trait<Output=T> + $trait, const N: u64> $trait<&T> for Modding<T, N> {
            #[inline(always)]
            fn $meth(&mut self, rhs: &T) {
                *self = $other_trait::$other_meth(*self, *rhs)
            }
        }
    )*};
}

impl_binop! {
    (Add, add)
    (Sub, sub)
    (Div, div)
    (Mul, mul)
    (Rem, rem)
    (Shl, shl)
    (Shr, shr)
}

impl_assign_binop! {
    (AddAssign, add_assign = Add, add)
    (SubAssign, sub_assign = Sub, sub)
    (DivAssign, div_assign = Div, div)
    (MulAssign, mul_assign = Mul, mul)
    (RemAssign, rem_assign = Rem, rem)
    (ShlAssign, shl_assign = Shl, shl)
    (ShrAssign, shr_assign = Shr, shr)
}
