use std::cell::UnsafeCell;
use std::cmp::Reverse;
use std::str::FromStr;
use std::io::Read;
use std::num::{Wrapping, Saturating};
use std::sync::atomic::{AtomicBool, Ordering};
use modding_num::Modding;

#[cfg(feature = "heap-array")]
pub mod heap_array { pub use ::heap_array::*; }
pub mod rng;
pub mod modding_num;

/// [`FromStr`] with lifetime support
pub trait Parse<'a>: Sized {
    type Err;
    fn from_str(s: &'a str) -> Result<Self, Self::Err>;
}

macro_rules! parse_upstream {
    ($($T:ty)*) => {$(
        impl<'a> Parse<'a> for $T {
            type Err = <$T as FromStr>::Err;

            #[inline(always)]
            fn from_str(s: &'a str) -> Result<Self, Self::Err> {
                <$T as FromStr>::from_str(s)
            }
        }
    )*};
}
macro_rules! parse_non_zero {
    ($($T:ident |> $real_T: ty)*) => {$(
        impl<'a> Parse<'a> for std::num::$T {
            type Err = <std::num::$T as FromStr>::Err;
            #[inline(always)]
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                <std::num::$T as FromStr>::from_str(s)
            }
        }
        impl<'a> Parse<'a> for Option<std::num::$T> {
            type Err = <$real_T as FromStr>::Err;

            #[inline(always)]
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match <$real_T as FromStr>::from_str(s) {
                    Ok(num) => Ok(std::num::$T::new(num)),
                    Err(e) => Err(e),
                }
            }
        }
    )*};
}
macro_rules! parse_wrapped {
    ($($Wrapper:ident<$T:ident $(, for{const $generic: ident: $gen_ty: ty})?>)*) => {$(
        impl<'a, $T: Parse<'a>$(, const $generic: $gen_ty)?> Parse<'a> for $Wrapper<$T $(, $generic)?> {
            type Err = <T as Parse<'a>>::Err;

            #[inline(always)]
            fn from_str(s: &'a str) -> Result<Self, Self::Err> {
                <$T as Parse<'a>>::from_str(s).map($Wrapper)
            }
        }
    )*};
}

impl<'a> Parse<'a> for &'a str {
    type Err = std::convert::Infallible;

    #[inline(always)]
    fn from_str(s: &'a str) -> Result<Self, Self::Err> {
        Ok(s)
    }
}

parse_upstream! {
    f32 f64 i8 i16 i32 i64 isize i128 u8 u16 u32 u64 usize u128
    char bool String
}
parse_non_zero! {
    NonZeroI8 |> i8
    NonZeroU8 |> u8

    NonZeroI16 |> i16
    NonZeroU16 |> u16

    NonZeroI32 |> i32
    NonZeroU32 |> u32

    NonZeroI64 |> i64
    NonZeroU64 |> u64
}
parse_wrapped!  {
    Reverse<T>
    Wrapping<T>
    Saturating<T>
    Modding<T, for{const N: u64}>
}


#[repr(transparent)]
pub struct TokenReader<'a> {
    data: &'a str,
}

impl<'a> TokenReader<'a> {
    fn _next(&mut self, mut f: impl FnMut(u8) -> bool) -> Option<&'a str> {
        if self.data.is_empty() {
            return None
        }

        while let Some(idx) = self.data.bytes().position(&mut f) {
            // all of these are ascii operations, we won't break any utf-8 boundaries
            // and position always returns a value within the iterator
            let ret = unsafe { self.data.get_unchecked(..idx) };
            self.data = unsafe { self.data.get_unchecked(idx + 1..) };
            if ret.is_empty() { continue }

            return Some(ret)
        }

        match self.data.is_empty() {
            true => None,
            false => Some(std::mem::take(&mut self.data))
        }
    }

    #[inline(always)]
    pub fn next_token(&mut self) -> Option<&'a str> {
        self._next(|b: u8| -> bool { b.is_ascii_whitespace() })
    }

    #[inline(always)]
    pub fn next_line(&mut self) -> Option<&'a str> {
        self._next(|b: u8| -> bool { matches!(b, b'\n' | b'\r') })
    }
}

impl<'a> Iterator for TokenReader<'a> {
    type Item = &'a str;

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

static FIRST_INPUT_THREAD: AtomicBool = AtomicBool::new(false);

thread_local! {
    static TOKEN_READER: UnsafeCell<TokenReader<'static>> = {
        if FIRST_INPUT_THREAD.swap(true, Ordering::SeqCst) {
            panic!("Only 1 thread can take input")
        }

        let mut buf = String::with_capacity(1024 * 1024);
        std::io::stdin().lock().read_to_string(&mut buf).unwrap();
        buf.shrink_to_fit();
        UnsafeCell::new(TokenReader {
            data: buf.leak()
        })
    }
}


/// This is the only safe way to get a reference to TOKEN_READER
#[doc(hidden)]
pub fn with_token_reader<F: FnOnce(&mut TokenReader<'static>) -> T, T>(fun: F) -> T {
    TOKEN_READER.with(move |r_ptr| {
        // Safety:
        // we never let the initial reference of &mut TokenReader escape
        let r = unsafe { &mut *r_ptr.get() };
        fun(r)
    })
}

#[macro_export]
macro_rules! parse {
    ($val:expr, $t:ty) => { <$t as $crate::Parse>::from_str($val).unwrap() };
}

#[macro_export]
macro_rules! read {
    (    ) => { $crate::with_token_reader(|r| r.next_token()).expect("Ran out of input") };
    (line) => { $crate::with_token_reader(|r| r.next_line ()).expect("Ran out of input") };
    ($t:ty) => { $crate::parse!(read!(), $t) };
    ($($t:ty),+ $(,)?) => { ($($crate::read!($t)),+) };

    [r!($($t:tt)*); $n:expr; Array; Map($map: expr)] => {{
        #[allow(unused_mut)]
        let mut map = ($map);
        ::std::array::from_fn::<_, {$n as usize}, _>(|_| map($crate::read!($($t)*)))
    }};
    [r!($($t:tt)*); $n:expr; Array] => { $crate::read!(r!($($t)*); $n; Array; Map(|x| x)) };
    [$t:ty; $n:expr; Array]     => { $crate::read!(r!($t); $n; Array) };
    [r!($($t:tt)*); $n:literal] => { $crate::read![r!($($t)*); $n; Array] };
    [$t:ty; $n:literal] => { $crate::read![r!($t); $n] };

    [r!($($t:tt)*); $n:expr; $container: ident; Map($map: expr)] => {
        (0..({$n} as usize))
            .map(|_| read!($($t)*))
            .map($map)
            .collect::<$container<_>>()
    };
    [$t:ty; $n:expr; $container: ident; Map($map: expr)] => {
        $crate::read!(r!($t); $n; $container; Map($map))
    };

    [r!($($t:tt)*); $n:expr; $container: ident] => { $crate::read!(r!($($t)*); $n; $container; Map(|x| x)) };
    [     $t:ty   ; $n:expr; $container: ident] => { $crate::read!(r!(  $t  ); $n; $container) };

    [r!($($t:tt)*); $n:expr] => {{ use ::std::vec::Vec; $crate::read![r!($($t)*); $n; Vec] }};
    [     $t:ty   ; $n:expr] => { $crate::read![r!($t); $n; Vec] };
}

#[macro_export] macro_rules! min {($first: expr $(, $other: expr)+) => {($first)$(.min($other))+};}
#[macro_export] macro_rules! max {($first: expr $(, $other: expr)+) => {($first)$(.max($other))+};}