use std::cell::{UnsafeCell};
use std::cmp::Reverse;
use std::fmt::Display;
use std::str::FromStr;
use std::io::{BufRead, BufReader, BufWriter, Read};
use std::io::Write;
use std::num::{Wrapping, Saturating};
use std::ops::{Deref, DerefMut, Not};
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
    f32 f64
    i8 i16 i32 i64 isize i128
    u8 u16 u32 u64 usize u128
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

    NonZeroI128 |> i128
    NonZeroU128 |> u128

    NonZeroUsize |> usize
    NonZeroIsize |> isize
}
parse_wrapped!  {
    Reverse<T>
    Wrapping<T>
    Saturating<T>
    Modding<T, for{const N: u64}>
}

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

struct InputSource(Box<dyn BufRead>);

impl Deref for InputSource {
    type Target = dyn BufRead;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl DerefMut for InputSource {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *self.0
    }
}
impl Default for InputSource {
    fn default() -> InputSource {
        FIRST_INPUT_THREAD.swap(true, Ordering::SeqCst)
            .not()
            .then(|| Box::new(std::io::stdin().lock()) as Box<dyn BufRead>)
            .map(InputSource)
            .expect("Only 1 thread can take input")
    }
}

struct OutputSource(BufWriter<Box<dyn Write>>);

impl Default for OutputSource {
    fn default() -> Self {
        OutputSource(BufWriter::new(Box::new(std::io::stdout().lock())))
    }
}

impl Deref for OutputSource {
    type Target = BufWriter<dyn Write>;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for OutputSource {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Default)]
struct DroppingOutputSource(UnsafeCell<OutputSource>);

impl Deref for DroppingOutputSource {
    type Target = UnsafeCell<OutputSource>;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for DroppingOutputSource {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Drop for DroppingOutputSource {
    fn drop(&mut self) {
        self.get_mut().flush().expect("FATAL: output source refused flush")
    }
}

thread_local! {
    static INPUT_SOURCE: UnsafeCell<InputSource> =
        UnsafeCell::new(InputSource::default());

    static OUTPUT_SOURCE: DroppingOutputSource = DroppingOutputSource::default();

    static TOKEN_READER: UnsafeCell<TokenReader<'static>> = {
        let mut buf = String::with_capacity(1024 * 1024);
        INPUT_SOURCE.with(|r| {
            // we don't let borrows escape the current thread, not the func
            unsafe {&mut **r.get()}
                .read_to_string(&mut buf)
                .expect("unable to read input to a string")
        });

        buf.shrink_to_fit();
        UnsafeCell::new(TokenReader {
            data: buf.leak()
        })
    }
}

pub fn set_output(output: impl Write + 'static) {
    OUTPUT_SOURCE.with(|out| {
        // we don't let borrows escape the current thread, not the func
        let mut out = std::mem::replace(
            unsafe { &mut *out.get() },
            OutputSource(BufWriter::new(Box::new(output)))
        );
        out.flush().expect("could not flush the old output");
        // make sure its dropped after to avoid some weird deadlock
        drop(out)
    })
}

pub fn set_input(input: impl Read + 'static) {
    set_buffered_input(BufReader::new(input))
}

pub fn set_buffered_input(input: impl BufRead + 'static) {
    INPUT_SOURCE.with(|r#in| {
        // we don't let borrows escape the current thread, not the func

        let input = std::mem::replace(
            unsafe { &mut *r#in.get() },
            InputSource(Box::new(input))
        );
        // make sure its dropped after to avoid some weird deadlock
        drop(input)
    })
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
macro_rules! file_io {
    (
        $(in : $in_file : literal $(,)?)?
        $(out: $out_file: literal $(,)?)?
    ) => {{
        $($crate::set_input (::std::fs::File::open  ($in_file ).unwrap());)?
        $($crate::set_output(::std::fs::File::create($out_file).unwrap());)?
    }};
    (
        $(out: $out_file: literal $(,)?)?
        $(in : $in_file : literal $(,)?)?
    ) => {$crate::file_io!(in: $in_file, out: $out_file)};
}

#[macro_export]
macro_rules! flush {
    () => { $crate::__flush() }
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

#[doc(hidden)]
pub fn __output<I: IntoIterator<Item=D>, D: Display>(iter: I) {
    const WRITE_ERR_MSG: &str = "unable to write to output";

    OUTPUT_SOURCE.with(|out| {
        // we don't let borrows escape the current thread, not the func
        let out = unsafe { &mut **out.get() };
        let mut iter = iter.into_iter();
        if let Some(first) = iter.next() {
            write!(out, "{first}").expect(WRITE_ERR_MSG);
            for x in iter {
                write!(out, " {x}").expect(WRITE_ERR_MSG);
            }
        }

        out.write_all(b"\n").expect(WRITE_ERR_MSG);

        if cfg!(debug_assertions) { let _ = out.flush(); }
    })
}

#[doc(hidden)]
pub fn __flush() {
    OUTPUT_SOURCE.with(|out| {
        // we don't let borrows escape the current thread, not the func
        unsafe { &mut **out.get() }.flush().expect("unable to flush stdout");
    })
}

#[macro_export]
macro_rules! output {
    (one  $x: expr) => { $crate::__output(Some($x)) };
    (iter $x: expr) => { $crate::__output($x) };
}

#[macro_export] macro_rules! min {($first: expr $(, $other: expr)+) => {($first)$(.min($other))+};}
#[macro_export] macro_rules! max {($first: expr $(, $other: expr)+) => {($first)$(.max($other))+};}