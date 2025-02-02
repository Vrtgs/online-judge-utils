use modding_num::Modding;
use std::borrow::Cow;
use std::cell::Cell;
use std::cmp::Reverse;
use std::fmt::Display;
use std::io::Write;
use std::io::{BufRead, BufReader, BufWriter, Read};
use std::num::{Saturating, Wrapping};
use std::ops::{Deref, DerefMut, Not};
use std::str::FromStr;
use std::sync::atomic::{AtomicBool, Ordering};

pub mod modding_num;
pub mod rng;

#[cfg(feature = "heap-array")]
pub mod heap_array {
    pub use ::heap_array::*;
}

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
parse_wrapped! {
    Reverse<T>
    Wrapping<T>
    Saturating<T>
    Modding<T, for{const N: u64}>
}

pub struct TokenReader<'a> {
    data: &'a str,
}

impl<'a> TokenReader<'a> {
    fn next(&mut self, mut f: impl FnMut(char) -> bool) -> Option<&'a str> {
        if self.data.is_empty() {
            return None;
        }

        while let Some(idx) = self
            .data
            .char_indices()
            .find_map(|(i, c)| f(c).then_some(i))
        {
            // all of these are ascii operations, we won't break any utf-8 boundaries
            // and position always returns a value within the iterator
            let ret = unsafe { self.data.get_unchecked(..idx) };
            self.data = unsafe { self.data.get_unchecked(idx + 1..) };
            if ret.is_empty() {
                continue;
            }

            return Some(ret);
        }

        match self.data.is_empty() {
            true => None,
            false => Some(std::mem::take(&mut self.data)),
        }
    }

    #[inline(always)]
    pub fn next_token(&mut self) -> Option<&'a str> {
        self.next(|b| -> bool { b.is_whitespace() })
    }

    #[inline(always)]
    pub fn next_line(&mut self) -> Option<&'a str> {
        self.next(|b| -> bool { matches!(b, '\n' | '\r') })
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
        FIRST_INPUT_THREAD
            .swap(true, Ordering::SeqCst)
            .not()
            .then(|| Box::new(io::stdin().lock()) as Box<dyn BufRead>)
            .map(InputSource)
            .expect("Only 1 thread can take input")
    }
}

struct OutputSource(BufWriter<Box<dyn Write>>);

impl Default for OutputSource {
    fn default() -> Self {
        OutputSource(BufWriter::new(Box::new(io::stdout().lock())))
    }
}

impl Deref for OutputSource {
    type Target = BufWriter<Box<dyn Write>>;

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
struct DroppingOutputSource(RefCell<OutputSource>);

impl Deref for DroppingOutputSource {
    type Target = RefCell<OutputSource>;

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
        self.get_mut()
            .flush()
            .expect("FATAL: output source refused flush")
    }
}

use std::cell::RefCell;
use std::panic::UnwindSafe;
use std::rc::Rc;
use std::{io, rc};

pub struct OutputCapture {
    inner: Rc<RefCell<Vec<u8>>>,
    old_output: Box<dyn Write>,
}

struct OutputInner(rc::Weak<RefCell<Vec<u8>>>);

impl OutputCapture {
    pub fn connect() -> Self {
        let inner = Rc::new(RefCell::new(Vec::new()));
        let old = replace_output(OutputInner(Rc::downgrade(&inner)));
        Self {
            inner,
            old_output: old,
        }
    }

    pub fn capture(self) -> Capture {
        let data =
            Rc::try_unwrap(self.inner).map_or_else(|x| x.replace(Vec::new()), RefCell::into_inner);
        Capture(Ok(data), self.old_output)
    }
}

impl Write for OutputInner {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0
            .upgrade()
            // we got disconnected, no point in doing anything
            .map_or(Ok(buf.len()), |cell| cell.borrow_mut().write(buf))
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[derive(Debug)]
pub struct PoisonedOutput {
    pub panic: Cow<'static, str>,
    pub output: Vec<u8>
}

pub struct Capture(Result<Vec<u8>, PoisonedOutput>, Box<dyn Write>);

impl Capture {
    pub fn flush(self) -> io::Result<()> {
        let data = self.0.as_ref().map_or_else(|e| &*e.output, |x| &**x);
        let mut writer = BufWriter::new(self.1);
        let res = writer.write_all(data);
        set_output_buffered(writer);
        res
    }

    pub fn set_output(self) {
        self.replace_output();
    }

    pub fn replace_output(self) -> Box<dyn Write> {
        replace_output(BufWriter::new(self.1))
    }

    pub fn into_inner(self) -> Result<Vec<u8>, PoisonedOutput> {
        self.0
    }
}

pub fn capture(f: impl FnOnce() + UnwindSafe) -> Capture {
    let output = OutputCapture::connect();

    let res =
        std::panic::catch_unwind(f).map_err(|panic| match panic.downcast_ref::<&'static str>() {
            Some(&s) => Cow::Borrowed(s),
            None => match panic.downcast::<String>() {
                Ok(s) => Cow::Owned(*s),
                Err(_) => Cow::Borrowed("Box<dyn Any>"),
            },
        });

    match res {
        Ok(()) => output.capture(),
        Err(e) => {
            let mut capture = output.capture();
            let err = PoisonedOutput {
                panic: e,
                output: capture.0.unwrap()
            };
            capture.0 = Err(err);
            capture
        }
    }
}

thread_local! {
    static INTERACTIVE: Cell<bool> = const { Cell::new(false) };
}


fn read_line() -> Option<&'static str> {
    let mut buf = String::new();
    let 1.. = INPUT_SOURCE.with(|r| {
        // we don't let borrows escape the current thread, nor the func
        let mut r = r.borrow_mut();
        let res = match INTERACTIVE.get() {
            true => r.read_line(&mut buf),
            false => r.read_to_string(&mut buf)
        };
        
        res.expect("unable to read input to a string")
    }) else {
        return None;
    };

    Some(Box::leak(Box::<str>::from(buf.trim())))
}

#[doc(hidden)]
pub fn __set_interactive() {
    INTERACTIVE.set(true)
}

thread_local! {
    static INPUT_SOURCE: RefCell<InputSource> =
        RefCell::new(InputSource::default());

    static OUTPUT_SOURCE: DroppingOutputSource = DroppingOutputSource::default();

    static CURRENT_TOKENS: Cell<&'static str> = Cell::new(read_line().unwrap_or(""));
}

pub fn set_output(output: impl Write + 'static) {
    set_output_buffered(BufWriter::new(Box::new(output)))
}

pub fn set_output_buffered(output: BufWriter<Box<dyn Write>>) {
    replace_output_buffered(output);
}

pub fn replace_output(output: impl Write + 'static) -> Box<dyn Write> {
    replace_output_buffered(BufWriter::new(Box::new(output)))
}

pub fn replace_output_buffered(output: BufWriter<Box<dyn Write>>) -> Box<dyn Write> {
    OUTPUT_SOURCE.with(|out| {
        // # Safety: we don't let borrows escape the current thread
        std::mem::replace(&mut *out.borrow_mut(), OutputSource(output))
            .0
            .into_inner()
            .unwrap_or_else(|_| panic!("could not flush the old output"))
    })
}

pub fn set_input(input: impl Read + 'static) {
    set_input_buffered(BufReader::new(input))
}

pub fn set_input_buffered(input: impl BufRead + 'static) {
    replace_input_buffered(input);
}

pub fn replace_input(input: impl Read + 'static) -> Box<dyn BufRead> {
    replace_input_buffered(BufReader::new(input))
}

pub fn replace_input_buffered(input: impl BufRead + 'static) -> Box<dyn BufRead> {
    INPUT_SOURCE.with(|r#in| {
        // # Safety: we don't let borrows escape the current thread
        std::mem::replace(&mut *r#in.borrow_mut(), InputSource(Box::new(input))).0
    })
}

/// This is the only correct way to get a reference to a TokenReader
/// you can only call TokenReader methods once
#[doc(hidden)]
fn with_token_reader<F: FnOnce(&mut TokenReader<'static>) -> T, T>(fun: F) -> T {
    CURRENT_TOKENS.with(move |current_tokens| {
        // the end is already trimmed when reading
        match current_tokens.get().trim_start() {
            "" => {
                while let Some(s) = read_line() {
                    match s { 
                        "" => continue,
                        s => { 
                            current_tokens.set(s);
                            break
                        }
                    }
                }
            }
            // might as well benefit from trimming the string
            str => current_tokens.set(str),
        }

        struct ReaderDropGuard<'c, 'a> {
            reader: TokenReader<'a>,
            current_tokens: &'c Cell<&'a str>,
        }

        impl Drop for ReaderDropGuard<'_, '_> {
            fn drop(&mut self) {
                self.current_tokens.set(self.reader.data);
            }
        }

        let mut guard = ReaderDropGuard {
            reader: TokenReader {
                data: current_tokens.get(),
            },
            current_tokens,
        };

        fun(&mut guard.reader)
    })
}

pub mod get_input {
    use super::{with_token_reader, TokenReader};
    
    pub fn current_line() -> Option<&'static str> {
        with_token_reader(TokenReader::next_line)
    }

    pub fn next_token() -> Option<&'static str> {
        with_token_reader(TokenReader::next_token)
    }
}

#[macro_export]
macro_rules! file_io {
    (
        $(in : $in_file : literal $(,)?)?
        $(out: $out_file: literal $(,)?)?
    ) => {{
        $($crate::set_input (::std::fs::File::open  ($in_file ).unwrap());)?
        $($crate:set_outputt(::std::fs::File::create($out_file).unwrap());)?
    }};
    (
        $(out: $out_file: literal $(,)?)?
        $(in : $in_file : literal $(,)?)?
    ) => {file_io!(in: $in_file, out: $out_file)};
}

#[macro_export]
macro_rules! flush {
    () => {
        $crate::__flush()
    };
}

#[macro_export]
macro_rules! parse {
    ($val:expr, $t:ty) => {
        <$t as $crate::Parse>::from_str($val).unwrap()
    };
}

#[macro_export]
macro_rules! interactive_mode {
    () => { let () = $crate::__set_interactive(); };
}

#[macro_export]
macro_rules! input {
    (    ) => { $crate::get_input::next_token  ().expect("Ran out of input") };
    (line) => { $crate::get_input::current_line().expect("Ran out of input") };
    ($t:ty) => { parse!(input!(), $t) };
    ($($t:ty),+ $(,)?) => { ($(input!($t)),+,) };

    [r!($($t:tt)*); $n:expr; Iterator] => {
        (0..($n)).map(|_| input!($($t)*))
    };
    [$t:ty; $n:expr; Iterator] => {
        input![r!($t); $n; Iterator]
    };

    [r!($($t:tt)*); $n:expr; Array; Map($map: expr)] => {{
        #[allow(unused_mut)]
        let mut map = ($map);
        ::std::array::from_fn::<_, {$n as usize}, _>(|_| map(input!($($t)*)))
    }};
    [r!($($t:tt)*); $n:expr; Array] => { input!(r!($($t)*); $n; Array; Map(::std::convert::identity)) };
    [$t:ty; $n:expr; Array]     => { input!(r!($t); $n; Array) };
    [r!($($t:tt)*); $n:literal] => { input![r!($($t)*); $n; Array] };
    [$t:ty; $n:literal] => { input![r!($t); $n] };

    [r!($($t:tt)*); $n:expr; $container: ident; Map($map: expr)] => {
        input![r!($($t)*); $n; Iterator]
            .map($map)
            .collect::<$container<_>>()
    };
    [$t:ty; $n:expr; $container: ident; Map($map: expr)] => {
        input![r!($t); $n; $container; Map($map)]
    };

    [r!($($t:tt)*); $n:expr; $container: ident] => { input![r!($($t)*); $n; $container; Map(::std::convert::identity)] };
    [     $t:ty   ; $n:expr; $container: ident] => { input![r!(  $t  ); $n; $container] };

    [r!($($t:tt)*); $n:expr] => {{ 
        use ::std::boxed::Box;
        type BoxSlice<T> = Box<[T]>;
        
        let x: Box<[_]> = input![r!($($t)*); $n; BoxSlice];
        x
    }};
    [     $t:ty   ; $n:expr] => {{ input![r!($t); $n] }};
}

#[doc(hidden)]
pub fn __output<I: IntoIterator<Item = D>, D: Display>(iter: I) {
    const WRITE_ERR_MSG: &str = "unable to write to output";

    OUTPUT_SOURCE.with(|out| {
        // we don't let borrows escape the current thread, not the func
        let out = &mut **out.borrow_mut();
        let mut iter = iter.into_iter();
        if let Some(first) = iter.next() {
            write!(out, "{}", first).expect(WRITE_ERR_MSG);
            for x in iter {
                write!(out, " {}", x).expect(WRITE_ERR_MSG);
            }
        }

        out.write_all(b"\n").expect(WRITE_ERR_MSG);

        if cfg!(debug_assertions) {
            let _ = out.flush();
        }
    })
}

#[doc(hidden)]
pub fn __flush() {
    OUTPUT_SOURCE.with(|out| {
        // we don't let borrows escape the current thread, not the func
        out
            .borrow_mut()
            .flush()
            .expect("unable to flush stdout");
    })
}

#[macro_export]
macro_rules! output {
    (one  $x: expr) => {
        output!(iter {::std::iter::once($x)})
    };
    (iter $x: expr) => {
        $crate::__output(($x).into_iter())
    };
    (chars $x: expr) => {{
        let iter = $x;

        // avoid having them in the same scope
        {
            use ::core::cell::Cell;
            use ::core::option::Option;
            use ::core::iter::Iterator;
            use ::core::fmt::{Write, Formatter, Result, Display};
            struct DisplayChars<I>(Cell<Option<I>>);

            impl<I: Iterator<Item=char>> Display for DisplayChars<I> {
                fn fmt(&self, f: &mut Formatter<'_>) -> Result {
                    for c in self.0.take().unwrap() {
                        f.write_char(c)?
                    }
                    Ok(())
                }
            }

        output!(one DisplayChars(Cell::new(Option::Some(iter))))
    }}};
}

#[macro_export]
macro_rules! min {($first: expr $(, $other: expr)+ $(,)?) => {($first)$(.min($other))+};}
#[macro_export]
macro_rules! max {($first: expr $(, $other: expr)+ $(,)?) => {($first)$(.max($other))+};}
