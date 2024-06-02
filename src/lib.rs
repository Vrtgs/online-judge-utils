use modding_num::Modding;
use std::borrow::Cow;
use std::cell::UnsafeCell;
use std::cmp::Reverse;
use std::fmt::Display;
use std::io::Write;
use std::io::{BufRead, BufReader, BufWriter, Read};
use std::num::{Saturating, Wrapping};
use std::ops::{Deref, DerefMut, Not};
use std::str::FromStr;
use std::sync::atomic::{AtomicBool, Ordering};

pub mod bumpy {
    use std::alloc::{GlobalAlloc, Layout};
    use std::cell::UnsafeCell;
    use std::mem::MaybeUninit;
    use std::ptr;
    use std::sync::atomic::{AtomicUsize, Ordering};

    pub struct Bumpy<const LEN: usize> {
        curr: AtomicUsize,
        arena: UnsafeCell<[MaybeUninit<u8>; LEN]>,
    }

    unsafe impl<const LEN: usize> Send for Bumpy<LEN> {}
    unsafe impl<const LEN: usize> Sync for Bumpy<LEN> {}

    impl<const LEN: usize> Bumpy<LEN> {
        pub const fn empty() -> Self {
            Self {
                curr: AtomicUsize::new(0),
                arena: UnsafeCell::new(
                    // [MaybeUninit<u8>; LEN] is safe ot not be init
                    unsafe { MaybeUninit::uninit().assume_init() },
                ),
            }
        }
    }

    unsafe impl<const LEN: usize> GlobalAlloc for Bumpy<LEN> {
        unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
            let alloc_len = (layout.size() + layout.align()) - 1;
            let curr = self.curr.fetch_add(alloc_len, Ordering::Relaxed);

            if (curr + alloc_len) >= LEN {
                return ptr::null_mut();
            }

            let ptr = self.arena.get().cast::<u8>().add(curr);
            let offset = ptr.align_offset(layout.align());

            if offset >= layout.align() {
                return ptr::null_mut();
            }

            ptr.add(offset)
        }

        #[inline(always)]
        unsafe fn dealloc(&self, _: *mut u8, _: Layout) {}
    }
}

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

pub struct Capture(Result<Vec<u8>, Cow<'static, str>>, Box<dyn Write>);

impl Capture {
    pub fn flush(self) -> io::Result<()> {
        let data = self.0.as_ref().map_or_else(|e| e.as_bytes(), |x| &**x);
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

    pub fn into_inner(self) -> Result<Vec<u8>, Cow<'static, str>> {
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
            capture.0 = Err(e);
            capture
        }
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
        std::mem::replace(unsafe { &mut *out.get() }, OutputSource(output))
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
        std::mem::replace(unsafe { &mut *r#in.get() }, InputSource(Box::new(input))).0
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
macro_rules! input {
    (    ) => { $crate::with_token_reader(|r| r.next_token()).expect("Ran out of input") };
    (line) => { $crate::with_token_reader(|r| r.next_line ()).expect("Ran out of input") };
    ($t:ty) => { parse!(input!(), $t) };
    ($($t:ty),+ $(,)?) => { ($(input!($t)),+,) };

    [r!($($t:tt)*); $n:expr; Iterator] => {
        (0..({$n} as usize)).map(|_| input!($($t)*))
    };
    [$t:ty; $n:expr; Iterator] => {
        input![r!($t); $n; Iterator]
    };

    [r!($($t:tt)*); $n:expr; Array; Map($map: expr)] => {{
        #[allow(unused_mut)]
        let mut map = ($map);
        ::std::array::from_fn::<_, {$n as usize}, _>(|_| map(input!($($t)*)))
    }};
    [r!($($t:tt)*); $n:expr; Array] => { input!(r!($($t)*); $n; Array; Map(|x| x)) };
    [$t:ty; $n:expr; Array]     => { input!(r!($t); $n; Array) };
    [r!($($t:tt)*); $n:literal] => { input![r!($($t)*); $n; Array] };
    [$t:ty; $n:literal] => { input![r!($t); $n] };

    [r!($($t:tt)*); $n:expr; $container: ident; Map($map: expr)] => {
        input![r!($($t)*); $n; Iterator]
            .map($map)
            .collect::<$container<_>>()
    };
    [$t:ty; $n:expr; $container: ident; Map($map: expr)] => {
        input!(r!($t); $n; $container; Map($map))
    };

    [r!($($t:tt)*); $n:expr; $container: ident] => { input!(r!($($t)*); $n; $container; Map(|x| x)) };
    [     $t:ty   ; $n:expr; $container: ident] => { input!(r!(  $t  ); $n; $container) };

    [r!($($t:tt)*); $n:expr] => {{ use ::std::boxed::Box; read![r!($($t)*); $n; Box] }};
    [     $t:ty   ; $n:expr] => { input![r!($t); $n; Vec] };
}

#[doc(hidden)]
pub fn __output<I: IntoIterator<Item = D>, D: Display>(iter: I) {
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

        if cfg!(debug_assertions) {
            let _ = out.flush();
        }
    })
}

#[doc(hidden)]
pub fn __flush() {
    OUTPUT_SOURCE.with(|out| {
        // we don't let borrows escape the current thread, not the func
        unsafe { &mut **out.get() }
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
}

#[macro_export]
macro_rules! min {($first: expr $(, $other: expr)+) => {($first)$(.min($other))+};}
#[macro_export]
macro_rules! max {($first: expr $(, $other: expr)+) => {($first)$(.max($other))+};}
