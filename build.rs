
fn main() {
    // use std::fs::File;
    // use std::io::Write;
    // use proc_macro2::TokenStream;
    //
    // let src = include_str!("./minified/minified.rs")
    //     .parse::<TokenStream>().unwrap();
    //
    // let mini = format!("#[cfg(not(feature = \"local-build\"))] #[macro_use] pub mod online_judge_utils {{ {src} }}")
    //     .replace("$ crate", "online_judge_utils")
    //     .replace("online_judge_utils :: read", "read")
    //     .replace("online_judge_utils :: parse", "parse");
    // File::create("./minified/__minified.rs").unwrap().write_all(mini.as_bytes()).unwrap()
}