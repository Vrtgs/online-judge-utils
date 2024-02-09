fn main() {
//     use std::fs::File;
//     use std::io::Write;
//     use proc_macro2::TokenStream;
//
//     let src = include_str!("./src/minified.rs").parse::<TokenStream>()
//         .unwrap();
//     write!(File::create("./src/__minified.rs").unwrap(),
//     "#[cfg(not(feature = \"local-build\"))] #[macro_use] pub mod online_judge_utils {{ {src} }}"
//     ).unwrap()
}