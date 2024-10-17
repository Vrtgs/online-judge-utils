use proc_macro2::TokenStream;
use std::fs::File;
use std::io::Write;


fn main() {
    if std::fs::exists("./minified").unwrap() {
        let src = include_str!("./src/lib.rs")
            .replace(
                "pub mod rng;",
                concat!("pub mod rng { ", include_str!("./src/rng.rs"), " }"),
            )
            .replace(
                "pub mod modding_num;",
                concat!(
                "pub mod modding_num { ",
                include_str!("./src/modding_num.rs"),
                " }"
                ),
            )
            .parse::<TokenStream>()
            .unwrap();

        let mini = format!("#[rustfmt::skip] #[allow(unexpected_cfgs)] #[cfg(not(feature = \"local-build\"))] #[macro_use] pub mod online_judge_utils {{ {src} }}")
            .replace("$ crate", "online_judge_utils")
            .replace("online_judge_utils :: read", "read")
            .replace("online_judge_utils :: file_io", "file_io")
            .replace("online_judge_utils :: output", "output")
            .replace("online_judge_utils :: parse", "parse");

        File::create("./minified/__minified.rs")
            .unwrap()
            .write_all((mini + "\n").as_bytes())
            .unwrap()
    }
}
