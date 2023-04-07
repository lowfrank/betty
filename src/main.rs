// As of now I want to support this only
#![cfg(all(target_arch = "x86_64", target_os = "windows"))]
#![allow(clippy::type_complexity)]

use clap::Parser;

mod lang;

use lang::run::{BettyFile, BettyRepl, CliParser};

fn main() {
    let settings = CliParser::parse();

    // If a path is provided, run the file, otherwise open the repl
    if settings.path.is_some() {
        BettyFile::new(settings).run()
    } else {
        BettyRepl::new(settings).run()
    }
}
