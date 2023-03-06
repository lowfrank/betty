#![cfg(all(target_arch = "x86_64", target_os = "windows"))]
#![allow(clippy::type_complexity)]

use clap::Parser;

mod lang;

use lang::run::{BettyFile, BettyRepl, CliParser};

fn main() {
    let settings = CliParser::parse();
    if settings.path.is_some() {
        BettyFile::new(settings).run()
    } else {
        BettyRepl::new(settings).run()
    }
}
