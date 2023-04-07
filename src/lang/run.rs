//! A collection of structs and functions used to run betty REPL or a betty file

use std::collections::VecDeque;
use std::fmt;
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;
use std::thread;
use std::time;

use clap::Parser as ClapParser;
use device_query::DeviceQuery;
use device_query::{DeviceState, Keycode};

use super::error::{Ctx, Error, ErrorKind};
use super::interpreter::Interpreter;
use super::lexer::Lexer;
use super::namespace::Namespace;
use super::object::Object;
use super::parser::Parser;
use super::type_alias::ParserResults;

/// If Ctrl+Z have been pressed, quit the repl
const QUIT_KEYS: [Keycode; 2] = [Keycode::Z, Keycode::LControl];

/// Spawn the main thread with the desired stack size
macro_rules! main_thread {
    ($stack_size:expr, $block:block) => {
        thread::Builder::new()
            .name("<betty main>".into())
            .stack_size($stack_size)
            .spawn(move || $block)
            .unwrap_or_else(|err| panic!("OS failed to create a thread to run betty: {}", err))
            .join()
            .unwrap();
    };
}

/// This struct represents the version of betty
struct BettyVersion;

impl BettyVersion {
    const MAJOR: u16 = 0;
    const MINOR: u16 = 1;
    const MICRO: u16 = 0;
}

/// Nicely print the betty version in the repl
impl fmt::Display for BettyVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}.{}.{}",
            BettyVersion::MAJOR,
            BettyVersion::MINOR,
            BettyVersion::MICRO,
        )
    }
}

/// Cli application handler
#[derive(ClapParser)]
#[command(author, version, about)]
pub struct CliParser {
    /// Path of the betty file. If not provided, run repl console
    pub path: Option<String>,

    /// Time
    #[arg(short, long, help = "Time the program execution")]
    pub time: bool,

    /// Additional args provided to the program
    pub args: Option<Vec<String>>,

    /// Stack size
    #[arg(long, default_value_t = 2 * 1024 * 1024, help = "Allocate memory for the stack")]
    pub stack_size: usize,
}

pub struct BettyFile {
    filename: String, // This cannot be &str otherwise a weird error shows up in the macro...
    args: Option<Vec<String>>,
    stack_size: usize,
    time: bool,
}

impl BettyFile {
    pub fn new(settings: CliParser) -> Self {
        Self {
            filename: settings.path.unwrap(), // Guarded by main function
            args: settings.args,
            stack_size: settings.stack_size,
            time: settings.time,
        }
    }

    pub fn run(self) {
        let timer = if self.time {
            Some(time::Instant::now())
        } else {
            None
        };

        main_thread!(self.stack_size, {
            // Get source
            let source = match self.get_source() {
                Ok(source) => source,
                Err(err) => return eprintln!("{}", err),
            };
            let filename = PathBuf::from(self.filename);

            // Lexer
            let tokens =
                match Lexer::new(Some(filename.clone()), source.chars().collect()).make_tokens() {
                    Ok(Some(tokens)) => tokens,
                    Ok(None) => return, // No tokens emitted!
                    Err(err) => return eprintln!("{}", err),
                };

            // Parser
            let nodes = match Parser::new(VecDeque::from(tokens))
                .filename(filename.clone())
                .parse()
            {
                Ok(nodes) => nodes,
                Err(err) => return eprintln!("{}", err),
            };

            // Interpreter
            if let Err(err) = Interpreter::new(Namespace::new())
                .filename(filename)
                .insert_args(self.args)
                .visit_multiple(nodes)
            {
                eprintln!("{}", err);
            }
        });

        if let Some(timer) = timer {
            println!(
                "\nElapsed time: {:.3} seconds",
                timer.elapsed().as_secs_f64()
            );
        }
    }

    fn get_source(&self) -> Result<String, String> {
        fs::read_to_string(&self.filename).map_err(|err| {
            format!(
                "Cannot read betty source code from '{}' because of the following error: {}",
                self.filename, err
            )
        })
    }

    /// Import a betty module into the program, return the nodes
    pub fn import_module(path: PathBuf, ctx: Ctx) -> ParserResults {
        let source = fs::read_to_string(&path).map_err(|err| {
            Error::new(
                ErrorKind::ModuleImport,
                Some(format!(
                    "An error occurred when importing {:?}: {}",
                    path, err
                )),
                Some(ctx),
            )
        })?;
        let Some(tokens) = Lexer::new(Some(path.clone()), source.chars().collect()).make_tokens()? else {
            return Ok(Vec::new());  // No nodes because of no tokens!
        };

        Parser::new(VecDeque::from(tokens)).filename(path).parse()
    }
}

pub struct BettyRepl {
    args: Option<Vec<String>>,
    stack_size: usize,
}

impl BettyRepl {
    pub fn new(settings: CliParser) -> Self {
        Self {
            args: settings.args,
            stack_size: settings.stack_size,
        }
    }

    pub fn run(self) {
        main_thread!(self.stack_size, {
            let mut interpreter = Interpreter::repl().insert_args(self.args);
            Self::print_version();
            loop {
                let Some(source) = Self::get_source() else {
                    return;  // quit keys have been pressed
                };

                // Lexer
                let tokens = match Lexer::new(None, source.chars().collect()).make_tokens() {
                    Ok(Some(tokens)) => tokens,
                    Ok(None) => continue,
                    Err(err) => {
                        eprintln!("{}", err);
                        continue;
                    }
                };

                // Parser
                let node = match Parser::new(VecDeque::from(tokens)).parse() {
                    // There is only one node anyway
                    Ok(mut nodes) => nodes.remove(0),
                    Err(err) => {
                        eprintln!("{}", err);
                        continue;
                    }
                };

                // Interpreter
                match interpreter.visit(node) {
                    Ok(result) => {
                        if result != Object::Nothing {
                            println!("{}", result);
                        }
                    }
                    Err(err) => eprintln!("{}", err),
                }
            }
        });
    }

    fn print_version() {
        let descr = format!("betty {}", BettyVersion);
        let sep = "-".repeat(descr.len());
        println!("{}\n{}", descr, sep);
    }

    fn get_source() -> Option<String> {
        let mut source = String::new();
        let keyboard = DeviceState::new();
        if keyboard.get_keys() == QUIT_KEYS {
            return None;
        }

        print!(">>> ");
        io::stdout()
            .flush()
            .expect("Fatal internal error in flushing stdout while getting repl source");
        io::stdin()
            .read_line(&mut source)
            .expect("Fatal internal error in reading stdin while getting repl source");

        if source.ends_with('\n') {
            source.pop();
            if source.ends_with('\r') {
                source.pop();
            }
        }
        Some(source)
    }
}
