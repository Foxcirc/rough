
#[cfg(test)]
pub(crate) mod test;

pub mod diagnostic;
pub mod parser;
pub mod compiler;

use std::{env, fs, collections::HashSet};
use crate::diagnostic::Diag;

fn main() {
    
    let opts = match cli::parse(env::args()) {
        Err(err) => {
            let diag = match err {
                cli::CliError::ExpectedPath            => Diag::error("Expected Path"),
                cli::CliError::ExpectedMode            => Diag::error("Expected Mode"),
                cli::CliError::InvalidMode(other)      => Diag::error("Invalid Mode").note(&other),
                cli::CliError::ExpectedVerbosity       => Diag::error("Expected Verbosity"),
                cli::CliError::InvalidVerbosity(other) => Diag::error("Invalid Verbosity").note(&other),
                cli::CliError::InvalidFlag(other)      => Diag::error("Invalid Flag").note(&other),
            };
            eprintln!("{}", diag.format());
            return
        },
        Ok(cli::Opts { help: cli::Help::Version, .. }) => {
            eprintln!("{}", cli::VERSION);
            return
        },
        Ok(cli::Opts { help: cli::Help::Info, .. }) => {
            eprintln!("{}", cli::HELP);
            return
        },
        Ok(opts) => opts,
    };

    // start parsing and discovering the project's dependencies
    // (non recursive version)
    
    let mut visited = HashSet::new();
    let mut result  = Vec::new();

    let mut files_to_process = Vec::new();
    files_to_process.push(opts.input);

    while let Some(file) = files_to_process.pop() {

        if !visited.insert(Clone::clone(&file)) { continue }; // todo: remove this clone

        let content_raw = fs::read(file).expect("todo: (io-error) cannot open file");
        let content = String::from_utf8(content_raw).expect("todo: (utf8-error) input file is not valid utf8");

        result.push(SourceFile { content, items: parser::ItemList::default() });
        let source_file = result.last_mut().expect("No last element");

        let items = parser::parse(&source_file.content).expect("todo: (parse-error) invalid source file");

        let uses: Vec<String> = items.uses.iter().map(|item| item.path.to_string()).collect(); // todo: also borrow 'a ??
        files_to_process.extend(uses);

        source_file.items = items; // todo: fuck, this is a self-referencial struct no?

    }

    println!("Files visited: {:?}", visited);

}

struct SourceFile<'a> {
    pub(crate) content: String,
    pub(crate) items: parser::ItemList<'a>
}

mod cli {

    pub(crate) const VERSION: &str = "rough 0.0.1 (experimental build)";

    pub(crate) const HELP: &str = r#"
Usage: rh <file> [options]

[options]:
    -h --help                       Show the help menu
    --version                       Show the version
    -i --input <path>               Input file/directoy path
    -o --output <path>              Output file/directory path
    -m --mode <mode>                Select the compiler mode
    -v --verbosity <verbosity>      Select the verbosity level

[mode]:
    build                           Build the program
    run (default)                   Build the program, then run it
    show-items                      Show the parsed item list
    show-ir                         Show the intermediate representation

[verbosity]:
    quiet
    info (default)
    all

Example: rh main.rh --mode build --verbosity debug
Written by Foxcirc.
"#;

    pub(crate) fn parse(mut args: impl Iterator<Item = String>) -> Result<Opts, CliError> {

        let mut opts = Opts::default();

        loop {
            match args.next().as_deref() {
                Some("-h" | "--help") => opts.help = Help::Info,
                Some("--version")     => opts.help = Help::Version,
                Some("-i" | "--input")  => opts.input = match args.next().as_deref() {
                    Some(path) => path.to_string(),
                    None => return Err(CliError::ExpectedPath)
                },
                Some("-o" | "--output") => opts.output = match args.next().as_deref() {
                    Some(path) => path.to_string(),
                    None => return Err(CliError::ExpectedPath)
                },
                Some("-m" | "--mode") => opts.mode = match args.next().as_deref() {
                    Some("build")      => Mode::Build,
                    Some("run")        => Mode::Run,
                    Some("show-items") => Mode::ShowItems,
                    Some("show-ir")    => Mode::ShowIr,
                    Some(other)        => return Err(CliError::InvalidMode(other.to_string())),
                    None               => return Err(CliError::ExpectedMode)
                },
                Some("-v" | "--verbosity") => opts.verbosity = match args.next().as_deref() {
                    Some("quiet") => Verbosity::Quiet,
                    Some("info")  => Verbosity::Info,
                    Some("debug") => Verbosity::Debug,
                    Some(other)   => return Err(CliError::InvalidVerbosity(other.to_string())),
                    None          => return Err(CliError::ExpectedVerbosity)
                },
                Some(other) if other.starts_with('-') => return Err(CliError::InvalidFlag(other.to_string())),
                Some(other)                           => opts.input = other.to_string(),
                None => break,
            }
        }

        Ok(opts)

    }

    pub(crate) enum CliError {
        ExpectedPath,
        ExpectedMode,
        InvalidMode(String),
        ExpectedVerbosity,
        InvalidVerbosity(String),
        InvalidFlag(String),
    }

    #[derive(Default)]
    pub(crate) struct Opts {
        pub(crate) help: Help, // show help menu: -h, --help
        pub(crate) input: String, // the input file to compile: [file], -i, --i [file]
        pub(crate) output: String, // the output path: -o, --output [path]
        pub(crate) mode: Mode, // what to do: -m, --mode [mode]
        pub(crate) verbosity: Verbosity, // how verbose to be: -v , --verbose [verbosity]
    }

    #[derive(Default)]
    pub(crate) enum Help {
        #[default]
        None,
        Info,
        Version,
    }

    #[derive(Default)]
    pub(crate) enum Verbosity {
        Quiet,
        #[default]
        Info,
        Debug,
    }

    #[derive(Default)]
    pub(crate) enum Mode {
        Build,
        #[default]
        Run,
        ShowItems,
        ShowIr,
    }

}



