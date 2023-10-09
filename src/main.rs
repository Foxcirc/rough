
#[cfg(test)]
pub(crate) mod test;

pub mod diagnostic;
pub mod parser;
pub mod compiler;

use std::{env, fs, collections::{HashSet, self, VecDeque}, hash::{self, Hasher}, path::PathBuf, time};
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
            diag.emit();
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
    
    let parsing_start_time = time::Instant::now();
    
    let mut results  = Vec::new();
    let mut visited = HashSet::new();
    let mut paths   = VecDeque::new();

    let (base, input) = match (opts.input.parent(), opts.input.file_name()) {
        (Some(base), Some(input)) => (base, PathBuf::from(input)),
        (..) => {
            Diag::error("Invalid input path")
                .code(&opts.input.to_string_lossy())
                .emit();
            return
        },
    };

    paths.push_back(input);

    while let Some(path) = paths.pop_front() {

        if !visited.insert(path.clone()) { continue };

        let human_name = match path.file_name() {
            Some(val) => val.to_string_lossy().to_string(),
            None => {
                Diag::error("Invalid source-file path").emit();
                return
            }
        };

        let content_raw = match fs::read(base.join(&path)) {
            Ok(val) => val,
            Err(err) => {
                Diag::error("Cannot read source-file")
                    .code(human_name)
                    .note(err)
                    .emit();
                return
            }
        };

        let content = match String::from_utf8(content_raw) {
            Ok(val) => val,
            Err(err) => {
                Diag::error("Source file contains invalid Utf-8")
                    .code(human_name)
                    .note(err)
                    .emit();
                return
            }
        };

        let items = match parser::parse(&content) {
            Ok(val) => val,
            Err(err) => {
                parser::format_parse_error(err).emit();
                return
            }
        };

        for item in items.uses.iter() {
            paths.push_back(PathBuf::from(item.path.inside(&content)));
        }

        results.push(SourceFile { name: human_name, content, items });

    }

    if opts.debug() {
        let files: Vec<_> = results.iter().map(|item| &item.name[..]).collect();
        Diag::debug("Parsing done")
            .note(format!("Took: {:?}", time::Instant::now() - parsing_start_time))
            .note(format!("In total {} files visited: {}", results.len(), files.join(", ")))
            .emit();
    }

    // we now need to parse the files in reverse order
    results.reverse();



}

#[derive(Debug)]
struct SourceFile {
    pub(crate) name: String,
    pub(crate) content: String,
    pub(crate) items: parser::ItemList
}

mod cli {

    use std::path::PathBuf;

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
                    Some(path) => PathBuf::from(path),
                    None => return Err(CliError::ExpectedPath)
                },
                Some("-o" | "--output") => opts.output = match args.next().as_deref() {
                    Some(path) => PathBuf::from(path),
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
                Some(other)                           => opts.input = PathBuf::from(other),
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
        pub(crate) input: PathBuf, // the input file to compile: [file], -i, --i [file]
        pub(crate) output: PathBuf, // the output path: -o, --output [path]
        pub(crate) mode: Mode, // what to do: -m, --mode [mode]
        pub(crate) verbosity: Verbosity, // how verbose to be: -v , --verbose [verbosity]
    }

    impl Opts {
        pub(crate) fn debug(&self) -> bool {
            matches!(self.verbosity, Verbosity::Debug)
        }
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



