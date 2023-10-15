
#[cfg(test)]
pub(crate) mod test;

pub mod parser;
pub mod codegen;
pub mod typecheck;
pub mod diagnostic;

use std::{env, time};
use diagnostic::Diagnostic;

fn main() {
    
    let opts = match cli::parse(env::args()) {
        Err(err) => {
            let diag = match err {
                cli::CliError::ExpectedPath            => Diagnostic::error("Expected Path"),
                cli::CliError::ExpectedMode            => Diagnostic::error("Expected Mode"),
                cli::CliError::InvalidMode(other)      => Diagnostic::error("Invalid Mode").note(&other),
                cli::CliError::ExpectedVerbosity       => Diagnostic::error("Expected Verbosity"),
                cli::CliError::InvalidVerbosity(other) => Diagnostic::error("Invalid Verbosity").note(&other),
                cli::CliError::InvalidFlag(other)      => Diagnostic::error("Invalid Flag").note(&other),
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

    // start discovering and parsing the project's dependencies

    let parsing_start_time = time::Instant::now();

    let mut state = parse_modules::State::default();

    match parse_modules::parse_modules(&mut state, opts.input.clone()) {
        Ok(()) => (),
        Err(diag) => {
            diag.emit();
            return
        },
    };

    let source_files = parse_modules::source_files(state);

    if opts.debug() {
        let files: Vec<_> = source_files.iter().map(|item| item.name()).collect();
        let how_many = source_files.len();
        let what = if source_files.len() == 1 { "file" } else { "files" };
        Diagnostic::debug("parsing done")
            .note(format!("took {:?}", time::Instant::now() - parsing_start_time))
            .note(format!("{} {}: {}", how_many, what, files.join(", ")))
            .emit();
    }

    // we now need to genrate code for the source files

    let mut state = codegen::State::<arch::Intel64>::default();

    for source_file in source_files.into_iter() {

        match codegen::codegen(&mut state, source_file) {
            Ok(()) => (),
            Err(err) => {
                if opts.debug() {
                    Diagnostic::debug("codegen failed").emit();
                }
                codegen::format_error(err).emit();
                return
            }
        };

    }

    let bytecode = state.bytecode;

    if opts.mode == cli::Mode::ShowIr {
        Diagnostic::debug("showing intermediate representation").emit();
        for (idx, instruction) in bytecode.iter().enumerate() {
            eprintln!("{:3}: {:?}", idx, instruction);
        }
    }

    match typecheck::typecheck(bytecode) {
        Ok(()) => (),
        Err(err) => {
            if opts.debug() {
                Diagnostic::debug("typecheck failed").emit();
            }
            typecheck::format_error(err).emit();
            return;
        }
    };

}

pub(crate) mod arch {

    use crate::parser::Type;

    pub(crate) trait Intrinsic: PartialEq + Eq {
        fn generate(name: &str) -> Option<Self> where Self: Sized;
        fn signature(&self) -> &[&[Type]; 2];
    }

    #[derive(Debug, PartialEq, Eq)]
    pub(crate) enum Intel64 {
        Syscall1,
        Syscall2,
        Syscall3,
        Syscall4,
        Syscall5,
    }

    impl Intrinsic for Intel64 {
        fn generate(name: &str) -> Option<Self> {
            match name {
                "syscall-1" => Some(Self::Syscall1),
                "syscall-2" => Some(Self::Syscall2),
                "syscall-3" => Some(Self::Syscall3),
                "syscall-4" => Some(Self::Syscall4),
                "syscall-5" => Some(Self::Syscall5),
                _other => None,
            }
        }
        fn signature(&self) -> &[&[Type]; 2] {
            const INT: Type = Type::Int;
            match self {
                Self::Syscall1 => &[&[INT; 1], &[INT]],
                Self::Syscall2 => &[&[INT; 2], &[INT]],
                Self::Syscall3 => &[&[INT; 3], &[INT]],
                Self::Syscall4 => &[&[INT; 4], &[INT]],
                Self::Syscall5 => &[&[INT; 5], &[INT]],
            }
        }
    }

}

pub(crate) mod parse_modules {

    use std::{path::{PathBuf, Path}, ffi::OsStr, fs, collections::{HashSet, HashMap, VecDeque}};
    use crate::{parser::{self, TranslationUnit}, diagnostic::Diagnostic};

    pub(crate) fn parse_modules(state: &mut State, path: PathBuf) -> Result<(), Diagnostic> {

        if !state.visited.insert(path.clone()) { // todo: clone?

            let idx = state.source_files.iter()
                .position(|item: &SourceFile| &item.path == &path)
                .expect("visited path not found");

            // move this entry to the last position
            let item = state.source_files.remove(idx).expect("invalid index");
            state.source_files.push_front(item);

            return Ok(())

        };

        let (base, file_name) = split_file_name(&path)?;

        let content = match fs::read_to_string(&path) {
            Ok(val) => val,
            Err(err) => return Err(Diagnostic::error("cannot read file").note(err.to_string()).file(file_name))
        };

        let items = match parser::parse(&content, state.visited.len()) { // todo: is span_id needed?
            Ok(val) => val,
            Err(err) => return Err(parser::format_error(err).file(file_name)),
        };

        let mut module_map = HashMap::new();

        for item in items.uses {

            let module_name = &item.path;
            let module_path = base.join(module_name);
            module_map.insert(item, module_path.clone());

            parse_modules(state, module_path)?;

        }

        state.source_files.push_back(SourceFile {
            path,
            content,
            items: ModuleTranslationUnit {
                uses: module_map,
                funs: items.funs,
                types: items.types,
            }
        });

        Ok(())

    }

    fn split_file_name(path: &Path) -> Result<(&Path, &str), Diagnostic> {
        Ok((
            path.parent().ok_or(Diagnostic::error("invalid file"))?,
            path.file_name().ok_or(Diagnostic::error("invalid file"))
                .and_then(|name| name.to_str().ok_or(Diagnostic::error("non-utf8 file name")))?
        ))
    }

    pub(crate) fn source_files(state: State) -> Vec<SourceFile> {
        state.source_files.into()
    }

    #[derive(Default)]
    pub(crate) struct State {
        pub(crate) visited: HashSet<PathBuf>,
        pub(crate) source_files: VecDeque<SourceFile>,
    }

    #[derive(Debug)]
    pub(crate) struct SourceFile {
        pub(crate) path: PathBuf,
        pub(crate) content: String,
        pub(crate) items: ModuleTranslationUnit
    }

    pub(crate) type ModuleTranslationUnit = TranslationUnit<HashMap<parser::Use, PathBuf>>;

    impl SourceFile {
        pub(crate) fn name(&self) -> &str {
            self.path.file_name()
                .unwrap_or(&OsStr::new("unknown"))
                .to_str()
                .expect("file name not valid utf8")
        }
    }

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

    #[derive(Default, PartialEq, Eq)]
    pub(crate) enum Mode {
        Build,
        #[default]
        Run,
        ShowItems,
        ShowIr,
    }

}



