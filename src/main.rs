
#[cfg(test)]
pub(crate) mod test;

pub mod diagnostic;
pub mod arena;
pub mod parser;
pub mod basegen;
pub mod typegen;
pub mod eval;

use std::{env, time, fmt, fs, path, thread, sync::Arc};
use arch::{Intrinsic, Intel64};
use basegen::Program;
use diagnostic::Diagnostic;

fn main() {
    
    let opts = match cli::parse(env::args()) {
        Err(err) => {
            let diag = match err {
                cli::CliError::ExpectedPath            => Diagnostic::error("expected path"),
                cli::CliError::ExpectedMode            => Diagnostic::error("expected mode"),
                cli::CliError::InvalidMode(other)      => Diagnostic::error("invalid mode").note(&other),
                cli::CliError::ExpectedVerbosity       => Diagnostic::error("expected verbosity"),
                cli::CliError::InvalidVerbosity(other) => Diagnostic::error("invalid verbosity").note(&other),
                cli::CliError::InvalidFlag(other)      => Diagnostic::error("invalid flag").note(&other),
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

    let path = opts.input.clone();
    let opts_arc = Arc::new(opts);

    // asynchronously compile the input file
    let receiver = compile::<Intel64>(Arc::clone(&opts_arc), path);
    let program = receiver.recv().unwrap();

    if matches!(opts_arc.mode, cli::Mode::ShowIr) {
        Diagnostic::debug("showing intermediate representation").emit();
        debug_print_program(&program);
    }

    if matches!(opts_arc.mode, cli::Mode::Run) {

        match eval::eval(program) {
            Ok(()) => (),
            Err(err) => {
                eval::format_error(err).emit();
                return;
            }
        }

    }

}

fn compile<'s, I: Intrinsic + Send + 'static>(opts: Arc<cli::Opts>, path: path::PathBuf) -> crossbeam_channel::Receiver<Program<I>> { // todo: use &'s Opts

    let file_name = path.file_stem().expect("invalid input file path").to_string_lossy().to_string();

    if opts.debug() {
        Diagnostic::debug(format!("compiling module `{}`", file_name)).emit();
    }

    let (sender, receiver) = crossbeam_channel::bounded(1);

    thread::Builder::new().stack_size(128 * 1024).name(format!("worker for module `{}`", file_name)).spawn(move || {

        let data = match fs::read_to_string(&path) {
            Ok(val) => val,
            Err(err) => {
                Diagnostic::error("cannot read input file")
                    .note(err.to_string())
                    .emit();
                return
            }
        };

        let source = match parser::parse(&data) {
            Ok(val) => val,
            Err(err) => {
                parser::format_error(err)
                    .file(file_name)
                    .emit();
                return
            }
        };

        let mut deps = Vec::new();

        for module in source.uses.iter() {

            let module_path = path.join(source.arena.get(module.path));
            let receiver = compile::<I>(Arc::clone(&opts), module_path);
            deps.push(receiver);

        }

        let mut compiled = Vec::new();

        for dep in deps {
            let code = dep.recv().unwrap();
            compiled.push(code);
        }

        // we now need to genrate code for the source files

        let symbols = match basegen::basegen(source) { // todo: add debug timings for codegen
            Ok(val) => val,
            Err(err) => {
                if opts.debug() {
                    Diagnostic::debug("codegen failed").emit();
                }
                basegen::format_error(err)
                    .file(file_name)
                    .emit();
                return
            }
        };

        let program = match typegen::typecheck(symbols) {
            Ok(val) => val,
            Err(err) => {
                if opts.debug() {
                    Diagnostic::debug("typecheck failed").emit();
                }
                typegen::format_error(err).emit();
                return;
            }
        };

        sender.send(program).unwrap();

    }).unwrap();

    receiver

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

fn debug_print_program<I: fmt::Debug>(program: &Program<I>) {
    for (_name, fun) in program.funs.iter() {
        // eprintln!("fn {}:", program.arena.get(*name));
        for (idx, instruction) in fun.body.iter().enumerate() {
            eprintln!("{:3}: {:?}", idx, instruction);
        }
    }
}

mod cli {

    use std::path::PathBuf;

    pub(crate) const VERSION: &str = "rough 0.0.0";

    pub(crate) const HELP: &str = r#"
usage: rh <file> [options]

[options]:
    -h --help                       Show the help menu
    --version                       Show the version
    -i --input <path>               Input file/directoy path
    -o --output <path>              Output file/directory path
    -m --mode <mode>                Select the compiler mode
    -v --verbosity <verbosity>      Select the verbosity level

[mode]:
    build                           Build the program
    run (default)                   Interpret the program
    show-items                      Show the parsed item list
    show-ir                         Show the intermediate representation

[verbosity]:
    quiet                           Shows only the output from your program.
    info (default)                  Shows some basic infos.
    debug                           Also shows timings.

tldr:

run `main.rh` (interpreted)
$ rh main.rh

build `main.rh` (compiled)
$ rh main.rh -m build

set the verbosity to `debug`
$ rn main.rh -v debug
"#;

    pub(crate) fn parse(mut args: impl Iterator<Item = String>) -> Result<Opts, CliError> {

        let mut opts = Opts::default();

        // skip the first arg
        args.next();

        loop {
            match args.next().as_deref() {
                Some("-h" | "--help") => opts.help = Help::Info,
                Some("--version")     => opts.help = Help::Version,
                Some("-i" | "--input")  => opts.input = match args.next() {
                    Some(path) => PathBuf::from(path),
                    None => return Err(CliError::ExpectedPath)
                },
                Some("-o" | "--output") => opts.output = match args.next() {
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
        pub(crate) help: Help,
        pub(crate) input: PathBuf,
        pub(crate) output: PathBuf,
        pub(crate) mode: Mode,
        pub(crate) verbosity: Verbosity,
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

