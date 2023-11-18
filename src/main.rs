
#[cfg(test)]
pub(crate) mod test;

pub mod diagnostic;
pub mod arena;
pub mod util;
pub mod parser;
pub mod basegen;
pub mod typegen;
pub mod eval;

use std::{env, fmt, fs, path, sync::Arc, thread, time, collections::HashMap, pin::Pin, mem, task::Poll, future::Future, iter};
use arch::{Intrinsic, Intel64};
use basegen::Program;
use diagnostic::Diagnostic;
use futures_lite::{future::{self, poll_fn}, FutureExt};

fn main() {
    
    let opts = match cli::parse(env::args()) {
        Err(err) => {
            let diag = match err {
                cli::CliError::Expected(what) => Diagnostic::error(format!("expected {}", what)),
                cli::CliError::Invalid(what, got) => Diagnostic::error(format!("expected {} but got {}", what, got)),
                cli::CliError::UnknownFlag(other) => Diagnostic::error("invalid flag").note(&other),
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

    if opts.input.file_name().is_none() {
        Diagnostic::error("no input file provided").emit();
        return
    }

    let input_path = opts.input.clone();
    let unique = Arc::new(SharedState {
        opts,
        executor: async_executor::Executor::new(),
        cache: async_lock::RwLock::new(HashMap::new()),
    });

    // asynchronously compile the program

    let max_threads = num_cpus::get();
    let num_threads = unique.opts.threads.as_ref().copied().unwrap_or(max_threads).clamp(1, max_threads);

    // used to determine if the compilation of the main module is done
    let barrier = Arc::new(async_lock::Barrier::new(num_threads + 1));

    // spawn the task that compiles the main program
    let shared_clone = Arc::clone(&unique);
    let barrier_clone = Arc::clone(&barrier);
    let task = unique.executor.spawn(async move {
        let result = compile::<Intel64>(shared_clone, input_path).await;
        barrier_clone.wait().await; // signal to the workers that they can exit now
        result
    });

    let start_time = time::Instant::now();
    Diagnostic::info("starting compilation")
        .note(format!("worker threads: {}", num_threads))
        .emit();

    // spawn the worker threads
    let mut handles = Vec::new();
    for _ in 0..num_threads { // spawn num_threads threads
        let shared_clone = Arc::clone(&unique);
        let barrier_clone = Arc::clone(&barrier);
        let handle = thread::spawn(move || {
            future::block_on(shared_clone.executor.run(barrier_clone.wait()));
        });
        handles.push(handle);
    }

    // wait for all workers to finish
    for handle in handles {
        match handle.join() {
            Ok(_) => (),
            Err(_) => {
                Diagnostic::error("worker thread panicked").emit();
                return
            }
        };
    }

    let took_time = time::Instant::now() - start_time;
    Diagnostic::info("all workers finished")
        .note(format!("took {:?}", took_time))
        .emit();

    // get the result of the task which compiled the main module
    // note: this will potentially still need to run the `barrier.wait().await` inside the main task
    // if all worker threads exited as soon as the barrier became free
    let result = match future::block_on(unique.executor.run(task)) {
        Ok(val) => val,
        Err(()) => return,
    };

    // all tasks must have completed by now
    assert!(unique.executor.is_empty());

    // destroy the shared state
    let mut unique = Arc::into_inner(unique).expect("state must be unique by now");
    unique.cache.get_mut().clear(); // clear the cache so we can take the program out of the arc

    let program = Arc::into_inner(result).expect("result must be unique by now");

    // now we can either evaluate the program or generate something else

    match unique.opts.mode {
        cli::Mode::ShowIr => {
            Diagnostic::info("showing intermediate representation").emit();
            debug_print_program(&program);
        },
        cli::Mode::Build => {
            Diagnostic::info("starting build").emit();
            todo!("codegen");
        },
        cli::Mode::Run => {
            Diagnostic::info("starting evaluation").emit();
            match eval::eval(program) {
                Ok(()) => (),
                Err(err) => {
                    eval::format_error(err).emit();
                    return;
                }
            }
        }
    }

}

fn compile<I: Intrinsic + Send + Sync + 'static>(shared: Arc<SharedState<'static, I>>, path: path::PathBuf) -> future::Boxed<Result<Arc<Program<I>>, ()>> {

    async move {

        let module_name = path.to_string_lossy().to_string();

        let guard = shared.cache.read().await;
        if let Some(val) = guard.get(&path) {
            if shared.opts.debug() {
                Diagnostic::debug(format!("cached module `{}`", module_name)).emit();
            }
            return Ok(Arc::clone(&val))
        }
        drop(guard);

        if shared.opts.debug() {
            Diagnostic::debug(format!("compiling module `{}`", module_name)).emit();
        }

        let data = match fs::read_to_string(&path) {
            Ok(val) => val,
            Err(err) => {
                Diagnostic::error("cannot read input file")
                    .note(err.to_string().to_lowercase())
                    .emit();
                return Err(())
            }
        };

        let source = match parser::parse(&data) {
            Ok(val) => val,
            Err(err) => {
                parser::format_error(err)
                    .file(module_name)
                    .emit();
                return Err(())
            }
        };

        let mut futs = Vec::new();

        for module in source.uses.iter() {

            let module_path = path.with_file_name("").join(source.arena.get(module.path));

            let val = shared.executor.spawn(compile::<I>(Arc::clone(&shared), module_path.clone()));
            futs.push(val);

        }

        // todo: keep track of building modules and wait for the module to be build if it's already building inside another worker

        // wait for all the dependencies to be built
        let _deps = util::join_all(futs).await;

        // we now can genrate code for this translation unit

        println!("sleep");
        thread::sleep(time::Duration::from_millis(100));

        let symbols = match basegen::basegen(source) { // todo: add debug timings for codegen
            Ok(val) => val,
            Err(err) => {
                if shared.opts.debug() {
                    Diagnostic::debug("codegen failed").emit();
                }
                basegen::format_error(err)
                    .file(module_name)
                    .emit();
                return Err(())
            }
        };

        let program = match typegen::typecheck(symbols) {
            Ok(val) => val,
            Err(err) => {
                if shared.opts.debug() {
                    Diagnostic::debug("typecheck failed").emit();
                }
                typegen::format_error(err).emit();
                return Err(())
            }
        };

        let arc = Arc::new(program);

        // write the built module into the cache
        let mut guard = shared.cache.write().await;
        guard.insert(path, Arc::clone(&arc));
        drop(guard);

        Ok(arc)

    }.boxed()

}

struct SharedState<'a, I: Send + Sync> {
    pub opts: cli::Opts,
    pub executor: async_executor::Executor<'a>,
    pub cache: async_lock::RwLock<HashMap<path::PathBuf, Arc<Program<I>>>>,
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
    --threads <number>              Set the number of worker threads

[mode]:
    build                           Build the program
    run (default)                   Interpret the program
    show                            Show the intermediate representation

[verbosity]:
    quiet                           Shows only the output from your program.
    info (default)                  Shows some basic information.
    debug                           Shows more verbose debug information.

examples:

$ rh main.rh                        Run main.rh (interpreted)
$ rh main.rh -m build               Build main.rh (compiled, produces binary)
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
                    None => return Err(CliError::Expected("path"))
                },
                Some("-o" | "--output") => opts.output = match args.next() {
                    Some(path) => PathBuf::from(path),
                    None => return Err(CliError::Expected("path"))
                },
                Some("-m" | "--mode") => opts.mode = match args.next().as_deref() {
                    Some("build")      => Mode::Build,
                    Some("run")        => Mode::Run,
                    Some("show")       => Mode::ShowIr,
                    Some(other)        => return Err(CliError::Invalid("mode", other.to_string())),
                    None               => return Err(CliError::Expected("mode"))
                },
                Some("-v" | "--verbosity") => opts.verbosity = match args.next().as_deref() {
                    Some("quiet") => Verbosity::Quiet,
                    Some("info")  => Verbosity::Info,
                    Some("debug") => Verbosity::Debug,
                    Some(other)   => return Err(CliError::Invalid("verbosity", other.to_string())),
                    None          => return Err(CliError::Expected("verbosity"))
                },
                Some("--threads") => opts.threads = match args.next() {
                    Some(num) => match usize::from_str_radix(&num, 10) {
                        Ok(val) => Some(val),
                        Err(_) => return Err(CliError::Invalid("number", num))
                    },
                    None => return Err(CliError::Expected("number"))
                },
                Some(other) if other.starts_with('-') => return Err(CliError::UnknownFlag(other.to_string())),
                Some(other)                           => opts.input = PathBuf::from(other),
                None => break,
            }
        }

        Ok(opts)

    }

    pub(crate) enum CliError {
        Expected(&'static str),
        Invalid(&'static str, String),
        UnknownFlag(String),
    }

    #[derive(Default)]
    pub(crate) struct Opts {
        pub(crate) help: Help,
        pub(crate) input: PathBuf,
        pub(crate) output: PathBuf,
        pub(crate) mode: Mode,
        pub(crate) verbosity: Verbosity,
        pub(crate) threads: Option<usize>,
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
        ShowIr,
    }

}

