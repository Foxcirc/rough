
#[cfg(test)]
pub(crate) mod test;

pub mod diagnostic;
pub mod arena;
pub mod util;
pub mod parser;
pub mod basegen;
pub mod typegen;
pub mod eval;

use std::{env, fmt, fs, path, sync::{Arc, atomic::{AtomicBool, Ordering}}, thread, time, collections::HashMap, ffi::OsString};
use arch::{Intrinsic, Intel64};
use basegen::Program;
use diagnostic::Diagnostic;
use futures_lite::{future, FutureExt};
use parser::TranslationUnit;

fn main() {
    
    let opts = match cli::parse(env::args()) {
        Err(err) => {
            let diag = match err {
                cli::CliError::Expected(what) => Diagnostic::error(format!("expected {}", what)),
                cli::CliError::Invalid(what, got) => Diagnostic::error(format!("invalid {}", what)).code(got),
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

    let (working_dir, input_file) = match split_path_unwrap(opts.input.clone()) {
        Some(val) => val,
        None => {
            Diagnostic::error("invalid (or no) input file provided").emit();
            return
        }
    };

    env::set_current_dir(working_dir).expect("cannot set working directory");

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
        let result = compile::<Intel64>(shared_clone, path::PathBuf::from(input_file)).await;
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
            debug_print_program(&program.inner);
        },
        cli::Mode::Build => {
            Diagnostic::info("starting build").emit();
            todo!("codegen");
        },
        cli::Mode::Run => {
            Diagnostic::info("starting evaluation").emit();
            match eval::eval(program.inner) {
                Ok(()) => (),
                Err(err) => {
                    eval::format_error(err).emit();
                    return;
                }
            }
        }
    }

}

fn compile<I: Intrinsic + Send + Sync + 'static>(shared: Arc<SharedState<'static, I>>, path: path::PathBuf) -> future::Boxed<Result<Arc<TranslationUnit<Program<I>>>, ()>> {

    async move {

        let module_name = path.to_string_lossy().to_string();

        // check if we already compiled this module or are compiling it right now
        let guard = shared.cache.read().await;
        if let Some(val) = guard.get(&path) {

            // possibly already compiling in another task
            if let CompilationState::InProgress(waiter) = val {

                if shared.opts.debug() {
                    Diagnostic::debug(format!("waiting for module `{}`", module_name)).emit();
                }

                let cloned = Arc::clone(&waiter);

                drop(guard);

                // wait for the compilation to finish
                cloned.wait().await;

            } else {

                drop(guard);

                if shared.opts.debug() {
                    Diagnostic::debug(format!("already compiled module `{}`", module_name)).emit();
                }

            }

            // compilation of the module must be done now
            let guard = shared.cache.read().await;
            if let Some(val) = guard.get(&path) {
                if let CompilationState::Done(program) = val {
                    return Ok(Arc::clone(program))
                } else {
                    unreachable!()
                }
            } else {
                unreachable!()
            }

        } else {
            drop(guard);
        }

        // tell other tasks that this module is being worked on already
        let waiter = Arc::new(Waiter::new());
        let mut guard = shared.cache.write().await;
        guard.insert(path.clone(), CompilationState::InProgress(waiter));
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

        for module in source.inner.uses.iter() {

            let module_path = path.with_file_name("").join(source.arena.get(module.path));

            // compile the module (caching handeled inside compile)
            let fut = shared.executor.spawn(compile::<I>(Arc::clone(&shared), module_path.clone()));
            futs.push(fut);

        }

        // wait for all the dependencies to be built
        let results = util::join_all(futs).await;
        let mut deps = Vec::with_capacity(results.len());
        for result in results {
            deps.push(result?);
        }

        drop(deps); // <- do something with the dependencies

        // we now can genrate code for this translation unit

        let base_program = match basegen::basegen(source) {
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

        let program = match typegen::typecheck(base_program) {
            Ok(val) => val,
            Err(err) => {
                if shared.opts.debug() {
                    Diagnostic::debug("typecheck failed").emit();
                }
                typegen::format_error(err).emit();
                return Err(())
            }
        };
        
        let result = Arc::new(program);

        // store the compiled module inside the cache
        let mut guard = shared.cache.write().await;
        let entry = guard.get_mut(&path).expect("module must be present");
        if let CompilationState::InProgress(waitstate) = entry {
            waitstate.flag.store(true, Ordering::SeqCst); // todo: what ordering to use here
            waitstate.event.notify(usize::MAX); // todo: use notify_relaxed here?
        } else {
            unreachable!()
        }
        *entry = CompilationState::Done(Arc::clone(&result));
        drop(guard);

        Ok(result)

    }.boxed()

}

fn split_path_unwrap(path: path::PathBuf) -> Option<(path::PathBuf, OsString)> {
    Some((path.parent()?.to_path_buf(), path.file_name()?.to_os_string()))
}

struct SharedState<'a, I: Send + Sync> {
    pub opts: cli::Opts,
    pub executor: async_executor::Executor<'a>,
    pub cache: async_lock::RwLock<HashMap<path::PathBuf, CompilationState<I>>>,
}

enum CompilationState<I> {
    InProgress(Arc<Waiter>), // inside an Arc, so we can access it without always holding the lock
    Done(Arc<TranslationUnit<Program<I>>>) // inside an Arc, so we can access it without always holding the lock
}

struct Waiter {
    pub event: event_listener::Event,
    pub flag: AtomicBool
}

impl Waiter {
    pub fn new() -> Self {
        Self {
            event: event_listener::Event::new(),
            flag: AtomicBool::new(false)
        }
    }
    pub async fn wait(&self) {
        let listener = event_listener::EventListener::new(&self.event);
        futures_lite::pin!(listener);
        loop {
            if self.flag.load(Ordering::Acquire) { break }
            if listener.as_mut().is_listening() { listener.as_mut().await }
            else { listener.as_mut().listen() }
        }
    }
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

