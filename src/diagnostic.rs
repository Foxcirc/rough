
use fancy::colorize;

#[derive(Clone)]
pub(crate) enum Level {
    Debug,
    Info,
    Warning,
    Error,
}

#[derive(Clone)]
pub(crate) struct Pos {
    pub(crate) line: usize,
    pub(crate) column: usize,
}

#[derive(Clone)]
pub(crate) struct Diag {
    pub(crate) level: Level,
    pub(crate) message: String,
    pub(crate) notes: Vec<String>,
    pub(crate) file: Option<String>,
    pub(crate) pos: Option<Pos>,
    pub(crate) code: Option<String>,
}

impl Diag {

    pub fn new() -> Self {
        Self {
            level: Level::Info,
            message: String::new(),
            notes: Vec::new(),
            file: None,
            pos: None,
            code: None,
        }
    }

    pub fn level(mut self, level: Level) -> Self {
        self.level = level;
        self
    }

    pub fn say<S: ToString>(mut self, message: S) -> Self {
        self.message = message.to_string();
        self
    }

    pub fn note<S: ToString>(mut self, message: S) -> Self {
        self.notes.push(message.to_string());
        self
    }

    pub fn code<S: ToString>(mut self, code: S) -> Self {
        self.code = Some(code.to_string());
        self
    }

    pub fn file<S: ToString>(mut self, path: S) -> Self {
        self.file = Some(path.to_string());
        self
    }

    pub fn pos(mut self, pos: Pos) -> Self {
        self.pos = Some(pos);
        self
    }

    pub fn format(&self) -> String {

        // -- example --
        // info: compiling foo.rh
        // error: use of unstable feature `enums`
        //     at lexer/token.rh:240:96
        //     in `let Tokenkind enum {`
        //   note: enable this feature using `#{unstable-feature: enums}`
        //   note: enumerations aren't stable yet, please consider using `std:Enum` for now

        let mut output = String::with_capacity(
            self.message.len() +
            self.code.clone().map(|s| s.len()).unwrap_or(0) +
            self.notes.iter().map(|s| s.len()).sum::<usize>()
        );

        match self.level {
            Level::Debug   => output.push_str(&colorize!("[white]debug: {}\n", self.message)),
            Level::Info    => output.push_str(&colorize!("[blue]info: {}\n", self.message)),
            Level::Warning => output.push_str(&colorize!("[yellow]warning: {}\n", self.message)),
            Level::Error   => output.push_str(&colorize!("[red]error: {}\n", self.message)),
        }

        match (&self.file, &self.pos) {
            (Some(path), Some(pos)) => output.push_str(&colorize!("    [246]in {}:{}:{}\n", path, pos.line, pos.column)),
            (Some(path), None     ) => output.push_str(&colorize!("    [246]in file {}\n", path)),
            (None,       Some(pos)) => output.push_str(&colorize!("    [246]at position {}:{}\n", pos.line, pos.column)),
            (..) => (),
        }

        if let Some(code) = &self.code {
            output.push_str(&colorize!("    [245]at `{}`\n", code));
        }

        for note in self.notes.clone() {
            output.push_str(&colorize!("[30]  note: [247]{}\n", note));
        }

        output

    }

    #[must_use]
    pub fn debug<S: ToString>(text: S) -> Self {
        Self::new()
            .level(Level::Debug)
            .say(text)
    }

    #[must_use]
    pub fn info<S: ToString>(text: S) -> Self {
        Self::new()
            .level(Level::Info)
            .say(text)
    }

    #[must_use]
    pub fn warning<S: ToString>(text: S) -> Self {
        Self::new()
            .level(Level::Warning)
            .say(text)
    }

    #[must_use]
    pub fn error<S: ToString>(text: S) -> Self {
        Self::new()
            .level(Level::Error)
            .say(text)
    }

    pub fn emit(self) {
        eprintln!("{}", self.format());
    }

}

