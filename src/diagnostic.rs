
#[derive(Clone)]
pub(crate) enum Level {
    Debug,
    Info,
    Warning,
    Error,
}

impl Level {
    pub fn is_light(&self) -> bool {
        matches!(self, Self::Debug | Self::Info)
    }
}

#[derive(Clone)]
pub(crate) struct Pos {
    pub(crate) line: usize,
    pub(crate) column: usize,
    pub(crate) offset: usize,
}

#[derive(Clone)]
pub(crate) struct Diagnostic {
    pub(crate) level: Level,
    pub(crate) message: String,
    pub(crate) notes: Vec<String>,
    pub(crate) file: Option<String>,
    pub(crate) pos: Option<Pos>,
    pub(crate) code: Option<String>,
}

impl Diagnostic {

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
            self.code.as_ref().map(|s| s.len()).unwrap_or(0) +
            self.notes.iter().map(|s| s.len()).sum::<usize>()
        );

        match self.level {
            Level::Debug   => output.push_str("\x1b[34mdebug: "),
            Level::Info    => output.push_str("\x1b[34minfo: \x1b[0m"),
            Level::Warning => output.push_str("\x1b[33mwarn: \x1b[0m"),
            Level::Error   => output.push_str("\x1b[31merror: \x1b[0m"),
        }

        output.push_str(&self.message);

        // if debug or info add notes into the same line
        if self.level.is_light() {
            for note in self.notes.iter() {
                output.push_str(", ");
                output.push_str(note);
            }
        }

        match (&self.file, &self.pos) {
            (Some(path), Some(pos)) => output.push_str(&format!("\n    in {}:{}:{} at offset {}", path, pos.line, pos.column, pos.offset)),
            (Some(path), None     ) => output.push_str(&format!("\n    in file {}", path)),
            (None,       Some(pos)) => output.push_str(&format!("\n    at position {}:{} at offset {}", pos.line, pos.column, pos.offset)),
            (..) => (),
        }

        if let Some(code) = &self.code {
            output.push_str(&format!("\n    at `{}`", code));
        }

        // if not debug, add notes here
        if !self.level.is_light() {
            for note in self.notes.iter() {
                output.push_str("\n");
                output.push_str("  ");
                output.push_str(note);
            }
        }

        output.push_str("\n");
        output.push_str("\x1b[0m"); // reset color style
        
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
        eprint!("{}", self.format());
    }

}

