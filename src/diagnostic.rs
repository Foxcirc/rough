
use fancy::colorize;

#[derive(Clone)]
pub(crate) enum Level {
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

    pub fn say(mut self, message: &str) -> Self {
        self.message = message.into();
        self
    }

    pub fn note(mut self, message: &str) -> Self {
        self.notes.push(message.into());
        self
    }

    pub fn code(mut self, code: &str) -> Self {
        self.code = Some(code.into());
        self
    }

    pub fn file(mut self, path: &str) -> Self {
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
            Level::Info    => output.push_str(&colorize!("[blue]info: {}\n", self.message)),
            Level::Warning => output.push_str(&colorize!("[yellow]warning: {}\n", self.message)),
            Level::Error   => output.push_str(&colorize!("[red]error: {}\n", self.message)),
        }

        match (&self.file, &self.pos) {
            (Some(path), Some(pos)) => output.push_str(&colorize!("    [246]at {}:{}:{}\n", path, pos.line, pos.column)),
            (Some(path), None     ) => output.push_str(&colorize!("    [246]in file {}\n", path)),
            (None,       Some(pos)) => output.push_str(&colorize!("    [246]at {}:{}\n", pos.line, pos.column)),
            (..) => (),
        }

        if let Some(code) = &self.code {
            output.push_str(&colorize!("    [245]in `{}`\n", code));
        }

        for note in self.notes.clone() {
            output.push_str(&colorize!("[30]  note: [247]{}\n", note));
        }

        output

    }

    pub fn info(text: &str) -> Self {
        Self::new()
            .level(Level::Info)
            .say(text)
    }

    pub fn warning(text: &str) -> Self {
        Self::new()
            .level(Level::Warning)
            .say(text)
    }

    pub fn error(text: &str) -> Self {
        Self::new()
            .level(Level::Error)
            .say(text)
    }

}

