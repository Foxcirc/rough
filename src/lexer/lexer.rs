
use std::str::Chars;
use std::iter::Peekable;
use crate::lexer::token::{stream::TokenStream, token::{Token, TokenKind, IntegerBase}};
use crate::lexer::{possible::Possible, check::Check};
use crate::lexer::pos::Pos;
use crate::lexer::tick::Tick;
use crate::lexer::error::{LexError, LexResult};

/// The Lexer converts Text to a Stream of Tokens.
/// 
/// The lexing is done something like this:
/// 
/// 0. Call Lexer::next to build the next token
/// 1. Get the next character.
/// 2. Update the list of tokens, this char is valid for.
/// 3. Check if this is the end of a multi-char token.
/// 4. Do it again.
/// 
/// Note: These steps are done in a different order then listed here.
/// 
pub(crate) struct Lexer<'a> {
    /// The text to lex.
    text: (Peekable<Chars<'a>>, Pos),
    /// Stores the current character.
    current: char,
    /// Stores the previous character.
    previous: char,
    /// Stores the characters that make up a token.
    buffer: String,
    /// The kind of token wich is being build.
    kind: TokenKind,
    /// Used to decide what kind of token to generate.
    /// Represents all Tokens the Sequence could currently be.
    possible: Possible,
}

/// The Lexer::run function will call Lexer::next and
/// do assertions about the state in the meantime.
impl <'b>Lexer<'b> {

    /// Constructor
    pub(crate) fn new(text: &'b mut String) -> Self {
        
        text.push(' ');

        Self {
            text: (text.chars().peekable(), Pos::new()),
            current: '\0',
            previous: '\0',
            buffer: String::new(),
            kind: TokenKind::Empty,
            possible: Possible::default(),
        }
    }
    
    /// Converts the whole Text into a TokenStream and consumes the Lexer.
    /// Inbetween the calls to Lexer::next, some assertions about
    /// the current state _should_ be done.
    pub(crate) fn run(mut self) -> LexResult<TokenStream> {
        
        let mut tree = TokenStream::new();
        
        // while let Some(token) = self.next() {
        //     tree.push(token);
        // } 
        
        loop {
            let token = match self.next() {
                Ok(v) => v,
                Err(LexError::EndOfInput) => break,
                Err(e) => return Err(e),
            };

            tree.push(token);
        }

        return Ok(tree)
        
    }
    
    /// Generate the next token. Returns an error if the
    /// Token could not be parsed.
    fn next(&mut self) -> LexResult<Token> {
        
        //? Skip over spaces and tabs, encountered while there's no matching going on.
        loop {
            match self.text.peek() {
                Some(chr) if *chr == ' ' || *chr == '\t' => { self.text.tick().unwrap(); }, // advance self.text
                None => return Err(LexError::EndOfInput), // return None if the whole text has been lexed
                _ => break // stop skipping at the character wich is not a space
            }
        }
        
        //? Reset some of the state.
        unsafe { self.possible.clear(); } // See the 'Clear' trait for more.
        self.buffer.clear();
        self.kind = TokenKind::Empty;
        self.previous = self.current;
        self.current = '\0'; // This is probably reduntant
        // self.branch = self.text.clone();
        
        //? Loop until the kind of token could be determined.
        loop {
            
            self.previous = self.current;

            // Get the next character. This uses peek, since the end of one Token
            // may be the start of another one.
            self.current = match self.text.peek() {
                Some(v) => *v,
                None => unreachable!()
            };

            if self.current == 'A' {
                let _x = 1;
            }
            
            let set = self.possible.set();
            
            // Check if this is the end of a multi-char token.
            // The first invalid character is considered the end.
            if self.possible.peek(self.current, self.previous) == 0 {
                
                // If there are currently no possible tokens, and this char also ins't valid for any,
                // the character is invalid. This *should* never happen.
                if set == 0 { panic!("Lexer: Invalid sequence at '{}', could not match to Token.", self.current) }
        
                // This is an Integer token. Every integer is also a valid float, so clear the float flag.
                if set == 2 && self.possible[TokenKind::Integer(IntegerBase::Decimal)] && self.possible[TokenKind::Float] {
                    self.possible[TokenKind::Float] = false;
                }
                
                // There should be only one possible token left by now,
                // since this is the end of a token.
                debug_assert!(self.possible.set() == 1);
                break;

            }
            
            // Update self.possible for the current char.
            self.possible.update(self.current, self.previous);
                                    
            // There is only one possibility for the kind of token left. Multi character tokens are scanned to the end.
            if self.possible.set() == 1 && !self.possible.multichar() { break; }
            
            // Push the current character onto the buffer, so it can be processed later.
            self.buffer.push(self.current);

            // Advance self.text, this is done here, because otherwise several characters
            // that follow multi-char tokens would be skipped.
            self.text.tick();
        }

        //? Now, that there's only one possible kind of token left, parse the 
        //? sequence as that kind of token.

        self.kind = self.possible.only();

        return self.build();

    }
    
    fn build(&mut self) -> LexResult<Token> {

        //? Parsing the sequence as eg. an integer is done here.

        macro_rules! check {
            ($eval:expr => $pos:expr, $target:expr, $seq:expr, $orig:expr) => {
                match $eval {
                    Ok(v) => v,
                    Err(_) => { return Err(LexError::InvalidSequence { pos: $pos, target: $target, seq: $seq, orig: $orig } ) }
                }
            };
        }

        return match self.kind {
            TokenKind::Empty   => { panic!("Lexer: Empty token not allowed at this point.") },
            TokenKind::Newline   => { self.text.tick(); Ok(Token::Newline) },
            TokenKind::Symbol(v) => { self.text.tick(); Ok(Token::Symbol(v)) },
            TokenKind::Brace(v)  => { self.text.tick(); Ok(Token::Brace(v)) },
    
            TokenKind::Integer(IntegerBase::Decimal) => {
                let valid: String = self.buffer.chars().filter(|e| *e != '_').collect(); // yes, this is inefficient
                let result = check!(isize::from_str_radix(&valid, 10) => self.text.1, self.kind, valid, self.buffer.clone());
                Ok(Token::Integer(result))
            },
            TokenKind::Integer(IntegerBase::Hexadecimal) => {
                let valid: String = self.buffer.chars().filter(|e| !(*e == '_' || *e == 'x')).collect();
                let result = check!(isize::from_str_radix(&valid, 16) => self.text.1, self.kind, valid, self.buffer.clone());
                Ok(Token::Integer(result))
            },
            TokenKind::Integer(IntegerBase::Binary) => {
                let valid: String = self.buffer.chars().filter(|e| !(*e == '_' || *e == 'b')).collect();
                let result = check!(isize::from_str_radix(&valid, 2) => self.text.1, self.kind, valid, self.buffer.clone());
                Ok(Token::Integer(result))
            },
            TokenKind::Float   => {
                let valid: String = self.buffer.chars().filter(|e| *e != '_').collect();
                let result = check!(valid.parse() => self.text.1, self.kind, valid, self.buffer.clone());
                Ok(Token::Float(result))
            },
        }
    }
}
