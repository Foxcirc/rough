
use std::str::Chars;
use std::iter::Peekable;
use std::ops::{Index, IndexMut};
use crate::lexer::token::{stream::TokenStream, token::{Token, TokenKind, Symbol, Brace}};
// use super::error::{Error, ErrorKind, /* Step */};

/// The Lexer converts Text to a Stream of Tokens.
/// 
/// The lexing is done something like this:
/// 
/// 0. Call Lexer::next to build the next token
/// 1. No tokens are possible
/// 2. Read chars and update the list of possible `TokenKind`s
/// 3. If there is only one `TokenKind` left. Go back to the start of the sequence.
/// 4. Read the characters again and parse them as the selected TokenKind.
/// 5. Repeat until there are no more chars left.
/// 
pub(crate) struct Lexer<'a> {
    /// The text to lex.
    text: Peekable<Chars<'a>>,
    /// A copy of self.text wich is temporarily made inside Lexer::next.
    branch: Peekable<Chars<'a>>,
    /// Stores the current character.
    current: char,
    /// Stores the previous character, if any.
    previous: char,
    /// Stores the characters that make up a token.
    buffer: String,
    /// The kind of token wich is being build.
    kind: TokenKind,
    /// Used to decide what kind of token to generate.
    /// Represents all Tokens the Sequence could currently be.
    possible: Possible,
    /// The current position in the Text. [line, column, char]
    cursor: [usize; 3]
}

/// The Lexer should not return any Result<>, because a Lexer
/// can't have invalid Input and all internal Errors should be panics.
// todo nevermind, what would "\'1s be? right, garbage 

/// The Lexer::run function will call Lexer::next and
/// do assertions about the state in the meantime.
impl <'b>Lexer<'b> {

    /// Constructor
    pub(crate) fn new(text: &'b mut String) -> Self {

        text.push(' ');

        Self {
            text: text.chars().peekable(),
            branch: text.chars().peekable(),
            current: '\0',
            previous: '\0',
            buffer: String::new(),
            kind: TokenKind::Empty,
            possible: Possible([false; 11]),
            cursor: [0, 0, 0]
        }
    }
    
    /// Converts the whole Text into a TokenStream and consumes the Lexer.
    /// Inbetween the calls to Lexer::next, some assertions about
    /// the current state are done.
    pub(crate) fn run(mut self) -> TokenStream {
        
        let mut tree = TokenStream::new();
        
        while let Some(token) = self.next() {
            tree.push(token);
        } 
        
        return tree
        
    }
    
    /// Generate the next token. Returns an error if the
    /// Token could not be parsed.
    fn next(&mut self) -> Option<Token> {
        
        //* This is probably a somewhat inperformant solution.
        //* ... it is elegant though, at least from my perspective
        
        // Clear the old possible tokens. See the 'Clear' trait for more.
        unsafe { self.possible.clear(); }
        self.buffer.clear();
        self.kind = TokenKind::Empty;
        self.previous = self.current;
        self.current = '\0'; //? This is probably reduntant
        self.branch = self.text.clone();
        
        // First loop until the kind of token could be determined.
        loop {
            
            self.previous = self.current;

            // Get the next character. This uses peek, since the end of one Token
            // may be the start of another one.
            self.current = match self.branch.next() {
                Some(v) => v,
                None => return None
            };
            
            if self.current == '1' { //* for debugging purposes
                let _x = 1;
            }
            // // Advance the cursor, the line only if the character is a newline.
            // self.cursor[0] += 1;
            // self.cursor[2] += 1;
            // if self.current == '\n' {
                //     self.cursor[0] = 0;
                //     self.cursor[1] += 1;
                // }            
                
                let set = self.possible.set();
                
                // Skip over spaces and tabs, encountered while there's no matching going on.
                if set == 0 && matches!(self.current, ' ' | '\t') { self.text.next(); continue; };
                
                //? Check if this is the end of a token. The end is
                //? considered when there  *would* be no more valid
                //? Tokens left.         
                
                // Check if there would be no more possibilities left after self.possible
                // is updated with the current char. 
                if self.possible.peek(self.current, self.previous) == 0 {
                    
                    // This is an Integer token. Every integer is also a valid float, so clear the float flag.
                    if set == 2 && self.possible[TokenKind::Integer] && self.possible[TokenKind::Float] {
                        self.possible[TokenKind::Float] = false;
                    }
                    
                    // assert!(self.possible.set() == 1);
                    break;
            }
            
            // Update self.possible for the current char.
            self.possible.update(self.current, self.previous);
            
            let set = self.possible.set();
            
            // There is only one possibility for the kind of token left.
            if set == 1 { break; }

            // There are no possible kinds of token left. This should never happen.
            else if set == 0 { panic!("Lexer: Invalid sequence at '{}', could not match to Token.", self.current) }

        }

        self.kind = self.possible.only();

        return Some(self.build());

    }
    
    fn build(&mut self) -> Token {
        return match self.kind {
            TokenKind::Empty   => { panic!("Lexer: Empty token not allowed at this point.") },
            TokenKind::Newline   => { self.text.next(); Token::Newline },
            TokenKind::Symbol(v) => { self.text.next(); Token::Symbol(v) }, // todo make it TokenKind::Symbol and parse the symbol here #consistency
            TokenKind::Brace(v)  => { self.text.next(); Token::Brace(v) },
    
            TokenKind::Integer   => {
    
                loop {
                    self.current = *self.text.peek().expect("Lexer: Iterator drained while building token.");
                    
                    // Store chars until the next whitespace.
                    if !matches!(self.current, '0'..='9' | '_') { break; }
                    
                    // Don't push the undercores, since these are invalid for an integer.
                    if !matches!(self.current, '_') { 
                        self.buffer.push(self.current);
                    }
                    
                    // Advance the iterator.
                    self.text.next();
                }
                
                let result = isize::from_str_radix(&self.buffer, 10).expect(&format!("Lexer: Could not build token for sequence '{}', invalid sequence for <Integer>", self.buffer));
                Token::Integer(result)
            },
            
            TokenKind::Float   => {
                
                loop {
                    self.current = *self.text.peek().expect("Lexer: Iterator drained while building token.");
                    
                    // Store chars until the next whitespace.
                    if !matches!(self.current, '0'..='9' | '_' | '.') { break; }
                    
                    // Don't push the undercores, since these are invalid for an integer.
                    if !matches!(self.current, '_') { 
                        self.buffer.push(self.current);
                    }

                    // Advance the iterator.
                    self.text.next();
                }
    
                let result = self.buffer.parse().expect(&format!("Lexer: Could not build token for sequence '{}', invalid sequence for <Float>", self.buffer));
                Token::Float(result)
            },
        }
    }
}
// todo update this
/// Basically Bitflags, but with the whole "bit" thing removed.
/// 
/// Indexing by TokenKind will return the corresponding
/// integer index into the inner buffer.
/// -----------------------  Layout ----------------------------
/// 0  Empty - this should never be set
/// 1  Newline
/// 2  Symbol
/// 3  Brace
/// 4  Integer
/// 5  Float
/// ------------------------------------------------------------
#[derive(Debug)]
struct Possible([bool; 11]);

impl Possible {
    /// Clears all flags.
    /// This may actually be undefined behaviour
    /// but it works so i don't care
    ///
    /// fuck it doesn't work
    /// 
    /// Ah it writes size_of::<T> * count bytes. But whhyyyy?
    pub(crate) unsafe fn clear(&mut self) {
        std::ptr::write_bytes(self, 0, 1);
    }
    
    /// Returns how many flags are set.
    // todo find a good lighter approach for this
    pub(crate) fn set(&self) -> usize {
        self.0.iter().filter(|i| **i).count()
    }

    /// Returns the kind of Token. Mey only be
    /// called if only one item is set. Otherwise
    /// is UnDeFiNeD bEhAVioUr, just kidding
    /// there's a check.
    pub(crate) fn only(&self) -> TokenKind {
        assert!(self.set() == 1);
        
        return
            
            if      self[TokenKind::Empty]                   { TokenKind::Empty }
            else if self[TokenKind::Newline]                   { TokenKind::Newline }
            
            else if self[TokenKind::Symbol(Symbol::Plus)]      { TokenKind::Symbol(Symbol::Plus) }
            else if self[TokenKind::Symbol(Symbol::Minus)]     { TokenKind::Symbol(Symbol::Minus) }
            else if self[TokenKind::Symbol(Symbol::Star)]      { TokenKind::Symbol(Symbol::Star) }
            else if self[TokenKind::Symbol(Symbol::Slash)]     { TokenKind::Symbol(Symbol::Slash) }
            else if self[TokenKind::Symbol(Symbol::Equal)]     { TokenKind::Symbol(Symbol::Equal) }
            
            else if self[TokenKind::Brace(Brace::NormalOpen)]  { TokenKind::Brace(Brace::NormalOpen) }
            else if self[TokenKind::Brace(Brace::NormalClose)] { TokenKind::Brace(Brace::NormalClose) }

            else if self[TokenKind::Integer]                   { TokenKind::Integer }
            else if self[TokenKind::Float]                     { TokenKind::Float }
            else { unreachable!() };
    }
}

/// Returns the value of the inner array for this TokenKind as reference.
impl Index<TokenKind> for Possible {
    type Output = bool;
    fn index(&self, index: TokenKind) -> &Self::Output {
        match index {
            TokenKind::Empty                   => &self.0[0],
            TokenKind::Newline                   => &self.0[1],
            TokenKind::Symbol(Symbol::Plus)      => &self.0[2],
            TokenKind::Symbol(Symbol::Minus)     => &self.0[3],
            TokenKind::Symbol(Symbol::Star)      => &self.0[4],
            TokenKind::Symbol(Symbol::Slash)     => &self.0[5],
            TokenKind::Symbol(Symbol::Equal)     => &self.0[6],
            TokenKind::Brace(Brace::NormalOpen)  => &self.0[7],
            TokenKind::Brace(Brace::NormalClose) => &self.0[8],
            TokenKind::Integer                   => &self.0[9],
            TokenKind::Float                     => &self.0[10],
        }
    }
}

/// Returns the value of the inner array for this TokenKind as mutable reference.
impl IndexMut<TokenKind> for Possible {
    fn index_mut(&mut self, index: TokenKind) -> &mut Self::Output {
        match index {
            TokenKind::Empty   => &mut self.0[0],
            TokenKind::Newline   => &mut self.0[1],
            TokenKind::Symbol(Symbol::Plus) => &mut self.0[2],
            TokenKind::Symbol(Symbol::Minus) => &mut self.0[3],
            TokenKind::Symbol(Symbol::Star) => &mut self.0[4],
            TokenKind::Symbol(Symbol::Slash) => &mut self.0[5],
            TokenKind::Symbol(Symbol::Equal) => &mut self.0[6],
            TokenKind::Brace(Brace::NormalOpen)  => &mut self.0[7],
            TokenKind::Brace(Brace::NormalClose)  => &mut self.0[8],
            TokenKind::Integer   => &mut self.0[9],
            TokenKind::Float     => &mut self.0[10],
        }
    }
}

/// Implemented for Possible.
/// This trait abstracts the checking if specific combinations of chars
/// are valid for a specific TokenKind.
trait Check {
    /// Update the flags according to chr.
    fn update(&mut self, chr: char, prev: char);
    /// Peek how many flags would be valid after a call to Char::update(chr).
    fn peek(&self, chr: char, prev: char) -> usize;
}

impl Check for Possible {
    fn update(&mut self, chr: char, _prev: char) {

        /*
            self[TokenKind::Something] = valid!{ // TODO Program this macro. 
                valid('a' | 'b' | 'c'); // what chars are valid
                previous('1' | '2'); // what previous chars are valid
                set(TokenKind::Other & TokenKind::What) // what flags have to be already set
            }
        */

        let set = self.set();
        // todo Put !self[TokenKind::Integer] into the float match, since a float isn't a valid integer.
        // todo This would make the places matching is done, more consistent.
        self[TokenKind::Newline]                   = matches!(chr, '\n') && set == 0;

        self[TokenKind::Symbol(Symbol::Plus)]      = matches!(chr, '+') && set == 0;
        self[TokenKind::Symbol(Symbol::Minus)]     = matches!(chr, '-') && set == 0;
        self[TokenKind::Symbol(Symbol::Star)]      = matches!(chr, '*') && set == 0;
        self[TokenKind::Symbol(Symbol::Slash)]     = matches!(chr, '/') && set == 0;
        self[TokenKind::Symbol(Symbol::Equal)]     = matches!(chr, '=') && set == 0;
        self[TokenKind::Brace(Brace::NormalOpen)]  = matches!(chr, '(') && set == 0;
        self[TokenKind::Brace(Brace::NormalClose)] = matches!(chr, ')') && set == 0;
       
        self[TokenKind::Integer]                   = matches!(chr, '0'..='9' | '_')       && (set == 0 || self[TokenKind::Integer]);
        self[TokenKind::Float]                     = matches!(chr, '0'..='9' | '_' | '.') && (set == 0 || self[TokenKind::Integer] || self[TokenKind::Float]);
    }
    fn peek(&self, chr: char, _prev: char) -> usize {

        let mut count = 0;
        let set = self.set();

        count += (matches!(chr, '\n') && set == 0) as usize;

        count += (matches!(chr, '+') && set == 0) as usize;
        count += (matches!(chr, '-') && set == 0) as usize;
        count += (matches!(chr, '*') && set == 0) as usize;
        count += (matches!(chr, '/') && set == 0) as usize;
        count += (matches!(chr, '=') && set == 0) as usize;
        count += (matches!(chr, '(') && set == 0) as usize;
        count += (matches!(chr, ')') && set == 0) as usize;
       
        count += (matches!(chr, '0'..='9' | '_')       && (set == 0 || self[TokenKind::Integer])) as usize;
        count += (matches!(chr, '0'..='9' | '_' | '.') && (set == 0 || !self[TokenKind::Float] || self[TokenKind::Integer])) as usize;

        return count

    }
}
