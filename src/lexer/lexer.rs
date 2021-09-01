
use std::str::Chars;
use std::iter::Peekable;
use crate::lexer::token::{stream::TokenStream, token::{Token, TokenKind}};
use crate::lexer::{possible::Possible, check::Check};
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
            possible: Possible::default(),
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
            self.current = match self.text.next() {
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

                // Check if there would be no more possibilities left after self.possible
                // is updated with the current char. 
                // This is only for tokens, wich consist of more then one character,
                // like Integers or Identifiers.
                if self.possible.peek(self.current, self.previous) == 0 {
                    
                    // This is an Integer token. Every integer is also a valid float, so clear the float flag.
                    if set == 2 && self.possible[TokenKind::Integer] && self.possible[TokenKind::Float] {
                        self.possible[TokenKind::Float] = false;
                    }
                    
                    // There should be only one possible token left by now,
                    // since this is the end of a token.
                    assert!(self.possible.set() == 1);
                    break;
            }
            
            // Push the current char onto the buffer, since it is valid for at least
            // one kind of token that is currently being build.
            self.buffer.push(self.current);

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
