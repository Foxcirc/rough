
use crate::lexer::lexer::Lexer;
use crate::lexer::token::stream::TokenStream;
use crate::lexer::token::token::{Token::*, Symbol::*, Brace::*};

#[test]
fn maths() {

    let mut test: String = "
    
        1 22 333 4444 55555
        1 22 3_3 4__4 5_5_5 6_____ 7___
        1 2. 3.3 4.44 55.55
        1 2. 3.3 4.4_ 5_.55 6.__6 7_._7_ .___1_
        1+22+333+4444+55555+666666+7777777+88888888+999999999
        1/2/3/4/5/6/7/8/9
        1*22/333+4444-55555
        1 + 2 - 3 * 4 / 5
        1 +  2  -   3   *    4    /     5
        \t1 \t+ 2\t
        () (()) ((())) (((())))
        \t(\t)\t
        (1+22+333+4444+55555)
        ( 1 + (  2   - 333   ))
        )-
        +)
        )*
        )/
        1_.2+-*/(+/-/*)((\t\t)) - - -  ***
        0xABCDEF0b4+0b101101
        
    ".to_owned();

    let lexer = Lexer::new(&mut test);
    // Build the TokenStream
    let stream = lexer.run().display_error();
    
    assert!(
        stream ==
        TokenStream::from(vec![Newline, Newline, Integer(1), Integer(22), Integer(333), Integer(4444), Integer(55555), Newline, Integer(1), Integer(22), Integer(33), Integer(44), Integer(555), Integer(6), Integer(7), Newline, Integer(1), Float(2.0), Float(3.3), Float(4.44), Float(55.55), Newline, Integer(1), Float(2.0), Float(3.3), Float(4.4), Float(5.55), Float(6.6), Float(7.7), Float(0.1), Newline, Integer(1), Symbol(Plus), Integer(22), Symbol(Plus), Integer(333), Symbol(Plus), Integer(4444), Symbol(Plus), Integer(55555), Symbol(Plus), Integer(666666), Symbol(Plus), Integer(7777777), Symbol(Plus), Integer(88888888), Symbol(Plus), Integer(999999999), Newline, Integer(1), Symbol(Slash), Integer(2), Symbol(Slash), Integer(3), Symbol(Slash), Integer(4), Symbol(Slash), Integer(5), Symbol(Slash), Integer(6), Symbol(Slash), Integer(7), Symbol(Slash), Integer(8), Symbol(Slash), Integer(9), Newline, Integer(1), Symbol(Star), Integer(22), Symbol(Slash), Integer(333), Symbol(Plus), Integer(4444), Symbol(Minus), Integer(55555), Newline, Integer(1), Symbol(Plus), Integer(2), Symbol(Minus), Integer(3), Symbol(Star), Integer(4), Symbol(Slash), Integer(5), Newline, Integer(1), Symbol(Plus), Integer(2), Symbol(Minus), Integer(3), Symbol(Star), Integer(4), Symbol(Slash), Integer(5), Newline, Integer(1), Symbol(Plus), Integer(2), Newline, Brace(NormalOpen), Brace(NormalClose), Brace(NormalOpen), Brace(NormalOpen), Brace(NormalClose), Brace(NormalClose), Brace(NormalOpen), Brace(NormalOpen), Brace(NormalOpen), Brace(NormalClose), Brace(NormalClose), Brace(NormalClose), Brace(NormalOpen), Brace(NormalOpen), Brace(NormalOpen), Brace(NormalOpen), Brace(NormalClose), Brace(NormalClose), Brace(NormalClose), Brace(NormalClose), Newline, Brace(NormalOpen), Brace(NormalClose), Newline, Brace(NormalOpen), Integer(1), Symbol(Plus), Integer(22), Symbol(Plus), Integer(333), Symbol(Plus), Integer(4444), Symbol(Plus), Integer(55555), Brace(NormalClose), Newline, Brace(NormalOpen), Integer(1), Symbol(Plus), Brace(NormalOpen), Integer(2), Symbol(Minus), Integer(333), Brace(NormalClose), Brace(NormalClose), Newline, Brace(NormalClose), Symbol(Minus), Newline, Symbol(Plus), Brace(NormalClose), Newline, Brace(NormalClose), Symbol(Star), Newline, Brace(NormalClose), Symbol(Slash), Newline, Float(1.2), Symbol(Plus), Symbol(Minus), Symbol(Star), Symbol(Slash), Brace(NormalOpen), Symbol(Plus), Symbol(Slash), Symbol(Minus), Symbol(Slash), Symbol(Star), Brace(NormalClose), Brace(NormalOpen), Brace(NormalOpen), Brace(NormalClose), Brace(NormalClose), Symbol(Minus), Symbol(Minus), Symbol(Minus), Symbol(Star), Symbol(Star), Symbol(Star), Newline, Integer(0xABCDEF0b4), Symbol(Plus), Integer(0b101101), Newline, Newline])
    )

}

#[test]
fn identifiers() {
    
    let mut test: String = "
    
        _ _ _ _
        __ __
        __1 __2_2
        a b c d
        a_ b_ c_ d_
        a1_2 b1_2 c1_2 d1_2
        __f__ __b__
        
    ".to_owned();

    let lexer = Lexer::new(&mut test);
    // Build the TokenStream
    let stream = lexer.run().display_error();

    assert!(
        stream ==
        TokenStream::from(vec![Newline, Newline, Underscore, Underscore, Underscore, Underscore, Newline, Identifier("__".into()), Identifier("__".into()), Newline, Identifier("__1".into()), Identifier("__2_2".into()), Newline, Identifier("a".into()), Identifier("b".into()), Identifier("c".into()), Identifier("d".into()), Newline, Identifier("a_".into()), Identifier("b_".into()), Identifier("c_".into()), Identifier("d_".into()), Newline, Identifier("a1_2".into()), Identifier("b1_2".into()), Identifier("c1_2".into()), Identifier("d1_2".into()), Newline, Identifier("__f__".into()), Identifier("__b__".into()), Newline, Newline])
    )
}

#[test]
fn feature() {

    let mut test: String = "
    
        .1235
        
    ".to_owned();

    let lexer = Lexer::new(&mut test);
    // Build the TokenStream
    let stream = lexer.run().display_error();

    println!("{:?}", stream);

    // assert!(
    //     stream ==
    //     TokenStream::from(vec![])
    // )

}

#[test]
// #[should_panic]
#[ignore]
fn invalid() {
    
    let mut test: String = "
    
        0xF0A01
        
    ".to_owned();

    let lexer = Lexer::new(&mut test);
    // Build the TokenStream
    let stream = lexer.run().display_error();

    println!("{}", stream);

}

trait DisplayError<T> {
    fn display_error(self) -> T;
}

impl<T, E: std::fmt::Display> DisplayError<T> for Result<T, E> {

    fn display_error(self) -> T {

        match self {
            Ok(v) => v,
            Err(e) => {
                eprintln!("{}", e);
                panic!("^^ see above ^^");
            }
        }

    }

}
