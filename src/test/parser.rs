
use std::fs;
use crate::parser::{self, ParseInput, Op, Span};

fn scnd<A, B>(tuple: (A, B)) -> B {
    tuple.1
}

#[test]
fn basic_parse() {

    assert_eq!(parser::parse_ident(ParseInput::new("foo")).map(scnd), Ok(Span { inner: 0..3 }));
    assert_eq!(parser::parse_ident(ParseInput::new("-foo")).map(scnd), Ok(Span { inner: 0..4 }));
    assert_eq!(parser::parse_ident(ParseInput::new("foo-bar")).map(scnd), Ok(Span { inner: 0..7 }));
    assert_eq!(parser::parse_ident(ParseInput::new("foo123")).map(scnd), Ok(Span { inner: 0..6 }));
    assert!(parser::parse_ident(ParseInput::new("123foo")).is_err());

    assert_eq!(parser::parse_op(ParseInput::new("add")).map(scnd), Ok(Op::Add));
    assert_eq!(parser::parse_str_escaped(ParseInput::new("\"add\"")).map(scnd), Ok(String::from("add")));
    assert_eq!(parser::parse_str_escaped(ParseInput::new("\"\\\"\"")).map(scnd),  Ok(String::from("\"")));

    assert_eq!(parser::parse_block(ParseInput::new("{add\tadd}")).map(scnd), Ok(vec![Op::Add, Op::Add]));
    assert_eq!(parser::parse_block(ParseInput::new("{\nadd add\n    add add\n\n}")).map(scnd), Ok(vec![Op::Add, Op::Add, Op::Add, Op::Add]));

}

#[test]
fn full_parse() {

    let code = fs::read_to_string("src/test/parser.rh").expect("Cannot not read file");

    let result = parser::parse(&code);

    match result {
        Ok(val) => println!("{val:?}"),
        Err(err) => panic!("\n{}\n", crate::parser::format_parse_error(err).format()),
        // Err(err) => panic!("\n{err:?}\n"),
    }

}

