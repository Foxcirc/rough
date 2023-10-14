
use std::fs;
use crate::parser::{self, ParseInput, Op};

fn scnd<A, B>(tuple: (A, B)) -> B {
    tuple.1
}

#[test]
fn basic_parse() {

    assert_eq!(parser::parse_ident(ParseInput::new_extra("foo", 0)).map(scnd), Ok("foo".to_string()));
    assert_eq!(parser::parse_ident(ParseInput::new_extra("-foo", 0)).map(scnd), Ok("-foo".to_string()));
    assert_eq!(parser::parse_ident(ParseInput::new_extra("foo-bar", 0)).map(scnd), Ok("foo-bar".to_string()));
    assert_eq!(parser::parse_ident(ParseInput::new_extra("foo123", 0)).map(scnd), Ok("foo123".to_string()));
    assert!(parser::parse_ident(ParseInput::new_extra("123foo", 0)).is_err());

    assert_eq!(parser::parse_op(ParseInput::new_extra("add", 0)).map(scnd), Ok(Op::Add));
    assert_eq!(parser::parse_str_escaped(ParseInput::new_extra("\"add\"", 0)).map(scnd), Ok(String::from("add")));
    assert_eq!(parser::parse_str_escaped(ParseInput::new_extra("\"\\\"\"", 0)).map(scnd),  Ok(String::from("\"")));

    assert_eq!(parser::parse_block(ParseInput::new_extra("{add\tadd}", 0)).map(scnd), Ok(vec![Op::Add, Op::Add]));
    assert_eq!(parser::parse_block(ParseInput::new_extra("{\nadd add\n    add add\n\n}", 0)).map(scnd), Ok(vec![Op::Add, Op::Add, Op::Add, Op::Add]));

}

#[test]
fn full_parse() {

    let code = fs::read_to_string("src/test/parser.rh").expect("Cannot not read file");

    let result = parser::parse(&code, 0);

    match result {
        Ok(val) => println!("{val:?}"),
        Err(err) => panic!("\n{}\n", parser::format_error(err).format()),
        // Err(err) => panic!("\n{err:?}\n"),
    }

}

