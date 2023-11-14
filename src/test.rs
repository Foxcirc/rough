
use std::fs;
use crate::{parser::{self, Type}, typegen};

fn scnd<A, B>(tuple: (A, B)) -> B {
    tuple.1
}

// #[test]
// fn basic_parse() {
// 
//     assert_eq!(parser::parse_ident(ParseInput::new("foo")).map(scnd), Ok("foo".to_string()));
//     assert_eq!(parser::parse_ident(ParseInput::new("-foo")).map(scnd), Ok("-foo".to_string()));
//     assert_eq!(parser::parse_ident(ParseInput::new("foo-bar")).map(scnd), Ok("foo-bar".to_string()));
//     assert_eq!(parser::parse_ident(ParseInput::new("foo123")).map(scnd), Ok("foo123".to_string()));
//     assert!(parser::parse_ident(ParseInput::new("123foo")).is_err());
// 
//     assert_eq!(parser::parse_op(ParseInput::new("add")).map(scnd), Ok(OpKind::Add));
//     assert_eq!(parser::parse_str_escaped(ParseInput::new("\"add\"")).map(scnd), Ok(String::from("add")));
//     assert_eq!(parser::parse_str_escaped(ParseInput::new("\"\\\"\"")).map(scnd),  Ok(String::from("\"")));
// 
//     assert_eq!(parser::parse_block(ParseInput::new("{add\tadd}")).map(scnd), Ok(vec![OpKind::Add, OpKind::Add]));
//     assert_eq!(parser::parse_block(ParseInput::new("{\nadd add\n    add add\n\n}")).map(scnd), Ok(vec![OpKind::Add, OpKind::Add, OpKind::Add, OpKind::Add]));
// 
// }

#[test]
fn full_parse() {

    let code = fs::read_to_string("src/test/main.rh").expect("Cannot not read file");

    let result = parser::parse(&code);

    match result {
        Ok(val) => println!("{val:?}"),
        Err(err) => panic!("\n{}\n", parser::format_error(err).format()),
        // Err(err) => panic!("\n{err:?}\n"),
    }

}

