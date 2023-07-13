
use std::fs;

use crate::parser::{Parser, Op, Fun, PItem, Item};

#[test]
fn basic_parse() {

    assert_eq!(Parser::parse_ident("foo"), Ok(("", "foo")));
    assert_eq!(Parser::parse_ident("-foo"), Ok(("", "-foo")));
    assert_eq!(Parser::parse_ident("foo-bar"), Ok(("", "foo-bar")));
    assert_eq!(Parser::parse_ident("foo123"), Ok(("", "foo123")));
    assert!(Parser::parse_ident("123foo").is_err());

    assert_eq!(Parser::parse_op("add"), Ok(("", Op::Add)));

    assert_eq!(Parser::parse_block("{add\tadd}"), Ok(("", vec![Op::Add, Op::Add])));
    assert_eq!(Parser::parse_block("{\nadd add\n    add add\n\n}"), Ok(("", vec![Op::Add, Op::Add, Op::Add, Op::Add])));

    assert_eq!(Parser::parse_fun("main { add add }"), Ok(("", PItem::Fun(Item { name: "main", quals: vec![], inner: Fun { sig: vec![], block: vec![Op::Add, Op::Add] } }))));
    assert_eq!(Parser::parse_pitem("fun main { add add }"), Ok(("", PItem::Fun(Item { name: "main", quals: vec![], inner: Fun { sig: vec![], block: vec![Op::Add, Op::Add] } }))));

}

#[test]
fn full_parse() {

    let code = fs::read_to_string("src/test/parser.rh").expect("Cannot not read file");

    let result = Parser::parse(&code);

    println!("{:?}", result);

    assert!(result.is_ok())

}

