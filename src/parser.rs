
use nom::{branch::alt, multi::{many0, many1, fold_many0}, sequence::{pair, delimited, preceded, terminated, tuple}, bytes::complete::{tag, escaped_transform, is_not}, combinator::{not, map, recognize, eof, value, cut, verify, peek, map_res}, character::complete::{char, alpha1, alphanumeric1, multispace0, one_of}, error::{context, VerboseErrorKind, VerboseError}, IResult, Finish};
use nom_locate::LocatedSpan;
use std::ops;
use crate::diagnostic;

pub(crate) fn parse(dat: &str) -> Result<ItemList, ParseError> {
    parse_items(LocatedSpan::new(dat)).finish().map(|(_, items)| items)
}

pub(crate) fn parse_items(dat: ParseInput) -> ParseResult<ItemList> {
    terminated(
        fold_many0(delimited(multispace0, parse_item, multispace0), ItemList::default, assign_item),
        context("expected top level item", eof)
    )(dat)
}

fn assign_item<'a>(mut dest: ItemList, item: Item) -> ItemList {
    match item {
        Item::Use(val)  => dest.uses.push(val),
        Item::Fun(val)  => dest.funs.push(val),
        Item::Type(val) => dest.types.push(val),
    }
    dest
}

pub(crate) fn parse_item(dat: ParseInput) ->  ParseResult<Item> {
    alt((
        preceded(tag("use"),  parse_use),
        preceded(tag("fn"),   parse_fun),
        preceded(tag("type"), parse_type),
    ))(dat)
}

pub(crate) fn parse_use(dat: ParseInput) -> ParseResult<Item> {
    map(
        tuple((multispace0, cut(parse_str_basic))),
        |(_, path)| Item::Use(Use { path })
    )(dat)
}

pub(crate) fn parse_fun(dat: ParseInput) -> ParseResult<Item> {
    map(
        tuple((multispace0, cut(parse_ident), multispace0, cut(parse_block))),
        |(_, name, _, block)| Item::Fun(Fun { name, signature: Vec::new(), block })
    )(dat)
}

pub(crate) fn parse_type(dat: ParseInput) -> ParseResult<Item> {
    map(
        tuple((multispace0, cut(parse_ident))),
        |(_, _)| Item::Type(Type::Int)
    )(dat)
}

pub(crate) fn parse_block(dat: ParseInput) -> ParseResult<Block> {
    delimited(
        context("expected block", char('{')),
        delimited(multispace0, many0(delimited(multispace0, parse_op, multispace0)), multispace0),
        context("invalid operation",  char('}'))
    )(dat)
}

pub(crate) fn parse_op(dat: ParseInput) -> ParseResult<Op> {
    context("Op", alt((
        alt((
            value(Op::Copy,   char('*')),
            value(Op::Drop,   char('~')),
            value(Op::Read,   char('>')),
            value(Op::Write,  char('<')),
            value(Op::Move,   char('~')),
            value(Op::Addr,   char('&')),
            value(Op::Type,   char('?')),
            value(Op::Size,   char('!')),
            value(Op::Access, char('.')),
        )),
        alt((
            value(Op::Add,   tag("add")),
            value(Op::Sub,   tag("sub")),
            value(Op::Mul,   tag("mul")),
            value(Op::Div,   tag("div")),
            value(Op::Mod,   tag("mod")),
        )),
        alt((
            map(preceded(pair(tag("if"),   multispace0), cut(parse_block)), |block| Op::If   { block }),
            map(preceded(pair(tag("elif"), multispace0), cut(parse_block)), |block| Op::Elif { block }),
            map(preceded(pair(tag("else"), multispace0), cut(parse_block)), |block| Op::Else { block }),
            map(preceded(pair(tag("loop"), multispace0), cut(parse_block)), |block| Op::Loop { block }),
            map(preceded(pair(tag("for"),  multispace0), cut(parse_block)), |block| Op::For  { block }),
            value(Op::Break, tag("break")),
        )),
        alt((
            value(Op::Push { value: Literal::Bool(true) },  tag("true")),
            value(Op::Push { value: Literal::Bool(false) }, tag("false")),
        )),
        alt((
            map(
                terminated(parse_integer, context("identifier cannot start with a number", cut(peek(not(alpha1))))),
                |integer| Op::Push { value: Literal::Int(integer) }
            ),
            map(parse_str_escaped, |string|  Op::Push { value: Literal::Str(string) }),
            map(parse_ident,       |ident|   Op::Call { ident })
        )),
    )))(dat)
}

pub(crate) fn parse_integer(dat: ParseInput) -> ParseResult<u64> {

    use nom::{Err as NomErr, error::ErrorKind};

    let parse_int = |radix| move |input: ParseInput| u64::from_str_radix(input.into_fragment(), radix);

    let mut result: IResult<_, _, ParseError> = context("expected integer", alt((
        map_res(preceded(alt((tag("0x"), tag("0X"))), recognize(many1(one_of("0123456789abcdefABCDEF")))), parse_int(16)),
        map_res(preceded(alt((tag("0b"), tag("0B"))), recognize(many1(one_of("01")))),                     parse_int(2)),
        map_res(                                      recognize(many1(one_of("0123456789"))),              parse_int(10)),
    )))(dat);

    // check if the integer literal was too large and make that a hard error
    if let Err(NomErr::Error(mut val)) = result {
        let verbose_error = val.errors.get(val.errors.len() - 3);
        if let Some((span, VerboseErrorKind::Nom(ErrorKind::MapRes))) = verbose_error {
            let span_clone = span.clone();
            let ctx = val.errors.last_mut().expect("No last error"); // swap the last error with this
            *ctx = (span_clone, VerboseErrorKind::Context("integer literal too large"));
            result = Err(NomErr::Failure(val))
        } else {
            result = Err(NomErr::Error(val))
        }
    }

    result
}

pub(crate) fn parse_ident(dat: ParseInput) -> ParseResult<Span> {
    map(
        context("identifier cannot be a keyword", verify(
            context("expected identifier", recognize(pair(alt((alpha1, tag("-"))), many0(alt((alphanumeric1, tag("-"))))))),
            |ident: &ParseInput| !matches!(ident.into_fragment(), "fn" | "type" | "if" | "elif" | "else" | "loop" | "for" | "break" | "true" | "false")
        )),
        to_span
    )(dat)
}

pub(crate) fn parse_str_basic(dat: ParseInput) -> ParseResult<Span> {
    context("expected string literal", map(
        delimited(char('\"'), is_not("\""), char('\"')),
        to_span
    ),
    )(dat)
}

pub(crate) fn parse_str_escaped(dat: ParseInput) -> ParseResult<String> {
    context("expected string literal", delimited(
        char('\"'),
        cut(context("invalid escape sequence", escaped_transform(
            is_not("\"\\"),
            '\\',
            alt((
                value("\\", tag("\\")),
                value("\"", tag("\"")),
                value("\n", tag("n"))
            ))
        ))),
        char('\"')
    ))(dat)
}

pub(crate) type ParseResult<'a, O> = IResult<ParseInput<'a>, O, ParseError<'a>>;
pub(crate) type ParseInput<'a> = LocatedSpan<&'a str>; // todo convert to LocatedSpan<str> for use with convert_error
pub(crate) type ParseError<'a> = VerboseError<ParseInput<'a>>;

pub(crate) fn format_parse_error(value: ParseError) -> diagnostic::Diag {

    let mut diag = diagnostic::Diag::error("invalid source file");

    if let Some((span, _)) = value.errors.first() {
        let code = std::str::from_utf8(span.get_line_beginning()).expect("Invalid Utf-8");
        let (before, highlight, after) = split_at_byte_index(code, span.get_utf8_column() - 1);
        diag.code = Some(format!("{}\x1b[4m{}\x1b[24m{}", before, highlight, after).trim().to_string());
        diag.pos  = Some(diagnostic::Pos { line: span.location_line() as usize, column: span.get_column() });
        diag.file = Some(String::from("input.rh"));
    }

    for (_, kind) in value.errors.iter() {
        if let VerboseErrorKind::Context(message) = kind {
            diag.notes.push(message.to_string());
            break
        }
    }

    diag

}

fn split_at_byte_index(input: &str, index: usize) -> (&str, &str, &str) {
    let mut before = "";
    let mut target = "";
    let mut after  = "";

    for (char_index, (byte_index, chr)) in input.char_indices().enumerate() {
        if char_index == index {
            target = &input[byte_index..byte_index + chr.len_utf8()];
        } else if char_index > index {
            after = &input[byte_index..];
            break;
        }
        before = &input[..byte_index];
    }

    (before, target, after)
}

#[derive(Debug, Default, PartialEq, Eq, Hash)]
pub(crate) struct ItemList {
    pub uses: Vec<Use>,
    pub funs: Vec<Fun>,
    pub types: Vec<Type>,
}

pub(crate) type Block = Vec<Op>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Item {
    Use(Use),
    Fun(Fun),
    Type(Type),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Use {
    pub path: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Fun {
    pub name: Span,
    pub signature: Vec<Type>,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Type {
    // standard types
    Int,
    Bool,
    Ptr { inner: Box<Type> },
    // fine-grained types
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    // type
    Type,
    // complex types
    Array { inner: Box<Type>, length: u64 },
    Tuple { inner: Vec<Type> },
    // user types (newtype)
    // New { name: &'a str, size: u64 }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Op {
    Push { value: Literal },
    Call { ident: Span },
    Drop,
    Copy,
    Read,
    Write,
    Move,
    Addr,
    Type,
    Size,
    Access,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    If   { block: Block },
    Elif { block: Block },
    Else { block: Block },
    Loop { block: Block },
    Break,
    For  { block: Block },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Literal {
    Int(u64),
    Bool(bool),
    Str(String), // owned String because we already processed escape sequences
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Span {
    pub(crate) inner: ops::Range<usize>
}

impl Span {
    pub(crate) fn inside<'a>(&self, src: &'a str) -> &'a str {
        &src[self.inner.clone()]
        //              ^^^^^^ why the heck does Range not implement Copy
    }
}

/// compute the index range for a given LocatedSpan
fn to_span(value: LocatedSpan<&str>) -> Span {
    let offset = value.location_offset();
    Span { inner: offset..offset + value.len() }
}

