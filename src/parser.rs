
use nom::{branch::alt, multi::{many0, many1, fold_many0}, sequence::{pair, delimited, preceded, terminated, tuple}, bytes::complete::{tag, escaped_transform, is_not, take_until}, combinator::{not, map, recognize, eof, value, cut, verify, peek, map_res, opt}, character::complete::{char, alpha1, alphanumeric1, multispace0, one_of, multispace1}, error::{context, VerboseErrorKind, VerboseError}, IResult, Finish};
use nom_locate::LocatedSpan;
use std::cell::RefCell;
use crate::{diagnostic, arena};

pub(crate) fn parse<'d>(dat: &'d str) -> Result<TranslationUnit, FinalParseError<'d>> {
    let arena = RefCell::new(arena::StrArena::default());
    parse_items(LocatedSpan::new_extra(dat, &arena)).finish()
        .map_err(|err| ignore_extra(err))
        .map(|res| res.1)
        .map(move |mut res| {
            res.arena = arena.into_inner();
            res
        })
}

fn ignore_extra<'a, 'b>(val: ParseError<'a, 'b>) -> FinalParseError<'a> {
    VerboseError { errors: val.errors.into_iter().map(|(dat, kind)| (
            unsafe { LocatedSpan::new_from_raw_offset(dat.location_offset(), dat.location_line(), dat.into_fragment(), ()) },
            kind
        )).collect() }
}

pub(crate) fn parse_items<'a, 'b>(dat: ParseInput<'a, 'b>) -> ParseResult<'a, 'b, TranslationUnit> {
    terminated(
        fold_many0(delimited(multispace0, parse_item, multispace0), TranslationUnit::default, assign_item),
        context("expected top level item", pair(multispace0, eof)) // this is there to allow empty files
    )(dat)
}

fn assign_item<'a>(mut dest: TranslationUnit, item: Item) -> TranslationUnit {
    match item {
        Item::Comment      => (),
        Item::Use(val)     => dest.uses.push(val),
        Item::FunDef(val)  => dest.funs.push(val),
        Item::TypeDef(val) => dest.types.push(val),
    }
    dest
}

pub(crate) fn parse_item<'a, 'b>(dat: ParseInput<'a, 'b>) ->  ParseResult<'a, 'b, Item> {
    alt((
        parse_comment,
        preceded(tag("use"),  cut(parse_use)),
        preceded(tag("fn"),   cut(parse_fun_def)),
        preceded(tag("type"), cut(parse_type_def)),
    ))(dat)
}

pub(crate) fn parse_comment<'a, 'b>(dat: ParseInput<'a, 'b>) -> ParseResult<'a, 'b, Item> {
    alt((
        value(Item::Comment, pair(tag("//"), take_until("\n"))),
        value(Item::Comment, delimited(tag("/*"), take_until("*/"), tag("*/"))),
    ))(dat)
}

pub(crate) fn parse_use<'a, 'b>(dat: ParseInput<'a, 'b>) -> ParseResult<'a, 'b, Item> {
    map(
        tuple((multispace0, cut(parse_str_basic))),
        |(_, path)| Item::Use(Use { path })
    )(dat)
}

pub(crate) fn parse_fun_def<'a, 'b>(dat: ParseInput<'a, 'b>) -> ParseResult<'a, 'b, Item> {
    map(
        tuple((
            preceded(multispace0, parse_ident),
            preceded(multispace0, opt(parse_signature)),
            preceded(multispace0, parse_block),
        )),
        |(name, signature, block)| Item::FunDef(FunDef { name, signature: signature.unwrap_or_default(), body: block, span: Span::from_located_span(&dat) })
    )(dat)
}

pub(crate) fn parse_type_def<'a, 'b>(dat: ParseInput<'a, 'b>) -> ParseResult<'a, 'b, Item> {
    map(
        tuple((
            preceded(multispace0, cut(parse_ident)),
            preceded(multispace0, cut(parse_partial_signature)),
        )),
        |(name, inner)| Item::TypeDef(TypeDef { name, signature: inner, span: Span::from_located_span(&dat) })
    )(dat)
}

pub(crate) fn parse_partial_signature<'a, 'b>(dat: ParseInput<'a, 'b>) -> ParseResult<'a, 'b, Vec<Op>> {
    context("expected partial signature", delimited(
        char('('),
        cut(many0(delimited(multispace0, parse_op, multispace0))),
        char(')')
    ))(dat)
}

pub(crate) fn parse_signature<'a, 'b>(dat: ParseInput<'a, 'b>) -> ParseResult<'a, 'b, Vec<Op>> {
    context("expected signature", delimited(
        char('('),
        cut(alt((
            value(Vec::new(), tag("variadic")),
            many0(delimited(multispace0, parse_op, multispace0))
        ))),
        char(')')
    ))(dat)
}

pub(crate) fn parse_block<'a, 'b>(dat: ParseInput<'a, 'b>) -> ParseResult<'a, 'b, Block> {
    delimited(
        context("expected block", char('{')),
        delimited(multispace0, many0(delimited(multispace0, parse_op, multispace0)), multispace0),
        context("invalid operation",  char('}'))
    )(dat)
}

pub(crate) fn parse_op<'a, 'b>(dat: ParseInput<'a, 'b>) -> ParseResult<'a, 'b, Op> {
    map(
        alt((
            alt((
                value(OpKind::Copy,   char('*')),
                value(OpKind::Drop,   char('~')),
                value(OpKind::Over,   char('+')),
                value(OpKind::Swap,   char('-')),
                value(OpKind::Addr,   char('&')),
                value(OpKind::Read,   char('>')),
                value(OpKind::Write,  char('<')),
                value(OpKind::Type,   char('?')),
                value(OpKind::Size,   char('!')), // todo: use ! for something different
                value(OpKind::Access, char(':')),
                value(OpKind::Arrow,  tag("->")),
                value(OpKind::Rot3,   terminated(tag("rot3"),   multispace1)),
                value(OpKind::Rot4,   terminated(tag("rot4"),   multispace1)),
                // value(OpKind::Move,   terminated(tag("move~"),  multispace1)),
            )),
            alt((
                value(OpKind::Add, terminated(tag("add"), multispace1)),
                value(OpKind::Sub, terminated(tag("sub"), multispace1)),
                value(OpKind::Mul, terminated(tag("mul"), multispace1)),
                value(OpKind::Dvm, terminated(tag("dvm"), multispace1)),
                value(OpKind::Not, terminated(tag("not"), multispace1)),
                value(OpKind::And, terminated(tag("and"), multispace1)),
                value(OpKind::Or,  terminated(tag("or"),  multispace1)),
                value(OpKind::Xor, terminated(tag("xor"), multispace1)),
                value(OpKind::Eq,  terminated(tag("eq"),  multispace1)),
                value(OpKind::Gt,  terminated(tag("gt"),  multispace1)),
                value(OpKind::Gte, terminated(tag("gte"), multispace1)),
                value(OpKind::Lt,  terminated(tag("lt"),  multispace1)),
                value(OpKind::Lte, terminated(tag("lte"), multispace1)),
            )),
            alt((
                map(preceded(pair(tag("if"),   multispace1), cut(parse_block)), |block| OpKind::If   { block }),
                map(preceded(pair(tag("elif"), multispace1), cut(parse_block)), |block| OpKind::Elif { block }),
                map(preceded(pair(tag("else"), multispace1), cut(parse_block)), |block| OpKind::Else { block }),
                map(preceded(pair(tag("loop"), multispace1), cut(parse_block)), |block| OpKind::Loop { block }),
                map(preceded(pair(tag("for"),  multispace1), cut(parse_block)), |block| OpKind::For  { block }),
                value(OpKind::Break, tag("break")),
            )),
            alt((
                // comments
                value(OpKind::Nop, parse_comment),
                // boolean literals
                value(OpKind::Push { value: Literal::Bool(true)  }, tag("true")),
                value(OpKind::Push { value: Literal::Bool(false) }, tag("false")),
                // integer literals
                map(
                    terminated(parse_integer, context("identifier cannot start with a number", cut(peek(not(alpha1))))),
                    |integer| OpKind::Push { value: Literal::Int(integer) }
                ),
                // string literals
                map(parse_str_escaped, |string| OpKind::Push { value: Literal::Str(string) }),
                // tuple literals
                map(parse_tuple_literal, |ops| OpKind::Push { value: Literal::Tuple(ops) }),
                // identifiers
                map(parse_ident, |name| OpKind::Call { name }),
            ))
        )),
        |kind| Op { kind, span: Span::from_located_span(&dat) }
    )(dat)
}

pub(crate) fn parse_integer<'a, 'b>(dat: ParseInput<'a, 'b>) -> ParseResult<'a, 'b, usize> {

    use nom::{Err as NomErr, error::ErrorKind};

    let parse_int = |radix| move |input: ParseInput| usize::from_str_radix(input.into_fragment(), radix);

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

pub(crate) fn parse_ident<'a, 'b>(dat: ParseInput<'a, 'b>) -> ParseResult<'a, 'b, IdentStr> {
    map(
        context("identifier cannot be a keyword", verify(
            context("expected identifier", recognize(pair(alpha1, many0(alt((alphanumeric1, tag("-"))))))),
            |ident: &ParseInput| !matches!(ident.into_fragment(), "fn" | "type" | "if" | "elif" | "else" | "loop" | "for" | "break" | "true" | "false")
        )),
        to_identstr
    )(dat)
}

pub(crate) fn parse_str_basic<'a, 'b>(dat: ParseInput<'a, 'b>) -> ParseResult<'a, 'b, IdentStr> {
    context("expected string literal", map(
        delimited(char('\"'), is_not("\""), char('\"')),
        to_identstr
    ),
    )(dat)
}

pub(crate) fn parse_str_escaped<'a, 'b>(dat: ParseInput<'a, 'b>) -> ParseResult<'a, 'b, String> {
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

pub(crate) fn parse_tuple_literal<'a, 'b>(dat: ParseInput<'a, 'b>) -> ParseResult<'a, 'b, Vec<Op>> {
    delimited(
        char('('),
        many0(delimited(multispace0, parse_op, multispace0)),
        char(')')
    )(dat)
}

pub(crate) type ParseResult<'a, 'b, O> = IResult<ParseInput<'a, 'b>, O, ParseError<'a, 'b>>;
pub(crate) type ParseInput<'a, 'b> = LocatedSpan<&'a str, &'b RefCell<arena::StrArena>>;
pub(crate) type ParseError<'a, 'b> = VerboseError<ParseInput<'a, 'b>>;
pub(crate) type FinalParseError<'a> = VerboseError<LocatedSpan<&'a str>>;

pub(crate) fn format_error(value: FinalParseError) -> diagnostic::Diagnostic {

    let mut diag = diagnostic::Diagnostic::error("invalid source file");

//    if let Some((span, _)) = value.errors.first() {
//        let code = std::str::from_utf8(span.get_line_beginning()).expect("invalid utf-8");
//        let (before, highlight, after) = split_at_char_index(code, span.get_utf8_column() - 1);
//        diag.code = Some(format!("{}\x1b[4m{}\x1b[24m{}", before, highlight, after).trim().to_string());
//        diag.pos  = Some(diagnostic::Pos { line: span.location_line() as usize, column: span.get_column(), offset: span.location_offset() });
//    }
//
//    for (_, kind) in value.errors.iter() {
//        if let VerboseErrorKind::Context(message) = kind {
//            diag.notes.push(message.to_string());
//            break
//        }
//    }

    diag

}

fn split_at_char_index(input: &str, index: usize) -> (&str, &str, &str) {
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

#[derive(Debug, Default)]
pub(crate) struct TranslationUnit {
    pub uses: Vec<Use>,
    pub funs: Vec<FunDef>,
    pub types: Vec<TypeDef>,
    pub arena: arena::StrArena,
}

pub(crate) type Block = Vec<Op>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Item {
    Comment,
    Use(Use),
    FunDef(FunDef),
    TypeDef(TypeDef),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Use {
    pub path: IdentStr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct FunDef {
    pub name: IdentStr,
    pub span: Span,
    pub signature: Vec<Op>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct TypeDef {
    pub name: IdentStr,
    pub span: Span,
    pub signature: Vec<Op>, // todo: ByteCode?
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Type {
    // special type used for typechecking
    // Any,
    // standard types
    Int,
    // Bool,
    // Char,
    // Ptr { inner: Box<Type> }, // todo: add slice type
    // fine-grained integer types
    // U8,
    // U16,
    // U32,
    // U64,
    // I8,
    // I16,
    // I32,
    // I64,
    // type
    // Type,
    // complex types
    // Array { inner: Box<Type>, length: usize },
    // Tuple { inner: Vec<Type> },
    // user types (newtype)
    // New { name: &'a str, size: u64 }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Op {
    pub(crate) kind: OpKind,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum OpKind {

    Nop, // this is currently only used to implement comments
    Push { value: Literal },

    Call { name: IdentStr },

    Copy,
    Over,
    Swap,
    Rot3,
    Rot4,
    Drop,

    Read,
    Write,
    Move,

    Addr,
    Type,
    Size,
    Access,

    Arrow,

    Add,
    Sub,
    Mul,
    Dvm,

    Not,
    And,
    Or,
    Xor,
    Eq,
    Gt,
    Gte,
    Lt,
    Lte,

    If   { block: Block },
    Elif { block: Block },
    Else { block: Block },

    Loop { block: Block },
    For  { block: Block },
    Break,

}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Literal {
    Int(usize),
    Bool(bool),
    Str(String), // owned String because we already processed escape sequences
    Type(Type),
    Tuple(Vec<Op>),
}

pub(crate) type IdentStr = arena::Id;

fn to_identstr(value: ParseInput) -> IdentStr {
    let mut arena = value.extra.borrow_mut();
    arena.put(value.fragment()) // allocate the literal inside the arena
}

#[derive(Debug, Clone, Default, Copy, PartialEq, Eq, Hash)] // todo: do we really need this many derives on all of the types
pub(crate) struct Span {
    pub(crate) offset: u32,
    pub(crate) line: u32,
    pub(crate) column: u32,
}

impl Span {
    pub(crate) fn from_located_span(value: &ParseInput) -> Self {
        let offset = value.location_offset() as u32;
        Span {
            offset,
            line: value.location_line(),
            column: value.get_utf8_column() as u32,
        }
    }
    pub(crate) fn to_pos(self) -> diagnostic::Pos {
        diagnostic::Pos {
            line: self.line as usize,
            column: self.column as usize,
            offset: self.offset as usize
        }
    }
}

