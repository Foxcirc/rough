
use nom::{branch::alt, multi::{many0, many1, fold_many0, separated_list1}, sequence::{pair, delimited, preceded, terminated, tuple, separated_pair}, bytes::complete::{tag, escaped_transform, is_not, take_until}, combinator::{not, map, recognize, eof, value, cut, verify, peek, map_res, opt}, character::complete::{char, alpha1, alphanumeric1, multispace0, one_of, multispace1}, error::{context, VerboseErrorKind, VerboseError}, IResult, Finish, Offset};
use nom_locate::LocatedSpan;
use crate::{diagnostic, typecheck::Entity};

pub(crate) fn parse(dat: &str) -> Result<ParseTranslationUnit, ParseError> {
    parse_items(LocatedSpan::new(dat)).finish().map(|(_, items)| items)
}

pub(crate) fn parse_items(dat: ParseInput) -> ParseResult<ParseTranslationUnit> {
    terminated(
        fold_many0(delimited(multispace0, parse_item, multispace0), TranslationUnit::default, assign_item),
        context("expected top level item", pair(multispace0, eof)) // this is there to allow empty files
    )(dat)
}

fn assign_item<'a>(mut dest: ParseTranslationUnit, item: Item) -> ParseTranslationUnit {
    match item {
        Item::Comment   => (),
        Item::Use(val)  => dest.uses.push(val),
        Item::FunDef(val)  => dest.funs.push(val),
        Item::TypeDef(val) => dest.types.push(val),
    }
    dest
}

pub(crate) fn parse_item(dat: ParseInput) ->  ParseResult<Item> {
    alt((
        parse_comment,
        preceded(tag("use"),  cut(parse_use)),
        preceded(tag("fn"),   cut(parse_fun_def)),
        preceded(tag("type"), cut(parse_type_def)),
    ))(dat)
}

pub(crate) fn parse_comment(dat: ParseInput) -> ParseResult<Item> {
    alt((
        value(Item::Comment, pair(tag("//"), take_until("\n"))),
        value(Item::Comment, delimited(tag("/*"), take_until("*/"), tag("*/"))),
    ))(dat)
}

pub(crate) fn parse_use(dat: ParseInput) -> ParseResult<Item> {
    map(
        tuple((multispace0, cut(parse_str_basic))),
        |(_, path)| Item::Use(Use { path })
    )(dat)
}

pub(crate) fn parse_fun_def(dat: ParseInput) -> ParseResult<Item> {
    map(
        tuple((
            preceded(multispace0, parse_ident),
            preceded(multispace0, opt(parse_signature)),
            preceded(multispace0, parse_block),
        )),
        |(name, signature, block)| Item::FunDef(FunDef { name, signature: signature.unwrap_or_default(), body: block, span: Span::from_located_span(&dat) })
    )(dat)
}

pub(crate) fn parse_type_def(dat: ParseInput) -> ParseResult<Item> {
    map(
        tuple((
            preceded(multispace0, cut(parse_ident)),
        )),
        |_| Item::TypeDef(Type::Int)
    )(dat)
}

pub(crate) fn parse_signature(dat: ParseInput) -> ParseResult<ParsedSignature> {
    map(
        delimited(
            char('('),
            separated_pair(
                many0(preceded(multispace0, parse_ident)),
                opt(preceded(multispace0, tag("->"))),
                many0(preceded(multispace0, parse_ident))
            ),
            char(')')
        ),
        |(takes, returns)| Signature { takes, returns }
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
    context("Op", map(
        alt((
            alt((
                value(OpKind::Copy,  char('*')),
                value(OpKind::Over,  char('+')),
                value(OpKind::Swap,  char('-')),
                value(OpKind::Rot3,  terminated(tag("rot3"), multispace1)),
                value(OpKind::Rot4,  terminated(tag("rot4"), multispace1)),
                value(OpKind::Drop,  char('~')),
                value(OpKind::Read,  char('>')),
                value(OpKind::Write, char('<')),
                value(OpKind::Move,  char('~')),
                value(OpKind::Addr,  char('&')),
                value(OpKind::Type,  char('?')),
                value(OpKind::Size,  char('!')),
                value(OpKind::Dot,   char('.')),
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
                value(OpKind::Push { value: Literal::Bool(true) },  tag("true")),
                value(OpKind::Push { value: Literal::Bool(false) }, tag("false")),
            )),
            alt((
                map(
                    terminated(parse_integer, context("identifier cannot start with a number", cut(peek(not(alpha1))))),
                    |integer| OpKind::Push { value: Literal::Int(integer) }
                ),
                map(parse_str_escaped, |string|  OpKind::Push { value: Literal::Str(string) }),
                map(parse_ident,       |name|    OpKind::Call { name })
            )),
        )),
        |kind| Op { kind, span: Span::from_located_span(&dat) }
    ))(dat)
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

pub(crate) fn parse_ident(dat: ParseInput) -> ParseResult<IdentStr> {
    map(
        context("identifier cannot be a keyword", verify(
            context("expected identifier", recognize(pair(alpha1, many0(alt((alphanumeric1, tag("-"))))))),
            |ident: &ParseInput| !matches!(ident.into_fragment(), "fn" | "type" | "if" | "elif" | "else" | "loop" | "for" | "break" | "true" | "false")
        )),
        to_identstr
    )(dat)
}

pub(crate) fn parse_str_basic(dat: ParseInput) -> ParseResult<IdentStr> {
    context("expected string literal", map(
        delimited(char('\"'), is_not("\""), char('\"')),
        to_identstr
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

pub(crate) fn format_error(value: ParseError) -> diagnostic::Diagnostic {

    let mut diag = diagnostic::Diagnostic::error("invalid source file");

    if let Some((span, _)) = value.errors.first() {
        let code = std::str::from_utf8(span.get_line_beginning()).expect("invalid utf-8");
        let (before, highlight, after) = split_at_char_index(code, span.get_utf8_column() - 1);
        diag.code = Some(format!("{}\x1b[4m{}\x1b[24m{}", before, highlight, after).trim().to_string());
        diag.pos  = Some(diagnostic::Pos { line: span.location_line() as usize, column: span.get_column(), offset: span.location_offset() });
    }

    for (_, kind) in value.errors.iter() {
        if let VerboseErrorKind::Context(message) = kind {
            diag.notes.push(message.to_string());
            break
        }
    }

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

#[derive(Debug, Default, PartialEq, Eq, Hash)]
pub(crate) struct TranslationUnit<U> { // todo: dont use generics here :/
    pub uses: U,
    pub funs: Vec<FunDef>,
    pub types: Vec<Type>,
}

pub(crate) type ParseTranslationUnit = TranslationUnit<Vec<Use>>;
pub(crate) type Block = Vec<Op>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Item {
    Comment,
    Use(Use),
    FunDef(FunDef),
    TypeDef(Type),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Use {
    pub path: IdentStr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct FunDef {
    pub name: IdentStr,
    pub span: Span,
    pub signature: ParsedSignature,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Signature<T> {
    pub takes: Vec<T>,
    pub returns: Vec<T>,
}

impl<I> Default for Signature<I> {
    fn default() -> Self {
        Self {
            takes: Vec::default(),
            returns: Vec::default(),
        }
    }
}

pub(crate) type ParsedSignature = Signature<IdentStr>;
pub(crate) type FnSignature = Signature<Entity>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Type {
    // special Any type used for typechecking
    Any,
    // standard types
    Int,
    Bool,
    Char,
    Ptr { inner: Box<Type> }, // todo: add slice type
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
    // Array { inner: Box<Type>, length: u64 },
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
    Dot,

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
    Int(u64),
    Bool(bool),
    Str(String), // owned String because we already processed escape sequences
}

pub(crate) type IdentStr = String;

fn to_identstr(value: ParseInput) -> String {
    value.fragment().to_string()
}

// todo: store everything more efficiently (Arena, Pinned Buffer?) (bumpalo, blink_alloc?)

#[derive(Debug, Clone, Default, Copy, PartialEq, Eq, Hash)] // todo: do we really need this much derives on all of the types
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

