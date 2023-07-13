
use nom::{IResult, branch::alt, multi::{count, many1, many0}, sequence::{tuple, pair, terminated, delimited, preceded}, bytes::complete::{tag, take_while}, combinator::{cut, opt, recognize, map, eof, success, verify, peek, fail, not, value}, character::{complete::{alpha1, alphanumeric1, line_ending, multispace1, multispace0, space1, space0}}, complete::take, error::ParseError as NomParseError};

pub struct Parser {
    
}

impl Parser {

    pub fn parse(dat: &str) -> Result<Code, ParseError> {

        let (dat, words) = many0(Self::spaced(Self::parse_pitem))(dat)?;
        eof(dat)?;

        println!("Words: {:?}", words);

        Ok(Code {
            funs: Vec::new(),
            types: Vec::new(),
        })

    }

    pub(crate) fn parse_pitem(dat: &str) ->  IResult<&str, PItem> {
        let result = alt((
            preceded(tag("fun"), cut(Self::parse_fun)),
            preceded(tag("new"), cut(Self::parse_type)),
        ))(dat)?;
        Ok(result)
    }

    pub(crate) fn parse_type(dat: &str) -> IResult<&str, PItem> {
        nom::combinator::fail(dat)
    }

    pub(crate) fn parse_fun(dat: &str) -> IResult<&str, PItem> {
        let (dat, name) = Self::parse_fun_head(dat)?;
        let (dat, block) = Self::parse_block(dat)?;
        let fun = Fun { sig: Vec::new(), block };
        let item = PItem::Fun(Item { name, quals: Vec::new(), inner: fun });
        Ok((dat, item))
    }

    pub(crate) fn parse_fun_head(dat: &str) -> IResult<&str, &str> {
        Self::spaced(Self::parse_ident)(dat)
    }

    pub(crate) fn parse_block(dat: &str) -> IResult<&str, Block> {
        delimited(tag("{"), many0(Self::spaced(Self::parse_op)), tag("}"))(dat)
    }

    pub(crate) fn parse_op(dat: &str) -> IResult<&str, Op> {
        let (mut dat, ident) = Self::parse_ident(dat)?;
        let op = match ident {
            "add" => Op::Add,
            "sub" => Op::Sub,
            "mul" => Op::Mul,
            "div" => Op::Div,
            "mod" => Op::Mod,
            "if" => {
                let (next, block) = Self::spaced(Self::parse_block)(dat)?;
                dat = next;
                Op::If { block }
            },
            "elif" => {
                let (next, block) = Self::spaced(Self::parse_block)(dat)?;
                dat = next;
                Op::Elif { block }
            },
            "else" => {
                let (next, block) = Self::spaced(Self::parse_block)(dat)?;
                dat = next;
                Op::Else { block }
            },
            "eq" => Op::Eq,
            _other => todo!(),
        };
        Ok((dat, op))
    }

    pub(crate) fn parse_ident(dat: &str) -> IResult<&str, &str> {
        recognize(pair(alt((alpha1, tag("-"))), many0(alt((alphanumeric1, tag("-"))))))(dat)
    }

    fn spaced<'d, O, E: nom::error::ParseError<&'d str>, P: nom::Parser<&'d str, O, E>>(parser: P) -> impl FnMut(&'d str) -> IResult<&'d str, O, E> {
        delimited(multispace0, parser, multispace0)
    }

}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum PItem<'a> {
    Fun(Item<'a, Fun>),
    Type(Item<'a, Type>),
}

#[derive(Debug, PartialEq)]
pub struct ParseError<'a> {
    inner: nom::Err<nom::error::Error<&'a str>>,
}

impl<'a> From<nom::Err<nom::error::Error<&'a str>>> for ParseError<'a> {
    fn from(inner: nom::Err<nom::error::Error<&'a str>>) -> Self {
        Self { inner }
    }
}

#[derive(Debug, PartialEq)]
pub struct Code<'a> {
    pub funs: Vec<Item<'a, Fun>>,
    pub types: Vec<Item<'a, Type>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item<'a, T> {
    pub name: &'a str,
    pub quals: Vec<ItemQual>,
    pub inner: T,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemQual {
    Pub,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fun {
    pub sig: Vec<Type>,
    pub block: Block,
}

pub type Block = Vec<Op>;

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Push,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    If { block: Block },
    Elif { block: Block },
    Else { block: Block },
    Eq,
}

