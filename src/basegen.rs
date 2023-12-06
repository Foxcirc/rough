

use std::{fmt, collections::HashMap};
use crate::{parser::{Literal, OpKind, Op, Span, Identifier, TranslationUnit, ParsedItems, TypeDef}, diagnostic::Diagnostic, arch::Intrinsic, arena};

pub(crate) fn basegen<I: Intrinsic>(source: TranslationUnit<ParsedItems>) -> Result<TranslationUnit<BaseProgram<I>>, CodegenError> {

    let mut part = TranslationUnit {
        inner: BaseProgram {
            funs: HashMap::with_capacity(source.inner.funs.len()),
            types: HashMap::with_capacity(source.inner.types.len())
        },
        arena: source.arena,
        main: source.main
    };

    for item in source.inner.types.into_iter() {

        // generate bytecode for the signature
        let mut signature_state = State {
            bytecode: Vec::with_capacity(item.signature.len()),
            counter: 0,
            arena: &part.arena
        };

        // todo: process signature as a tuple and generalize basegen for tuples
        codegen_block(&mut signature_state, item.signature, None)?;
        signature_state.bytecode.push(Instr::spanned(InstrKind::Return, item.span)); // todo: generate return for signature?

        let fun = TypeWithMetadata {
            signature: signature_state.bytecode,
        };

        part.inner.types.insert(item.name, fun);
    
    }

    for item in source.inner.funs.into_iter() {

        // generate bytecode for the signature
        let mut signature_state = State {
            bytecode: Vec::with_capacity(item.signature.len()),
            counter: 0,
            arena: &part.arena
        };

        // todo: process signature as a tuple and generalize basegen for tuples
        codegen_block(&mut signature_state, item.signature, None)?;
        signature_state.bytecode.push(Instr::spanned(InstrKind::Return, item.span)); // todo: generate return for signature?

        // generate bytecode for the body
        let mut body_state = State {
            bytecode: Vec::with_capacity(item.body.len()),
            counter: 0,
            arena: &part.arena
        };

        codegen_block(&mut body_state, item.body, None)?;
        body_state.bytecode.push(Instr::spanned(InstrKind::Return, item.span));

        let fun = FunWithMetadata {
            signature: signature_state.bytecode,
            body: body_state.bytecode,
        };

        part.inner.funs.insert(item.name, fun);
    
    }

    Ok(part)

}

fn codegen_block<I: Intrinsic>(state: &mut State<I>, block: Vec<Op>, loop_escape: Option<Label>) -> Result<(), CodegenError> {

    for op in block {

        match op.kind {

            OpKind::Nop => (),
            OpKind::Push { value: Literal::Tuple(tuple) } => {
                // generate instructions for this tuple
                // let mut inner_state = State { bytecode: Vec::new(), counter: state.counter, arena: state.arena };
                // codegen_block(&mut inner_state, tuple, None)?;
                // todo: push the things onto the stack and then create a tuple:new-n
                todo!("impl tuple literal");
            },
            OpKind::Push { value: parsed } => {
                let value = InstrLiteral::from_literal(parsed);
                state.bytecode.push(Instr::spanned(InstrKind::Push { value }, op.span))
            },

            OpKind::Call { name }  => {
                let inrinsic = I::basegen(&*state.arena.get(name));
                let result = match inrinsic {
                    Some(val) => InstrKind::Intrinsic(val),
                    None => InstrKind::Call { to: name }
                };
                state.bytecode.push(Instr::spanned(result, op.span));
            },

            OpKind::Dup  => state.bytecode.push(Instr::spanned(InstrKind::Dup, op.span)),
            OpKind::Over  => state.bytecode.push(Instr::spanned(InstrKind::Over, op.span)),
            OpKind::Swap  => state.bytecode.push(Instr::spanned(InstrKind::Swap, op.span)),
            OpKind::Rot3  => state.bytecode.push(Instr::spanned(InstrKind::Rot3, op.span)),
            OpKind::Rot4  => state.bytecode.push(Instr::spanned(InstrKind::Rot4, op.span)),
            OpKind::Drop  => state.bytecode.push(Instr::spanned(InstrKind::Drop, op.span)),

            OpKind::Read  => state.bytecode.push(Instr::spanned(InstrKind::Read,  op.span)),
            OpKind::Move  => state.bytecode.push(Instr::spanned(InstrKind::Move,  op.span)),
            OpKind::Write => state.bytecode.push(Instr::spanned(InstrKind::Write, op.span)),

            OpKind::Addr   => state.bytecode.push(Instr::spanned(InstrKind::Addr, op.span)),
            OpKind::Type   => todo!(),
            OpKind::Size   => todo!(),
            OpKind::Access => todo!(),

            OpKind::Arrow => state.bytecode.push(Instr::spanned(InstrKind::Arrow, op.span)),

            OpKind::Add => state.bytecode.push(Instr::spanned(InstrKind::Add, op.span)),
            OpKind::Sub => state.bytecode.push(Instr::spanned(InstrKind::Sub, op.span)),
            OpKind::Mul => state.bytecode.push(Instr::spanned(InstrKind::Mul, op.span)),
            OpKind::Dvm => state.bytecode.push(Instr::spanned(InstrKind::Dvm, op.span)),

            OpKind::Not => state.bytecode.push(Instr::spanned(InstrKind::Not, op.span)),
            OpKind::And => state.bytecode.push(Instr::spanned(InstrKind::And, op.span)),
            OpKind::Or  => state.bytecode.push(Instr::spanned(InstrKind::Or,  op.span)),
            OpKind::Xor => state.bytecode.push(Instr::spanned(InstrKind::Xor, op.span)),

            OpKind::Eq  => state.bytecode.push(Instr::spanned(InstrKind::Eq,  op.span)),
            OpKind::Gt  => state.bytecode.push(Instr::spanned(InstrKind::Gt,  op.span)),
            OpKind::Gte => state.bytecode.push(Instr::spanned(InstrKind::Gte, op.span)),
            OpKind::Lt  => state.bytecode.push(Instr::spanned(InstrKind::Lt,  op.span)),
            OpKind::Lte => state.bytecode.push(Instr::spanned(InstrKind::Lte, op.span)),

            OpKind::If { block: if_block } => {

                let end = state.next_label();
                state.bytecode.push(Instr::spanned(InstrKind::Bne { to: end }, op.span));
                codegen_block(state, if_block, loop_escape)?;
                state.bytecode.push(Instr::spanned(InstrKind::Label { label: end, producer: Producer::If }, op.span));

            },
            OpKind::Elif { block: _block } => todo!(),
            OpKind::Else { block: else_block } => {

                // check that this `else` is coming directly after an `if`
                if !matches!(state.bytecode.last().map(|val| &val.kind), Some(InstrKind::Label { producer: Producer::If, .. })) {
                    return Err(CodegenError::spanned(CodegenErrorKind::InvalidElse, op.span))
                }

                let end = state.next_label();
                state.bytecode.insert(state.bytecode.len() - 1, Instr::spanned(InstrKind::Bra { to: end }, op.span));
                codegen_block(state, else_block, loop_escape)?;
                state.bytecode.push(Instr::spanned(InstrKind::Label { label: end, producer: Producer::Else }, op.span));

            },
            OpKind::Loop { block: loop_block } => {

                let start = state.next_label();
                let escape = state.next_label();
                state.bytecode.push(Instr::spanned(InstrKind::Label { label: start, producer: Producer::Loop }, op.span));
                codegen_block(state, loop_block, Some(escape))?;
                state.bytecode.push(Instr::spanned(InstrKind::Bra { to: start }, op.span));
                state.bytecode.push(Instr::spanned(InstrKind::Label { label: escape, producer: Producer::Break }, op.span));

            },
            OpKind::For  { block: _block } => todo!(),
            OpKind::Break => {

                let escape = match loop_escape {
                    Some(val) => val,
                    None => return Err(CodegenError::spanned(CodegenErrorKind::InvalidBreak, op.span)),
                };
                state.bytecode.push(Instr::spanned(InstrKind::Bra { to: escape }, op.span))

            },
        };

    };

    Ok(())

}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum InstrLiteral {
    Int(usize),
    Bool(bool),
    Type(TypeFull),
    Str(String),
}

impl InstrLiteral {
    pub fn from_literal(parsed: Literal) -> Self {
        match parsed {
            Literal::Int(val)  => Self::Int(val),
            Literal::Bool(val) => Self::Bool(val),
            Literal::Str(val)  => Self::Str(val),
            Literal::Type(val)  => Self::Type(val),
            Literal::Tuple(..) => unreachable!(),
        }
    }
}

#[derive(PartialEq, Eq, Clone)]
pub(crate) struct Instr<I> {
    pub(crate) kind: InstrKind<I>,
    pub(crate) span: Span,
}

impl<I: fmt::Debug> fmt::Debug for Instr<I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

impl<I> Instr<I> {
    pub(crate) fn spanned(kind: InstrKind<I>, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum InstrKind<I> {

    Label { label: Label, producer: Producer }, // todo: rename?

    Push { value: InstrLiteral },

    Call { to: Identifier },
    Return,

    Drop,
    Dup,
    Over,
    Swap,
    Rot3,
    Rot4,

    Read,
    Move,
    Write,

    Addr,
    // Type,
    // Size,

    // Access,

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

    Bne { to: Label },
    Bra { to: Label },

    Intrinsic(I),

}

pub(crate) type Label = usize;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Producer {
    If,
    Else,
    Loop,
    Break,
}

struct State<'a, I> {
    pub bytecode: Vec<Instr<I>>,
    pub counter: usize,
    pub arena: &'a arena::StrArena
}

impl<'a, I> State<'a, I> {
    pub fn next_label(&mut self) -> usize {
        self.counter += 1;
        self.counter
    }
}

pub(crate) struct BaseProgram<I> {
    pub funs: HashMap<Identifier, FunWithMetadata<I>>,
    pub types: HashMap<Identifier, TypeWithMetadata<I>>,
}

// cannot derive Default because of the generics
impl<I> Default for BaseProgram<I> {
    fn default() -> Self {
        Self {
            funs: HashMap::new(),
            types: HashMap::new(),
        }
    }
}

pub(crate) enum CommonIdentifier {
    Common(&'static str),
    Runtime(Identifier)
}

impl CommonIdentifier {
    fn get<'a>(&self, arena: &'a arena::StrArena) -> &'a str {
        match self {
            CommonIdentifier::Common(val) => val,
            CommonIdentifier::Runtime(val) => arena.get(*val)
        }
    }
}

pub(crate) struct Program<I> {
    pub funs: HashMap<Identifier, FunWithMetadata<I>>,
    pub types: HashMap<Identifier, TypeWithMetadata<I>>,
}

pub(crate) type Bytecode<I> = Vec<Instr<I>>;
pub(crate) type BytecodeSlice<I> = [Instr<I>];

fn find_instr_kind<I: Intrinsic>(bytecode: &BytecodeSlice<I>, instr_kind: InstrKind<I>) -> Option<usize> {
    bytecode.iter().position(|item| item.kind == instr_kind)
}

fn find_label<I: Intrinsic>(bytecode: &BytecodeSlice<I>, label: Label, producer: Producer) -> Option<usize> {
    find_instr_kind(bytecode, InstrKind::Label { label, producer })
}

#[derive(Debug, Clone)]
pub(crate) struct FunWithMetadata<I> { // todo: rename to basegenFun
    pub signature: Bytecode<I>,
    pub body: Bytecode<I>,
}

#[derive(Debug, Clone)]
pub(crate) struct TypeWithMetadata<I> { // todo: rename to basegenFun
    pub signature: Bytecode<I>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct TypeFull {
    pub comptime: bool,
    pub t: Type,
}

impl TypeFull {
    pub fn runtime(t: Type) -> Self {
        Self { comptime: false, t }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Type {
    // special type used for typechecking, not to the same as `any` inside signatures
    // Any,
    // the type of types
    Type,
    // standard types
    Int,
    Bool,
    // todo: use Rc to store something like Ptr so cloning is cheap
    // Char,
    // Ptr { inner: Box<Type> }, // todo: add slice type
    // Slice { ptr: Box<Type>, len: usize },
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
    // Array { inner: Box<Type>, len: usize },
    // Tuple { inner: Vec<Type> },
}

pub(crate) struct CodegenError {
    pub(crate) kind: CodegenErrorKind,
    pub(crate) span: Span,
}

impl CodegenError {
    pub(crate) fn spanned(kind: CodegenErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}

pub(crate) enum CodegenErrorKind {
    InvalidElse,
    InvalidBreak,
}

pub(crate) fn format_error(value: CodegenError) -> Diagnostic {
    let diag = match &value.kind {
        CodegenErrorKind::InvalidElse => Diagnostic::error("`else` block withput `if` block"),
        CodegenErrorKind::InvalidBreak => Diagnostic::error("`break` outside loop"),
    };
    diag.pos(value.span.to_pos())
}

