

use std::{fmt, collections::HashMap};
use crate::{parser::{Literal, OpKind, Op, Span, IdentStr}, diagnostic::Diagnostic, parse_modules, arch::Intrinsic};

pub(crate) fn codegen<I: Intrinsic>(state: &mut Program<I>, source_file: parse_modules::SourceFile) -> Result<(), CodegenError> {

    // let file_name = source_file.name().to_string();

    for fun in source_file.items.funs {

        let mut bytecode = Bytecode::new();
        let mut counter = 0;

        codegen_block(&mut bytecode, &mut counter, fun.body, None)?;

        bytecode.push(Instr::spanned(InstrKind::Return, fun.span));

        state.funs.insert(fun.name, bytecode);
    
    }

    Ok(())

}

fn codegen_block<I: Intrinsic>(bytecode: &mut Bytecode<I>, counter: &mut usize, block: Vec<Op>, loop_escape: Option<Label>) -> Result<(), CodegenError> {

    for op in block {

        match op.kind {

            OpKind::Push { value } => bytecode.push(Instr::spanned(InstrKind::Push { value }, op.span)),

            OpKind::Call { name }  => {
                let inrinsic = I::generate(&name);
                let result = match inrinsic {
                    Some(val) => InstrKind::Intrinsic(val),
                    None => InstrKind::Call { to: name }
                };
                bytecode.push(Instr::spanned(result, op.span));
            },

            OpKind::Copy  => bytecode.push(Instr::spanned(InstrKind::Copy, op.span)),
            OpKind::Over  => bytecode.push(Instr::spanned(InstrKind::Over, op.span)),
            OpKind::Swap  => bytecode.push(Instr::spanned(InstrKind::Swap, op.span)),
            OpKind::Rot3  => bytecode.push(Instr::spanned(InstrKind::Rot3, op.span)),
            OpKind::Rot4  => bytecode.push(Instr::spanned(InstrKind::Rot4, op.span)),
            OpKind::Drop  => bytecode.push(Instr::spanned(InstrKind::Drop, op.span)),

            OpKind::Read  => bytecode.push(Instr::spanned(InstrKind::Read,  op.span)),
            OpKind::Write => bytecode.push(Instr::spanned(InstrKind::Write, op.span)),

            OpKind::Move  => todo!(),
            OpKind::Addr  => todo!(),
            OpKind::Type  => todo!(),
            OpKind::Size  => todo!(),
            OpKind::Dot   => todo!(),

            OpKind::Add => bytecode.push(Instr::spanned(InstrKind::Add, op.span)),
            OpKind::Sub => bytecode.push(Instr::spanned(InstrKind::Sub, op.span)),
            OpKind::Mul => bytecode.push(Instr::spanned(InstrKind::Mul, op.span)),
            OpKind::Dvm => bytecode.push(Instr::spanned(InstrKind::Dvm, op.span)),

            OpKind::Not => bytecode.push(Instr::spanned(InstrKind::Not, op.span)),
            OpKind::And => bytecode.push(Instr::spanned(InstrKind::And, op.span)),
            OpKind::Or  => bytecode.push(Instr::spanned(InstrKind::Or,  op.span)),
            OpKind::Xor => bytecode.push(Instr::spanned(InstrKind::Xor, op.span)),

            OpKind::Eq  => bytecode.push(Instr::spanned(InstrKind::Eq,  op.span)),
            OpKind::Gt  => bytecode.push(Instr::spanned(InstrKind::Gt,  op.span)),
            OpKind::Gte => bytecode.push(Instr::spanned(InstrKind::Gte, op.span)),
            OpKind::Lt  => bytecode.push(Instr::spanned(InstrKind::Lt,  op.span)),
            OpKind::Lte => bytecode.push(Instr::spanned(InstrKind::Lte, op.span)),

            OpKind::If { block: if_block } => {

                let end = next_label(counter);
                bytecode.push(Instr::spanned(InstrKind::Bne { to: end }, op.span));
                codegen_block(bytecode, counter, if_block, loop_escape)?;
                bytecode.push(Instr::spanned(InstrKind::Label { label: end, producer: Producer::If }, op.span));

            },
            OpKind::Elif { block: _block } => todo!(),
            OpKind::Else { block: else_block } => {

                // check that this `else` is coming directly after an `if`
                if !matches!(bytecode.last().map(|val| &val.kind), Some(InstrKind::Label { producer: Producer::If, .. })) {
                    return Err(CodegenError::spanned(CodegenErrorKind::InvalidElse, op.span))
                }

                let end = next_label(counter);
                bytecode.insert(bytecode.len() - 1, Instr::spanned(InstrKind::Bra { to: end }, op.span));
                codegen_block(bytecode, counter, else_block, loop_escape)?;
                bytecode.push(Instr::spanned(InstrKind::Label { label: end, producer: Producer::Else }, op.span));

            },
            OpKind::Loop { block: loop_block } => {

                let start = next_label(counter);
                let escape = next_label(counter);
                bytecode.push(Instr::spanned(InstrKind::Label { label: start, producer: Producer::Loop }, op.span));
                codegen_block(bytecode, counter, loop_block, Some(escape))?;
                bytecode.push(Instr::spanned(InstrKind::Bra { to: start }, op.span));
                bytecode.push(Instr::spanned(InstrKind::Label { label: escape, producer: Producer::Break }, op.span));

            },
            OpKind::For  { block: _block } => todo!(),
            OpKind::Break => {

                let escape = match loop_escape {
                    Some(val) => val,
                    None => return Err(CodegenError::spanned(CodegenErrorKind::InvalidBreak, op.span)),
                };
                bytecode.push(Instr::spanned(InstrKind::Bra { to: escape }, op.span))

            },
        };

    };

    Ok(())

}

fn next_label(counter: &mut usize) -> usize {
    *counter += 1;
    *counter
}

#[derive(PartialEq, Eq)]
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
    pub(crate) fn unspanned(kind: InstrKind<I>) -> Self {
        Self { kind, span: Span::default() }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum InstrKind<I> {

    Label { label: Label, producer: Producer }, // todo: rename?

    Push { value: Literal },

    Call { to: IdentStr },
    Return,

    Drop,
    Copy,
    Over,
    Swap,
    Rot3,
    Rot4,

    Read,
    Write,
    // Move,

    // Addr,
    // Type,
    // Size,

    // Access,

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

    Bne { to: Label }, // todo: make it all tuple variants
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

struct State<I> {
    pub bytecode: Vec<Instr<I>>,
    pub counter: usize,
}

impl<I> Default for State<I> {
    fn default() -> Self {
        Self {
            bytecode: Vec::new(),
            counter: 0
        }
    }
}

impl<I> State<I> {
    pub fn next_label(&mut self) -> usize {
        self.counter += 1;
        self.counter
    }
}

pub(crate) struct Program<I> {
    pub funs: HashMap<IdentStr, Bytecode<I>>,
}

impl<I> Default for Program<I> {
    fn default() -> Self {
        Self {
            funs: HashMap::new(),
        }
    }
}

pub(crate) type Bytecode<I> = Vec<Instr<I>>;

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
    InvalidSignature,
}

pub(crate) fn format_error(value: CodegenError) -> Diagnostic {
    let diag = match &value.kind {
        CodegenErrorKind::InvalidElse => Diagnostic::error("`else` block withput `if` block"),
        CodegenErrorKind::InvalidBreak => Diagnostic::error("`break` outside loop"),
        CodegenErrorKind::InvalidSignature => Diagnostic::error("invalid type in fn signature"),
    };
    diag.pos(value.span.to_pos())
}

