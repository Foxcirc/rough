
use crate::{
    basegen::{Program, FunWithMetadata, InstrKind, InstrLiteral},
    diagnostic::Diagnostic,
    parser::{Span, TranslationUnit},
    arch::Intrinsic, common,
};

pub(crate) fn eval<I: Intrinsic>(input: TranslationUnit<Program<I>>) -> Result<(), EvalError> {

    let program = input.inner;

    let mut memory = common::Memory::new();

    // set up the stack
    let stack = memory.alloc(1024, 8).expect("allocate stack memory"); // todo: calculate needed size during typegen

    // todo: push statics (str literals) onto the stack and set them up
    // todo: do all of this alrdy in the typegen?? setup stack alrdy in typegen??

    let mut state = State {
        common: common::CommonState {
            arena: &input.arena,
            memory,
            stack,
        }
    };

    let fun = match input.main {
        Some(val) => program.funs.get(&val).expect("get entry point"),
        None => return Err(EvalError::unspanned(EvalErrorKind::NoEntryPoint)),
    };

    eval_fun(&mut state, fun)?;

   Ok(())

}

pub(crate) fn literal_to_bytes(literal: &InstrLiteral) -> Vec<u8> {
    match literal {
        InstrLiteral::Int(val) => Vec::from(val.to_ne_bytes()),
        InstrLiteral::Bool(_val) => todo!("bool literal in eval"),
        InstrLiteral::Str(..) => unreachable!(),
    }
}

fn eval_fun<I: Intrinsic>(state: &mut State, fun: &FunWithMetadata<I>) -> Result<(), EvalError> {
    
    for instr in fun.body.iter() {

        match &instr.kind {

            InstrKind::Label { label, producer } => todo!(),

            InstrKind::Push { value } => state.common.push(literal_to_bytes(value)),

            InstrKind::Call { to } => todo!(),
            InstrKind::Return => return Ok(()),

            InstrKind::Drop => drop(state.common.pop(8)),
            InstrKind::Dup  => todo!(),
            InstrKind::Over => todo!(),
            InstrKind::Swap => todo!(),
            InstrKind::Rot3 => todo!(),
            InstrKind::Rot4 => todo!(),

            InstrKind::Read  => todo!(),
            InstrKind::Move  => todo!(),
            InstrKind::Write => todo!(),

            InstrKind::Addr => todo!(),
            // InstrKind::Type => todo!(),
            // InstrKind::Size => todo!(),

            // InstrKind::Access => todo!(),

            InstrKind::Arrow => todo!(),

            InstrKind::Add => state.common.math_op_1(|lhs, rhs| lhs + rhs),
            InstrKind::Sub => todo!(),
            InstrKind::Mul => todo!(),
            InstrKind::Dvm => todo!(),

            InstrKind::Not => todo!(),
            InstrKind::And => todo!(),
            InstrKind::Or  => todo!(),
            InstrKind::Xor => todo!(),

            InstrKind::Eq  => todo!(),
            InstrKind::Gt  => todo!(),
            InstrKind::Gte => todo!(),
            InstrKind::Lt  => todo!(),
            InstrKind::Lte => todo!(),

            InstrKind::Bne { to } => todo!(),
            InstrKind::Bra { to } => todo!(),

            InstrKind::Intrinsic(intrinsic) => todo!(),

        }

    }

    unreachable!()

}

struct State<'a> {
    pub common: common::CommonState<'a>,
}

impl<'a> State<'a> {

    pub fn push(&mut self, data: Vec<u8>) { self.common.push(data) }
    pub fn pop(&mut self, size: usize) -> Vec<u8> { self.common.pop(size) }
    pub fn shrink(&mut self, size: usize) { self.common.shrink_by(size) }
    pub fn dup(&mut self, size: usize) { self.common.dup(size) }
    pub fn over(&mut self, size1: usize, size2: usize) { self.common.over(size1, size2) }
    pub fn swap(&mut self, size1: usize, size2: usize) { self.common.swap(size1, size2) }
    pub fn rot3(&mut self, size1: usize, size2: usize, size3: usize) { self.common.rot3(size1, size2, size3) }
    pub fn rot4(&mut self, size1: usize, size2: usize, size3: usize, size4: usize) { self.common.rot4(size1, size2, size3, size4) }
    pub fn add(&mut self) { self.common.math_op_1(|lhs, rhs| lhs + rhs) }
    pub fn sub(&mut self) { self.common.math_op_1(|lhs, rhs| lhs - rhs) }
    pub fn mul(&mut self) { self.common.math_op_1(|lhs, rhs| lhs * rhs) }
    pub fn dvm(&mut self) { self.common.math_op_2(|lhs, rhs| (lhs / rhs, lhs % rhs)) }

}

// todo: impl debug and Error for all errors, so they can be used as actuall error types (eg .unwrap works only for Debug types)
pub(crate) struct EvalError {
    kind: EvalErrorKind,
    span: Span,
}

impl EvalError {
    pub(crate) fn spanned(kind: EvalErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
    pub(crate) fn unspanned(kind: EvalErrorKind) -> Self {
        Self { kind, span: Span::default() }
    }
}

pub(crate) enum EvalErrorKind {
    NoEntryPoint,
}

pub(crate) fn format_error(value: EvalError) -> Diagnostic {
    let diag = Diagnostic::error("eval-error");
    diag
}

