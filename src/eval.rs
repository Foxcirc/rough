
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

fn eval_fun<I: Intrinsic>(state: &mut State, fun: &FunWithMetadata<I>) -> Result<(), EvalError> {
    
    for instr in fun.body.iter() {

        match &instr.kind {

            InstrKind::Label { label, producer } => todo!(),

            InstrKind::Push { value: InstrLiteral::Int(number) } => state.push_int(*number),
            InstrKind::Push { .. } => todo!(),

            InstrKind::Call { to } => todo!(),
            InstrKind::Return => return Ok(()),

            InstrKind::Drop => drop(state.pop_int()),
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

            InstrKind::Add => {
                let lhs = state.pop_int();
                let rhs = state.pop_int();
                let t = state.pop(8);
                println!("{lhs} + {rhs} = {}", lhs + rhs);
            },
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
    common: common::CommonState<'a>,
}

impl<'a> State<'a> {
    pub fn push(&mut self, value: Vec<u8>) {
        self.common.push(value)
    }
    pub fn pop(&mut self, size: usize) -> Vec<u8> {
        self.common.pop(size)
    }
    pub fn push_int(&mut self, value: usize) {
        self.common.push_int(value)
    }
    pub fn pop_int(&mut self) -> usize {
        self.common.pop_int()
    }
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

