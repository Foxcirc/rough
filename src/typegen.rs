
use std::cell::{RefCell, RefMut};

use crate::{basegen::{BaseProgram, Program, FunWithMetadata, InstrKind, InstrLiteral}, parser::{Span, TranslationUnit, Type}, arch::Intrinsic, diagnostic::Diagnostic, arena, eval::{self, MemoryPtr, CommonState}};

pub(crate) fn typegen<I: Intrinsic>(base_program: TranslationUnit<BaseProgram<I>>) -> Result<TranslationUnit<Program<I>>, TypeError> {

    let mut part = TranslationUnit {
        inner: Program {
            funs: Default::default(), types: Default::default(),
        },
        arena: base_program.arena,
        main: base_program.main,
    };

    let mut memory = eval::Memory::new();
    // allocate statics here

    // set up the stack
    let stack = memory.alloc(1024, 8).expect("allocate stack memory"); // todo: make the stack grow when it needs more space

    let state = State {
        common: CommonState {
            arena: &part.arena,
            memory,
            stack,
        },
        types: Vec::new(),
    };

    let cell = RefCell::new(state);

    for (_fun_name, fun) in base_program.inner.funs.clone() {

        typecheck_fun(&cell, fun)?;

    }

    let mut state = cell.into_inner();
    state.common.memory.dealloc(state.common.stack).expect("deallocate stack memory");
    assert!(state.common.memory.allocations() == 0, "some memory was leaked");

    part.inner.funs = base_program.inner.funs;
    part.inner.types = base_program.inner.types;

    Ok(part)

}

fn typecheck_fun<I: Intrinsic>(state: &RefCell<State>, fun: FunWithMetadata<I>) -> Result<(), TypeError> {

    // todo: push signature onto the stack
    
    for instr in fun.body {

        match instr.kind {

            InstrKind::Label { label, producer } => todo!(),

            InstrKind::Push { value: InstrLiteral::Int(number) } => {
                state.borrow_mut().push_int(number)
            },
            InstrKind::Push { .. } => todo!(),

            InstrKind::Call { to } => todo!(),
            InstrKind::Return => return Ok(()),

            InstrKind::Drop => todo!(),
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

            InstrKind::Add => todo!(),
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

    unreachable!();

}

// very similar to the state in eval
struct State<'a> {
    common: CommonState<'a>,
    pub types: Vec<Item>,
}

impl<'a> State<'a> {

    pub fn push_int(&mut self, value: usize) {
        self.types.push(Item { comptime: true, t: Type::Int });
        self.common.push_int(value)
    }

    pub fn pop_int(&mut self) -> usize {
        assert!(matches!(self.types.pop(), Some(Item { t: Type::Int, .. })));
        self.common.pop_int()
    }

}

pub(crate) struct Item {
    comptime: bool,
    t: Type,
}

pub(crate) struct TypeError {
    kind: TypeErrorKind,
    span: Span,
}

impl TypeError {
    pub(crate) fn spanned(kind: TypeErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}

pub(crate) enum TypeErrorKind {
    UnknownFn { name: String },
    BranchesNotEmpty,
    BranchesNotEqual,
    Mismatch { want: Vec<()>, got: Vec<()> },
    Error,
}

pub(crate) fn format_error(value: TypeError) -> Diagnostic {
    let diag = match value.kind {
        TypeErrorKind::UnknownFn { name } => Diagnostic::error("unknown word").code(name),
        TypeErrorKind::BranchesNotEmpty => {
            Diagnostic::error("branch changes stack")
                .note("this branch may not change the types on the stack")
                .note("consider adding an `else` block")
        },
        TypeErrorKind::BranchesNotEqual => {
            Diagnostic::error("branches not equal")
                .note("both branches have to evaluate to the same types")
        },
        TypeErrorKind::Mismatch { want, got } => {
            Diagnostic::error("type mismatch")
                .note(format!("want {:?}", want.into_iter().map(|val| val).collect::<Vec<_>>()))
                .note(format!("got {:?}", got .into_iter().map(|val| val).collect::<Vec<_>>()))
        },
        TypeErrorKind::Error => Diagnostic::error("TODO: IMPLEMENT TYPE ERROR")
    };
    diag
}

