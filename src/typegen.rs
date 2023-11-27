
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

            InstrKind::Push { value: InstrLiteral::Int(number) } => state.borrow_mut().push_int(number),
            InstrKind::Push { .. } => todo!(),

            InstrKind::Call { to } => todo!(),
            InstrKind::Return => return Ok(()),

            InstrKind::Drop => drop(state.borrow_mut().pop_int()),
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
                let mut borrow = state.borrow_mut();
                let lhs = check(borrow.pop(), Type::Int)?;
                let rhs = check(borrow.pop(), Type::Int)?;
                let result = match comptime([lhs, rhs]) {
                    Some(value) => run(value, Calc::add),
                    None => borrow.push(Item::runtime(Type::Int))
                };
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

            InstrKind::Intrinsic(intrinsic) => {
                RefMut::map(state.borrow_mut(), |state| {
                    I::typegen(&intrinsic, state); // this can basically do anything
                    state
                });
            }

        }

    }

    unreachable!();

}

// very similar to the state in eval
pub(crate) struct State<'a> {
    common: CommonState<'a>,
    pub types: Vec<Metadata>,
}

impl<'a> State<'a> {

    pub fn push(&mut self, item: Item<Option<Vec<u8>>>) {
        let size = 8; // todo: use actual size of t
        self.types.push(item.metadata);
        self.common.push(item.data.unwrap_or(vec![0; size]));
    }

    pub fn push_int(&mut self, value: usize) {
        self.push(Item {
            metadata: Metadata { comptime: true, t: Type::Int },
            data: Some(Vec::from(value.to_ne_bytes()))
        })
    }

    /// Peek at the top most stack item
    pub fn peek(&self) -> Option<&Metadata> {
        self.types.last()
    }

    pub fn pop(&mut self) -> Option<Item<Option<Vec<u8>>>> {
        // if the item is not comptime it will be `None`
        let metadata = self.types.pop()?;
        let data = self.common.pop(/* todo: use the actual size of the type */ 8);
        match metadata.comptime {
            true => Some(Item { metadata, data: Some(data) }),
            false => Some(Item { metadata, data: None })
        }
        
    }

    pub fn pop_int(&mut self) -> Item<Option<usize>> {
        // if the item is not comptime it will be `None`
        let item = self.pop().expect("pop item");
        assert!(item.metadata.t == Type::Int, "type must be integer");
        item.map(|data| usize::from_ne_bytes(data[..].try_into().unwrap()))
    }

}

pub(crate) struct Item<T> {
    pub metadata: Metadata,
    data: T, // use tinyvec / stackvec
}

impl<T> Item<Option<T>> {
    pub fn runtime(t: Type) -> Self {
        Self { metadata: Metadata { comptime: false, t }, data: None }
    }
    /// Maps the items data if it is `Some`
    pub fn map<U>(self, func: impl FnOnce(T) -> U) -> Item<Option<U>> {
        Item {
            metadata: self.metadata,
            data: if let Some(data) = self.data {
                Some(func(data))
            } else {
                None
            }
        }
    }
}

pub(crate) struct Metadata {
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
    pub(crate) fn unspanned(kind: TypeErrorKind) -> Self {
        Self { kind, span: Span::default() }
    }
}

pub(crate) enum TypeErrorKind {
    UnknownFn { name: String },
    BranchesNotEmpty,
    BranchesNotEqual,
    Mismatch { want: Type, got: Option<Type> },
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
            let diag = Diagnostic::error("type mismatch")
                .note(format!("want {:?}", want));
                match got {
                    Some(t) => diag.note(format!("got {:?}", t)),
                    None    => diag.note(format!("got <empty>"))
                }
        },
    };
    diag
}

