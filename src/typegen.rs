
use std::{cell::{RefCell, RefMut}, array};

use crate::{basegen::{BaseProgram, Program, FunWithMetadata, InstrKind, InstrLiteral}, parser::{Span, TranslationUnit, Type}, arch::Intrinsic, diagnostic::Diagnostic, eval, common::{self, CommonState}};

pub(crate) fn typegen<I: Intrinsic>(base_program: TranslationUnit<BaseProgram<I>>) -> Result<TranslationUnit<Program<I>>, TypeError> {

    let mut part = TranslationUnit {
        inner: Program {
            funs: Default::default(), types: Default::default(),
        },
        arena: base_program.arena,
        main: base_program.main,
    };

    let mut memory = common::Memory::new();
    // allocate statics here

    // set up the stack
    let stack = memory.alloc(1024, 8).expect("allocate stack memory"); // todo: make the stack grow when it needs more space

    let state = State {
        common: common::CommonState {
            arena: &part.arena,
            memory,
            stack,
        },
        items: Vec::new(),
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

            InstrKind::Label { .. } => (),

            InstrKind::Push { value } => state.borrow_mut().push(Item::literal(value)),

            InstrKind::Call { to } => todo!(),
            InstrKind::Return => return Ok(()),

            InstrKind::Drop => state.borrow_mut().drop()?,
            InstrKind::Dup  => state.borrow_mut().dup()?,
            InstrKind::Swap => state.borrow_mut().swap()?,
            InstrKind::Over => state.borrow_mut().over()?,
            InstrKind::Rot3 => state.borrow_mut().rot3()?,
            InstrKind::Rot4 => state.borrow_mut().rot4()?,

            InstrKind::Read  => todo!(),
            InstrKind::Move  => todo!(),
            InstrKind::Write => todo!(),

            InstrKind::Addr => todo!(),
            // InstrKind::Type => todo!(),
            // InstrKind::Size => todo!(),

            // InstrKind::Access => todo!(),

            InstrKind::Arrow => todo!(),

            InstrKind::Add => state.borrow_mut().add()?,
            InstrKind::Sub => state.borrow_mut().sub()?,
            InstrKind::Mul => state.borrow_mut().mul()?,
            InstrKind::Dvm => state.borrow_mut().dvm()?,

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
    common: common::CommonState<'a>,
    pub items: Vec<Metadata>,
}

impl<'a> State<'a> {

    pub fn push(&mut self, item: Item<Option<Vec<u8>>>) {
        let size = 8; // todo: use actual size of t
        self.items.push(item.metadata);
        self.common.push(item.v.unwrap_or(vec![0; size]));
    }

    /// Peek at the top most stack item
    pub fn peek(&self, sub: usize) -> Option<&Metadata> {
        let len = self.items.len();
        self.items.get(len - sub - 1)
    }

    pub fn pop(&mut self) -> Option<Item<Option<Vec<u8>>>> {
        // if the item is not comptime it will be `None`
        let md = self.items.pop()?;
        let size = 8; /* todo: use the actual size of the type */
        let data = self.common.pop(size);
        match md.comptime {
            true  => Some(Item { metadata: md, v: Some(data) }),
            false => Some(Item { metadata: md, v: None })
        }
        
    }

    /// Discards `n` values from the top of the stack
    fn shrink_by(&mut self, count: usize) {
        let len = self.items.len();
        if len < count { panic!("shrink did not have enough elements") };
        self.items.truncate(len - count);
        self.common.shrink_by(2);
    }

    /// Drops the element ontop of the stack
    pub fn drop(&mut self) -> Result<(), TypeError> {
        self.pop().ok_or(TypeError::unspecified())?;
        Ok(())
    }

    /// Duplicates the element ontop of the stack
    pub fn dup(&mut self) -> Result<(), TypeError> {
        let item = self.items.last().ok_or(TypeError::unspecified())?;
        let size = 8; // todo: use item1 actual size of t
        self.items.push(item.clone());
        self.common.dup(size); // perform the actual dup
        Ok(())
    }

    /// Duplicates the element second last element and pushes the it ontop of the stack
    /// E.g (A B -> A B A)
    pub fn over(&mut self) -> Result<(), TypeError> {
        let len = self.items.len();
        let item1 = &self.items.get(len - 1).ok_or(TypeError::unspecified())?.t;
        let item2 = &self.items.get(len - 2).ok_or(TypeError::unspecified())?.t;
        let size1 = 8; // todo: use item1 actual size of t
        let size2 = 8; // todo: use item2 actual size of t
        let duplicate = self.items.get(len - 2).ok_or(TypeError::unspecified())?;
        self.items.push(duplicate.clone());
        self.common.over(size1, size2); // perform the actual over
        Ok(())
    }

    /// Swaps the two elements ontop of the stack
    pub fn swap(&mut self) -> Result<(), TypeError> {
        let len = self.items.len();
        let item1 = &self.items.get(len - 1).ok_or(TypeError::unspecified())?.t;
        let item2 = &self.items.get(len - 2).ok_or(TypeError::unspecified())?.t;
        let size1 = 8; // todo: use item1 actual size of t
        let size2 = 8; // todo: use item2 actual size of t
        self.items.swap(len - 1, len - 2);
        self.common.swap(size1, size2); // perform the actual swap
        Ok(())
    }

    /// Rotate the top 3 elements to the left once
    // A B C => B C A
    pub fn rot3(&mut self) -> Result<(), TypeError> {
        let len = self.items.len();
        let item1 = &self.items.get(len - 1).ok_or(TypeError::unspecified())?.t;
        let item2 = &self.items.get(len - 2).ok_or(TypeError::unspecified())?.t;
        let item3 = &self.items.get(len - 3).ok_or(TypeError::unspecified())?.t;
        let size1 = 8; // todo: use item1 actual size of t
        let size2 = 8; // todo: use item2 actual size of t
        let size3 = 8; // todo: use item2 actual size of t
        self.items[len - 3 .. len].rotate_left(1);
        self.common.rot3(size1, size2, size3); // perform the actual rotation (rot3)
        Ok(())
    }

    /// Rotate the top 4 elements to the left once
    // A B C D => B C D A
    pub fn rot4(&mut self) -> Result<(), TypeError> {
        let len = self.items.len();
        let item1 = &self.items.get(len - 1).ok_or(TypeError::unspecified())?.t;
        let item2 = &self.items.get(len - 2).ok_or(TypeError::unspecified())?.t;
        let item3 = &self.items.get(len - 3).ok_or(TypeError::unspecified())?.t;
        let item4 = &self.items.get(len - 4).ok_or(TypeError::unspecified())?.t;
        let size1 = 8; // todo: use item1 actual size of t
        let size2 = 8; // todo: use item2 actual size of t
        let size3 = 8; // todo: use item2 actual size of t
        let size4 = 8; // todo: use item2 actual size of t
        self.items[len - 4 .. len].rotate_left(1);
        self.common.rot4(size1, size2, size3, size4); // perform the actual rotation (rot4)
        Ok(())
    }

    /// A general wrapper for a builtin math operation of
    /// the signature (int int -> int)
    fn math_op_1(&mut self, op: impl FnOnce(usize, usize) -> usize) -> Result<(), TypeError> {
        let md1 = self.peek(0).ok_or(TypeError::unspecified())?;
        let md2 = self.peek(1).ok_or(TypeError::unspecified())?;
        let size = 8; // todo: use actual size of t, both values will be the same size
        if md1.t != Type::Int ||
           md2.t != Type::Int {
            return Err(TypeError::expect_types(vec![Type::Int], vec![md1.clone(), md2.clone()]))
        }
        // comptime or runtime
        if md1.comptime && md2.comptime {
            self.common.math_op_1(op);
        } else {
            self.shrink_by(2);
            self.push(Item::runtime(Type::Int));
        }
        Ok(())
    }

    /// A general wrapper for a builtin math operation of
    /// the signature (int int -> int int)
    fn math_op_2(&mut self, op: impl FnOnce(usize, usize) -> (usize, usize)) -> Result<(), TypeError> {
        let md1 = self.peek(0).ok_or(TypeError::unspecified())?;
        let md2 = self.peek(1).ok_or(TypeError::unspecified())?;
        let size = 8; // todo: use actual size of t, both values will be the same size
        if md1.t != Type::Int ||
           md2.t != Type::Int {
            return Err(TypeError::expect_types(vec![Type::Int], vec![md1.clone(), md2.clone()]))
        }
        // comptime or runtime
        if md1.comptime && md2.comptime {
            self.common.math_op_2(op);
        } else {
            self.shrink_by(2);
            self.push(Item::runtime(Type::Int));
            self.push(Item::runtime(Type::Int));
        }
        Ok(())
    }

    pub fn add(&mut self) -> Result<(), TypeError> {
        self.math_op_1(|lhs, rhs| lhs + rhs)
    }

    pub fn sub(&mut self) -> Result<(), TypeError> {
        self.math_op_1(|lhs, rhs| lhs - rhs)
    }

    pub fn mul(&mut self) -> Result<(), TypeError> {
        self.math_op_1(|lhs, rhs| lhs * rhs)
    }

    pub fn dvm(&mut self) -> Result<(), TypeError> {
        self.math_op_2(|lhs, rhs| (lhs / rhs, lhs % rhs))
    }

}

#[derive(Clone)]
pub(crate) struct Item<T> {
    pub metadata: Metadata,
    v: T, // use tinyvec / stackvec
}

impl Item<Option<Vec<u8>>> {
    pub fn literal(literal: InstrLiteral) -> Self {
        match literal {
            InstrLiteral::Int(val) => Self::comptime(Type::Int, usize::to_rh(val)),
            InstrLiteral::Bool(_val) => todo!("bool literal"),
            _other => todo!("implement str literals as implicit statics as ptr of char")
        }
    }
}

impl<T> Item<Option<T>> {
    pub const fn comptime(t: Type, v: T) -> Self {
        Self { metadata: Metadata { comptime: true, t }, v: Some(v) }
    }
    pub const fn runtime(t: Type) -> Self {
        Self { metadata: Metadata { comptime: false, t }, v: None }
    }
}

#[derive(Clone)]
pub(crate) struct Metadata {
    pub comptime: bool,
    pub t: Type,
}

pub(crate) trait FromRh {
    const TYPE: Type;
    fn from_rh(rh: Vec<u8>) -> Self;
}

impl FromRh for usize { // todo: use RhInt
    const TYPE: Type = Type::Int;
    fn from_rh(rh: Vec<u8>) -> Self {
        usize::from_ne_bytes(rh.try_into().expect("convert to rust usize"))
    }
}

pub(crate) trait ToRh {
    const TYPE: Type;
    fn to_rh(rh: Self) -> Vec<u8>;
}

impl ToRh for usize { // todo: use RhInt
    const TYPE: Type = Type::Int;
    fn to_rh(rh: Self) -> Vec<u8> {
        Vec::from(rh.to_ne_bytes())
    }
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
    pub(crate) fn unspecified() -> Self {
        Self::unspanned(TypeErrorKind::Unspecified)
    }
    pub(crate) fn expect_types(want: Vec<Type>, got: Vec<Metadata>) -> Self {
        Self::unspanned(TypeErrorKind::Expect {
            want: want.into_iter().map(|t| Metadata { comptime: false, t }).collect(),
            got
        })
    }
    pub(crate) fn expect_metadata(want: Vec<Metadata>, got: Vec<Metadata>) -> Self {
        Self::unspanned(TypeErrorKind::Expect { want, got })
    }
}

pub(crate) enum TypeErrorKind {
    Unspecified,
    UnknownFn { name: String },
    BranchesNotEmpty,
    BranchesNotEqual,
    Expect { want: Vec<Metadata>, got: Vec<Metadata> },
}

pub(crate) fn format_error(value: TypeError) -> Diagnostic {
    let diag = match value.kind {
        TypeErrorKind::Unspecified => unreachable!(), // should be replaced by a more specific error
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
        TypeErrorKind::Expect { want, got } => {
            Diagnostic::error("type mismatch")
                .note(format!("want {}", format_metadata(want)))
                .note(format!("got  {}", format_metadata(got)))
        },
    };
    diag
}

fn format_metadata(mds: Vec<Metadata>) -> String {
    format!("{:?}", mds.into_iter().map(|it| {
        if it.comptime { format!("comptime {:?}", it.t) }
        else { format!("{:?}", it.t) }
    }).collect::<Vec<_>>())
}

