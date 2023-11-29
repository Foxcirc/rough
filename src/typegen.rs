
use std::{cell::{RefCell, RefMut}, array};

use crate::{basegen::{BaseProgram, Program, FunWithMetadata, InstrKind, InstrLiteral}, parser::{Span, TranslationUnit, Type}, arch::Intrinsic, diagnostic::Diagnostic, eval, common};

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
            InstrKind::Over => state.borrow_mut().over()?,
            InstrKind::Swap => state.borrow_mut().swap()?,
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

            InstrKind::Add => order(state.borrow_mut(), |values| calc(values, |[lhs, rhs]: [usize; 2]| [lhs + rhs]))?,
            InstrKind::Sub => order(state.borrow_mut(), |values| calc(values, |[lhs, rhs]: [usize; 2]| [lhs - rhs]))?,
            InstrKind::Mul => order(state.borrow_mut(), |values| calc(values, |[lhs, rhs]: [usize; 2]| [lhs * rhs]))?,
            InstrKind::Dvm => order(state.borrow_mut(), |values| calc(values, |[lhs, rhs]: [usize; 2]| [lhs / rhs, lhs % rhs]))?,

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
    pub fn peek(&self) -> Option<&Metadata> {
        self.items.last()
    }

    pub fn pop(&mut self) -> Option<Item<Option<Vec<u8>>>> {
        // if the item is not comptime it will be `None`
        let metadata = self.items.pop()?;
        let data = self.common.pop(/* todo: use the actual size of the type */ 8);
        match metadata.comptime {
            true  => Some(Item { metadata, v: Some(data) }),
            false => Some(Item { metadata, v: None })
        }
        
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

}

#[derive(Clone)]
pub(crate) struct Item<T> {
    pub metadata: Metadata,
    v: T, // use tinyvec / stackvec
}

impl Item<Option<Vec<u8>>> {
    pub const fn literal(literal: InstrLiteral) -> Self {
        match literal {
            InstrLiteral::Int(val) => Self::comptime(Type::Int, usize::to_rh(val)),
            InstrLiteral::Bool(val) => todo!("bool literal"),
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
    /// Maps the items data if it is `Some`
    pub fn map<U>(self, func: impl FnOnce(T) -> U) -> Item<Option<U>> {
        Item {
            metadata: self.metadata,
            v: if let Some(data) = self.v {
                Some(func(data))
            } else {
                None
            }
        }
    }
}

pub(crate) type RawItem = Item<Option<Vec<u8>>>;

#[derive(Clone)]
pub(crate) struct Metadata {
    comptime: bool,
    t: Type,
}

pub(crate) fn check(item: Option<RawItem>, want: Type) -> Result<RawItem, TypeError> {
    if let Some(item) = item {
        if item.metadata.t != want {
            Err(TypeError::unspanned(TypeErrorKind::Expect { want, got: Some(item.metadata.t) }))
        } else {
            Ok(item)
        }
    } else {
        Err(TypeError::unspanned(TypeErrorKind::Expect { want, got: None }))
    }
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

pub(crate) fn order<const IN: usize, const ON: usize, F>(mut borrow: RefMut<State>, func: F) -> Result<(), TypeError>
    where F: FnOnce([RawItem; IN]) -> Result<[RawItem; ON], TypeError>
{

    const IT: Option<RawItem> = None;
    let mut items = [IT; IN];
    for idx in (0..IN).rev() {
        let checked = borrow.pop().ok_or(TypeError::unspanned(TypeErrorKind::ExpectAnything))?;
        items[idx] = Some(checked);
    };

    let inputs = items.map(|item| item.unwrap());
    let outputs = func(inputs)?;
    for item in outputs { borrow.push(item) }

    Ok(())
}

pub(crate) fn calc<const IN: usize, const ON: usize, F, I, O>(values: [RawItem; IN], func: F) -> Result<[RawItem; ON], TypeError>
    where F: FnOnce([I; IN]) -> [O; ON], I: FromRh + Default + Copy, O: ToRh
{

    let comptime = values.iter().all(|item| item.v.is_some());
    if comptime {
        let inputs = values.map(|item| I::from_rh(item.v.unwrap()));
        let result = func(inputs);
        Ok(result.map(|val| Item::comptime(O::TYPE, O::to_rh(val))))
    } else {
        Ok(array::from_fn(|_| Item::runtime(O::TYPE)))
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
}

pub(crate) enum TypeErrorKind {
    Unspecified,
    UnknownFn { name: String },
    BranchesNotEmpty,
    BranchesNotEqual,
    Expect { want: Vec<Type>, got: Vec<Type> },
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
                .note(format!("want {:?}", want))
                .note(format!("got  {:?}", got))
        },
    };
    diag
}

