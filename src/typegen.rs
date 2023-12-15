
use std::{cell::{RefCell, RefMut}, collections::HashMap};
use crate::{basegen::{BaseProgram, Program, FunWithMetadata, InstrKind, InstrLiteral}, parser::{Span, TranslationUnit, Identifier}, arch::Intrinsic, diagnostic::Diagnostic, eval, common::{self, CommonState}, intern};

pub(crate) fn typegen<I: Intrinsic>(base_program: TranslationUnit<BaseProgram<I>>) -> Result<TranslationUnit<Program<I>>, TypeError> {

    let mut part = TranslationUnit {
        inner: Program {
            funs: Default::default(), newtypes: Default::default(),
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
        typemap: TypeMap::new(),
    };

    let cell = RefCell::new(state);

    for (_fun_name, fun) in base_program.inner.funs.clone() {

        typecheck_fun(&cell, fun)?;

    }

    let mut state = cell.into_inner();
    state.common.memory.dealloc(state.common.stack).expect("deallocate stack memory");
    assert!(state.common.memory.allocations() == 0, "some memory was leaked"); // todo: also assert this in eval

    // todo: remove the below codeeeeee
    part.inner.funs = base_program.inner.funs;
    part.inner.newtypes = base_program.inner.newtypes;

    Ok(part)

}

fn typecheck_fun<I: Intrinsic>(state: &RefCell<State<I>>, fun: FunWithMetadata<I>) -> Result<(), TypeError> {

    // todo: push signature onto the stack
    
    for instr in fun.body {

        match instr.kind {

            InstrKind::Label { .. } => (),

            InstrKind::Push { value: InstrLiteral::Int(num) } => state.borrow_mut().push(Item::comptime(BuiltinType::Int.into(), usize_to_bytes(num))),
            InstrKind::Push { value: InstrLiteral::Bool(val) } => state.borrow_mut().push(Item::comptime(BuiltinType::Bool.into(), bool_to_bytes(val))),
            InstrKind::Push { .. } => todo!(),

            InstrKind::Call { to } => todo!(),
            InstrKind::Return => return Ok(()),

            InstrKind::Drop => state.borrow_mut().drop()?,
            InstrKind::Dup  => state.borrow_mut().dup()?,
            InstrKind::Swap => state.borrow_mut().swap()?,
            InstrKind::Over => state.borrow_mut().over()?,
            InstrKind::Rot3 => state.borrow_mut().rot3()?,
            InstrKind::Rot4 => state.borrow_mut().rot4()?,

            InstrKind::Read  => {
                // debug types
                println!("{:?}", state.borrow_mut().items.iter().map(|it| &it.typeid).collect::<Vec<_>>())
            },
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

pub(crate) type TypeId = u64;

pub(crate) struct TypeData<I> {
    pub funs: HashMap<Identifier, FunWithMetadata<I>>,
}

pub(crate) struct TypeMap<I> {
    typedata: HashMap<Identifier, (TypeId, TypeData<I>)>, // user defined types
    next: u64,
}

impl<I> TypeMap<I> {
    pub fn new() -> Self {
        Self {
            typedata: HashMap::new(),
            next: BuiltinType::next(),
        }
    }
    pub fn get(&self, name: Identifier) -> Option<&(TypeId, TypeData<I>)> {
        self.typedata.get(&name)
    }
    pub fn insert(&mut self, name: Identifier, value: TypeData<I>) {
        self.next += 1;
        let id = self.next;
        self.typedata.insert(name, (id, value));
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum BuiltinType {
    Int,
    Bool
}

impl BuiltinType {
    pub fn next() -> TypeId {
        3
    }
    pub fn size(&self) -> usize {
        let size: u32 = match self {
            Self::Int => 8,
            Self::Bool => 1,
        };
        size as usize
    }
}

impl From<BuiltinType> for TypeId {
    fn from(value: BuiltinType) -> Self {
        match value {
            BuiltinType::Int => 1,
            BuiltinType::Bool => 2,
        }
    }
}

impl From<TypeId> for BuiltinType {
    fn from(value: TypeId) -> Self {
        match value {
            1 => BuiltinType::Int,
            2 => BuiltinType::Bool,
            _ => unreachable!()
        }
    }
}

/// comptime data for a value
#[derive(Clone)]
pub(crate) struct ComptimeInfo {
    typeid: TypeId,
    comptime: bool,
    data: Vec<u8>,
}

impl ComptimeInfo {
    pub const fn minimal(typeid: TypeId, comptime: bool) -> Self {
        Self { typeid, comptime, data: Vec::new() }
    }
}

// very similar to the state in eval
pub(crate) struct State<'a, I> {
    common: common::CommonState<'a>,
    pub items: Vec<ComptimeInfo>,
    typemap: TypeMap<I>,
}

impl<'a, I> State<'a, I> {

    pub fn push(&mut self, item: Item) {
        let data = if item.data.is_empty() {
            let size = BuiltinType::from(item.info.typeid).size();
            let mut data = Vec::with_capacity(size);
            data.resize(size, 0);
            data
        } else {
            item.data
        };
        self.common.push(data);
        self.items.push(item.info);
    }

    /// Peek at the top most stack item
    pub fn peek(&self, sub: usize) -> Option<&ComptimeInfo> {
        let len = self.items.len();
        self.items.get(len - sub - 1)
    }

    pub fn pop(&mut self) -> Option<Item> {
        // if the item is not comptime it will be `None`
        let info = self.items.pop()?;
        let size = 8; /* todo: use the actual size of the type */
        let data = self.common.pop(size); // todo: maybe for non-comptime types we dont need to pop any data here??
        if info.comptime {
            Some(Item { info, data })
        } else {
            Some(Item { info, data: Vec::new() })
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
        let size = BuiltinType::from(item.typeid).size();
        self.items.push(item.clone());
        self.common.dup(size); // perform the actual dup
        Ok(())
    }

    /// Duplicates the element second last element and pushes the it ontop of the stack
    /// E.g (A B -> A B A)
    pub fn over(&mut self) -> Result<(), TypeError> {
        let len = self.items.len();
        let item1 = &self.items.get(len - 1).ok_or(TypeError::unspecified())?;
        let item2 = &self.items.get(len - 2).ok_or(TypeError::unspecified())?;
        let size1 = BuiltinType::from(item1.typeid).size();
        let size2 = BuiltinType::from(item2.typeid).size();
        let duplicate = self.items.get(len - 2).ok_or(TypeError::unspecified())?;
        self.items.push(duplicate.clone());
        self.common.over(size1, size2); // perform the actual over
        Ok(())
    }

    /// Swaps the two elements ontop of the stack
    pub fn swap(&mut self) -> Result<(), TypeError> {
        let len = self.items.len();
        let item1 = &self.items.get(len - 1).ok_or(TypeError::unspecified())?;
        let item2 = &self.items.get(len - 2).ok_or(TypeError::unspecified())?;
        let size1 = BuiltinType::from(item1.typeid).size();
        let size2 = BuiltinType::from(item2.typeid).size();
        self.items.swap(len - 1, len - 2);
        self.common.swap(size1, size2); // perform the actual swap
        Ok(())
    }

    /// Rotate the top 3 elements to the left once
    // A B C => B C A
    pub fn rot3(&mut self) -> Result<(), TypeError> {
        let len = self.items.len();
        let item1 = &self.items.get(len - 1).ok_or(TypeError::unspecified())?;
        let item2 = &self.items.get(len - 2).ok_or(TypeError::unspecified())?;
        let item3 = &self.items.get(len - 3).ok_or(TypeError::unspecified())?;
        let size1 = BuiltinType::from(item1.typeid).size();
        let size2 = BuiltinType::from(item2.typeid).size();
        let size3 = BuiltinType::from(item3.typeid).size();
        self.items[len - 3 .. len].rotate_left(1);
        self.common.rot3(size1, size2, size3); // perform the actual rotation (rot3)
        Ok(())
    }

    /// Rotate the top 4 elements to the left once
    // A B C D => B C D A
    pub fn rot4(&mut self) -> Result<(), TypeError> {
        let len = self.items.len();
        let item1 = &self.items.get(len - 1).ok_or(TypeError::unspecified())?;
        let item2 = &self.items.get(len - 2).ok_or(TypeError::unspecified())?;
        let item3 = &self.items.get(len - 3).ok_or(TypeError::unspecified())?;
        let item4 = &self.items.get(len - 4).ok_or(TypeError::unspecified())?;
        let size1 = BuiltinType::from(item1.typeid).size();
        let size2 = BuiltinType::from(item2.typeid).size();
        let size3 = BuiltinType::from(item3.typeid).size();
        let size4 = BuiltinType::from(item4.typeid).size();
        self.items[len - 4 .. len].rotate_left(1);
        self.common.rot4(size1, size2, size3, size4); // perform the actual rotation (rot4)
        Ok(())
    }

    /// A general wrapper for a builtin math operation of
    /// the signature (int int -> int)
    fn math_op_1(&mut self, op: impl FnOnce(usize, usize) -> usize) -> Result<(), TypeError> {
        let item1 = self.peek(0).ok_or(TypeError::unspecified())?;
        let item2 = self.peek(1).ok_or(TypeError::unspecified())?;
        if item1.typeid != BuiltinType::Int.into() ||
           item2.typeid != BuiltinType::Int.into() {
            return Err(TypeError::expect_types(vec![BuiltinType::Int, BuiltinType::Int], vec![item1.clone(), item2.clone()]))
        }
        // comptime or runtime
        if item1.comptime && item2.comptime {
            self.common.math_op_1(op);
            self.items.truncate(self.items.len() - 2);
            self.items.push(ComptimeInfo::minimal(BuiltinType::Int.into(), true));
        } else {
            self.common.shrink_by(8 * 2); // todo: use actual size
            self.items.truncate(self.items.len() - 2);
            self.items.push(ComptimeInfo::minimal(BuiltinType::Int.into(), false));
        }
        Ok(())
    }

    /// A general wrapper for a builtin math operation of
    /// the signature (int int -> int int)
    fn math_op_2(&mut self, op: impl FnOnce(usize, usize) -> (usize, usize)) -> Result<(), TypeError> {
        let item1 = self.peek(0).ok_or(TypeError::unspecified())?;
        let item2 = self.peek(1).ok_or(TypeError::unspecified())?;
        // let size1 = self.typemap.size_by_typeid(&item1.typeid);
        if item1.typeid != BuiltinType::Int.into() ||
           item2.typeid != BuiltinType::Int.into() {
            return Err(TypeError::expect_types(vec![BuiltinType::Int, BuiltinType::Int], vec![item1.clone(), item2.clone()]))
        }
        // comptime or runtime
        if item1.comptime && item2.comptime {
            self.common.math_op_2(op);
            self.items.truncate(self.items.len() - 2);
            self.items.push(ComptimeInfo::minimal(BuiltinType::Int.into(), true));
            self.items.push(ComptimeInfo::minimal(BuiltinType::Int.into(), true));
        } else {
            self.common.shrink_by(8 * 2); // todo: use actual size
            self.items.truncate(self.items.len() - 2);
            self.items.push(ComptimeInfo::minimal(BuiltinType::Int.into(), false));
            self.items.push(ComptimeInfo::minimal(BuiltinType::Int.into(), false));
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

// a value that was popped from the stack
// and it's associated comptime data
#[derive(Clone)]
pub(crate) struct Item {
    pub info: ComptimeInfo,
    data: Vec<u8>, // todo: use tinyvec / stackvec for Vec<u8>
}

impl Item {
    pub const fn comptime(typeid: TypeId, data: Vec<u8>) -> Self {
        Self { info: ComptimeInfo::minimal(typeid, true), data }
    }
    pub const fn runtime(typeid: TypeId) -> Self {
        Self { info: ComptimeInfo::minimal(typeid, false), data: Vec::new() }
    }
}

pub(crate) fn usize_to_bytes(value: usize) -> Vec<u8> {
    value.to_ne_bytes().to_vec()
}

pub(crate) fn usize_from_bytes(value: Vec<u8>) -> usize {
    usize::from_ne_bytes(value.try_into().unwrap())
}

pub(crate) fn bool_to_bytes(value: bool) -> Vec<u8> {
    vec![value as u8]
}

pub(crate) fn bool_from_bytes(value: Vec<u8>) -> bool {
    value[0] != 0
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
    pub(crate) fn expect_types(want: Vec<BuiltinType>, got: Vec<ComptimeInfo>) -> Self {
        Self::unspanned(TypeErrorKind::Expect {
            want: want.into_iter().map(|t| ComptimeInfo::minimal(t.into(), false)).collect(),
            got
        })
    }
    pub(crate) fn expect(want: Vec<ComptimeInfo>, got: Vec<ComptimeInfo>) -> Self {
        Self::unspanned(TypeErrorKind::Expect { want, got })
    }
}

pub(crate) enum TypeErrorKind {
    Unspecified,
    UnknownFn { name: String },
    BranchesNotEmpty,
    BranchesNotEqual,
    Expect { want: Vec<ComptimeInfo>, got: Vec<ComptimeInfo> },
}

pub(crate) fn format_error(value: TypeError) -> Diagnostic {
    let diag = match value.kind {
        TypeErrorKind::Unspecified => unreachable!("unspecified type error"), // should be replaced by a more specific error
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

fn format_metadata(mds: Vec<ComptimeInfo>) -> String {
    format!("{:?}", mds.into_iter().map(|it| {
        if it.comptime { format!("comptime id {:?}", BuiltinType::from(it.typeid)) } // todo: format using actual names
        else { format!("id {:?}", BuiltinType::from(it.typeid)) }
    }).collect::<Vec<_>>())
}

