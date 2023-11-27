
use std::{alloc, collections::HashMap, slice};
use crate::{
    basegen::{Program, FunWithMetadata, InstrKind, InstrLiteral},
    diagnostic::Diagnostic,
    parser::{Span, TranslationUnit},
    arch::Intrinsic,
    arena::StrArena
};

pub(crate) fn eval<I: Intrinsic>(input: TranslationUnit<Program<I>>) -> Result<(), EvalError> {

    let program = input.inner;

    let mut memory = Memory::new();

    // set up the stack
    let stack = memory.alloc(1024, 8).expect("allocate stack memory"); // todo: calculate needed size during typegen

    // todo: push statics (str literals) onto the stack and set them up
    // todo: do all of this alrdy in the typegen?? setup stack alrdy in typegen??

    let mut state = State {
        common: CommonState {
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
    common: CommonState<'a>,
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

pub(crate) struct CommonState<'a> {
    pub arena: &'a StrArena,
    pub memory: Memory,
    pub stack: MemoryPtr,
}

impl<'a> CommonState<'a> {

    #[track_caller]
    pub fn push(&mut self, data: Vec<u8>) {
        let slice = self.memory.access_mut(self.stack).expect("access stack memory");
        slice[..data.len()].copy_from_slice(&data);
        self.stack.add_offset(data.len() as isize);
    }

    // todo: type information has to be encoded into the intrinsics
    #[track_caller]
    pub fn push_int(&mut self, value: usize) {
        let slice = self.memory.access_mut(self.stack).expect("access stack memory");
        slice[..8].copy_from_slice(&usize::to_ne_bytes(value));
        self.stack.add_offset(8);
    }

    #[track_caller]
    pub fn pop(&mut self, size: usize) -> Vec<u8> {
        self.stack.add_offset(- (size as isize));
        let slice = self.memory.access_mut(self.stack).expect("access stack memory");
        Vec::from(&slice[..size])
    }

    #[track_caller]
    pub fn pop_int(&mut self) -> usize {
        self.stack.add_offset(-8);
        let slice = self.memory.access_mut(self.stack).expect("access stack memory");
        let result = usize::from_ne_bytes(
            slice[..8].try_into().unwrap()
        );
        result
    }

}

// pub(crate) struct TypeMetadata {
//     pub tid: Type, // todo: later: TypeId
//     // pub size: u16, // fast path for type size
//     pub aligned: u8, // how many bytes of padding were added for this item to be aligned
// }

// todo: Pointers have to be stored with the same 64 bit size as the compiled version,
//       so a pointer is like (u32: MemoryId, u32: Index)

/// provides safe allocation for the VM
/// uses the global allocator to allocate objects
pub(crate) struct Memory {
    objects: HashMap<u32, MemoryMetadata>,
    current_id: u32,
    // todo: add a field "last_valid" that caches the last accessed still valid MemoryPtr so we dont have to do a hashmap lookup every time
}

impl Memory {

    pub fn new() -> Self {
        Self {
            objects: HashMap::new(),
            current_id: 0,
        }
    }

    pub fn alloc(&mut self, size: usize, align: usize) -> Result<MemoryPtr, MemoryError> {
        self.current_id += 1;
        if size == 0 { return Err(MemoryError::InvalidLayout) };
        let layout = alloc::Layout::from_size_align(size, align).map_err(|_| MemoryError::InvalidLayout)?;
        // SAFETY: layout is valid
        let ptr = unsafe { alloc::alloc(layout) };
        if ptr.is_null() { alloc::handle_alloc_error(layout) };
        let mp = MemoryPtr::new(self.current_id);
        self.objects.insert(mp.id, MemoryMetadata { layout, ptr, size, initialized: false });
        Ok(mp)
    }

    pub fn dealloc(&mut self, mp: MemoryPtr) -> Result<(), MemoryError> {
        let metadata = self.objects.get(&mp.id).ok_or(MemoryError::InvalidAccess)?;
        // SAFETY: if we found the layout the ptr will be valid
        unsafe { alloc::dealloc(metadata.ptr, metadata.layout) };
        self.objects.remove(&mp.id);
        Ok(())
    }

    // todo: make write and read methods to enable checking if memory was initialized before reading
    pub fn access<'a>(&'a self, mp: MemoryPtr) -> Result<&'a [u8], MemoryError> {
        let metadata = self.objects.get(&mp.id).ok_or(MemoryError::InvalidAccess)?;
        if !metadata.initialized { return Err(MemoryError::Uninitialized) };
        // SAFETY: if we found the layout the ptr will be valid
        let slice = unsafe { slice::from_raw_parts(metadata.ptr, metadata.size) };
        slice.get(mp.offset as usize..).ok_or(MemoryError::InvalidAccess)
    }

    pub fn access_mut<'a>(&'a mut self, mp: MemoryPtr) -> Result<&'a mut [u8], MemoryError> {
        let metadata = self.objects.get_mut(&mp.id).ok_or(MemoryError::InvalidAccess)?;
        metadata.initialized = true;
        // SAFETY: if we found the layout the ptr will be valid and we borrow self mutably
        let slice = unsafe { slice::from_raw_parts_mut(metadata.ptr, metadata.size) };
        slice.get_mut(mp.offset as usize..).ok_or(MemoryError::InvalidAccess)
    }

    pub fn allocations(&self) -> usize {
        self.objects.len()
    }

}

#[derive(Clone, Copy)]
pub(crate) struct MemoryPtr {
    pub id: u32,
    pub offset: u32,
}

impl MemoryPtr {
    pub fn new(id: u32) -> Self {
        Self { id, offset: 0 }
    }
    pub fn whole(mut self) -> Self {
        self.offset = 0;
        self
    }
    pub fn add_offset(&mut self, offset: isize) {
        self.offset = (self.offset as isize + offset) as u32;
    }
    pub fn with_offset(mut self, offset: isize) -> Self {
        self.offset = (self.offset as isize + offset) as u32;
        self
    }
}

pub(crate) struct MemoryMetadata {
    layout: alloc::Layout,
    ptr: *mut u8,
    size: usize,
    initialized: bool,
}

#[derive(Debug)]
pub(crate) enum MemoryError {
    InvalidLayout,
    InvalidAccess,
    Uninitialized,
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

