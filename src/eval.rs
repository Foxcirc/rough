
use std::{alloc, collections::HashMap, slice, mem};
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
    let allocation = memory.alloc(1024, 8).expect("allocate stack memory"); // todo: calculate needed size during typegen
    let stack = Stack::new(allocation);

    // todo: push statics (str literals) onto the stack and set them up
    // todo: do all of this alrdy in the typegen?? setup stack alrdy in typegen??

    let mut state = State {
        arena: input.arena,
        memory,
        stack,
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

            InstrKind::Push { value: InstrLiteral::Int(number) } => state.stack.push_int(&mut state.memory, *number),
            InstrKind::Push { .. } => todo!(),

            InstrKind::Call { to } => todo!(),
            InstrKind::Return => return Ok(()),

            InstrKind::Drop => {
                state.stack.pop_int(&mut state.memory);
            },
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
                let lhs = state.stack.pop_int(&mut state.memory);
                let rhs = state.stack.pop_int(&mut state.memory);
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

struct State {
    pub arena: StrArena,
    pub memory: Memory,
    pub stack: Stack,
}

/// representation of the stack, that enables safely
/// accessing everything as raw bytes etc.
/// needs to be created with a [`Memory`]
pub(crate) struct Stack {
    pub id: MemoryId,
    pub pos: usize,
}

// pub(crate) struct TypeMetadata {
//     pub tid: Type, // todo: later: TypeId
//     // pub size: u16, // fast path for type size
//     pub aligned: u8, // how many bytes of padding were added for this item to be aligned
// }

// todo: Pointers have to be stored with the same 64 bit size as the compiled version,
//       so a pointer is like (u32: MemoryId, u32: Index)

impl Stack {

    pub fn new(id: MemoryId) -> Self {
        Self { id, pos: 0 }
    }

    // todo: type information has to be encoded into the intrinsics
    pub fn push_int(&mut self, memory: &mut Memory, value: usize) {
        let slice = memory.access_mut(self.id).expect("access stack memory");
        slice[self.pos .. self.pos + 8].copy_from_slice(&usize::to_ne_bytes(value));
        self.pos += 8;
    }

    pub fn pop_int(&mut self, memory: &mut Memory) -> usize {
        let slice = memory.access_mut(self.id).expect("access stack memory");
        let result = usize::from_ne_bytes(slice[self.pos - 8 .. self.pos].try_into().expect("int conversion"));
        self.pos -= 8;
        result
    }

}

/// provides safe allocation for the VM
/// uses the global allocator to allocate objects
pub(crate) struct Memory {
    objects: HashMap<MemoryId, MemoryMetadata>,
    current_id: u32,
}

impl Memory {

    pub fn new() -> Self {
        Self {
            objects: HashMap::new(),
            current_id: 0,
        }
    }

    pub fn alloc(&mut self, size: usize, align: usize) -> Result<MemoryId, MemoryError> {
        self.current_id += 1;
        if size == 0 || align == 0 { return Err(MemoryError::InvalidLayout) };
        let layout = alloc::Layout::from_size_align(size, align).map_err(|_| MemoryError::InvalidLayout)?;
        // SAFETY: layout is valid
        let ptr = unsafe { alloc::alloc(layout) };
        if ptr.is_null() { alloc::handle_alloc_error(layout) };
        let id = MemoryId::new(self.current_id);
        self.objects.insert(id, MemoryMetadata { layout, ptr, size, initialized: false });
        Ok(id)
    }

    // pub fn alloc_t<T>(&mut self) -> Result<MemoryId, MemoryError> {
    //     let layout = alloc::Layout::new::<T>();
    //     self.alloc(layout.size(), layout.align())
    // }

    pub fn dealloc(&mut self, id: MemoryId) -> Result<(), MemoryError> {
        let metadata = self.objects.get(&id).ok_or(MemoryError::InvalidId)?;
        // SAFETY: if we found the layout the ptr will be valid
        unsafe { alloc::dealloc(metadata.ptr, metadata.layout) };
        self.objects.remove(&id);
        Ok(())
    }

    pub fn access<'a>(&'a self, id: MemoryId) -> Result<&'a [u8], MemoryError> {
        let metadata = self.objects.get(&id).ok_or(MemoryError::InvalidId)?;
        if !metadata.initialized { return Err(MemoryError::Uninitialized) };
        // SAFETY: if we found the layout the ptr will be valid
        Ok(unsafe { slice::from_raw_parts(metadata.ptr, metadata.size) })
    }

    pub fn access_mut<'a>(&'a mut self, id: MemoryId) -> Result<&'a mut [u8], MemoryError> {
        let metadata = self.objects.get_mut(&id).ok_or(MemoryError::InvalidId)?;
        metadata.initialized = true;
        // SAFETY: if we found the layout the ptr will be valid and we borrow self mutably
        Ok(unsafe { slice::from_raw_parts_mut(metadata.ptr, metadata.size) })
    }

    // pub fn access_t<'a, T>(&'a self, id: MemoryId) -> Result<&'a T, MemoryError> {
    //     let metadata = self.objects.get(&id).ok_or(MemoryError::InvalidId)?;
    //     // IMPORTANT: don't remove these assertions as they are safety checks
    //     // todo: check for align
    //     assert!(metadata.size == mem::size_of::<T>(), "size of T has to match the size of the allocation");
    //     assert!(metadata.initialized, "trying to access uninitialized data");
    //     unsafe { Ok(&*(self.access(id)?.as_ptr() as *const T)) }
    // }


    // pub fn access_t_mut<'a, T>(&'a mut self, id: MemoryId) -> Result<&'a mut mem::MaybeUninit<T>, MemoryError> {
    //     let metadata = self.objects.get(&id).ok_or(MemoryError::InvalidId)?;
    //     // IMPORTANT: don't remove these assertions as they are safety checks
    //     // todo: check for align
    //     assert!(metadata.size == mem::size_of::<T>(), "size of T has to match the size of the allocation");
    //     unsafe { Ok(&mut *(self.access_mut(id)?.as_mut_ptr() as *mut mem::MaybeUninit<T>)) }
    // }

    pub fn allocations(&self) -> usize {
        self.objects.len()
    }

}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct MemoryId {
    id: u32,
}

impl MemoryId {
    fn new(id: u32) -> Self {
        Self { id }
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
    InvalidId,
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

