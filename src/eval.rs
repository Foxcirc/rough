
use std::{alloc, collections::HashMap, slice};
use crate::{
    basegen::{Program, FunWithMetadata, InstrKind},
    diagnostic::Diagnostic,
    parser::{Span, TranslationUnit},
    arch::Intrinsic,
    arena::StrArena
};

pub(crate) fn eval<I: Intrinsic>(input: TranslationUnit<Program<I>>) -> Result<(), EvalError> {

    let program = input.inner;

    let mut state = State {
        memory: Vec::new(),
        arena: input.arena,
    };

    let main = input.main.and_then(|name| program.funs.get(&name));
    let fun = match main {
        Some(val) => val,
        None => return Err(EvalError::unspanned(EvalErrorKind::NoEntryPoint)),
    };

    eval_fun(&mut state, fun)?;

   Ok(())

}

fn eval_fun<I: Intrinsic>(state: &mut State, fun: &FunWithMetadata<I>) -> Result<(), EvalError> {
    
    for instr in fun.body.iter() {

        match &instr.kind {

            InstrKind::Label { label, producer } => todo!(),

            InstrKind::Push { value } => todo!(),

            InstrKind::Call { to } => todo!(),
            InstrKind::Return => todo!(),

            InstrKind::Drop => todo!(),
            InstrKind::Copy => todo!(),
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

    Ok(())

}

struct State {
    pub arena: StrArena,
    pub memory: Vec<u8>,
}

/// provides safe allocation for the VM
/// uses the global allocator to allocate objects
pub(crate) struct Memory {
    objects: HashMap<MemoryId, MemoryMetadata>,
}

impl Memory {

    pub fn new() -> Self {
        Self { objects: HashMap::new() }
    }

    pub fn alloc(&mut self, size: usize, align: usize) -> Result<MemoryId, MemoryError> {
        if size == 0 || align == 0 { return Err(MemoryError::InvalidLayout) };
        let layout = alloc::Layout::from_size_align(size, align).map_err(|_| MemoryError::InvalidLayout)?;
        // SAFETY: layout is valid
        let ptr = unsafe { alloc::alloc(layout) };
        if ptr.is_null() { alloc::handle_alloc_error(layout) };
        let id = MemoryId::new(ptr as usize);
        self.objects.insert(id, MemoryMetadata { layout, ptr, size });
        Ok(id)
    }

    pub fn dealloc(&mut self, id: MemoryId) -> Result<(), MemoryError> {
        let metadata = self.objects.get(&id).ok_or(MemoryError::InvalidId)?;
        // SAFETY: if we found the layout the ptr will be valid
        unsafe { alloc::dealloc(metadata.ptr, metadata.layout) };
        self.objects.remove(&id);
        Ok(())
    }

    pub fn access<'a>(&'a self, id: MemoryId) -> Result<&'a mut [u8], MemoryError> {
        let metadata = self.objects.get(&id).ok_or(MemoryError::InvalidId)?;
        // SAFETY: if we found the layout the ptr will be valid
        Ok(unsafe { slice::from_raw_parts_mut(metadata.ptr, metadata.size) })
    }

}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct MemoryId {
    id: usize,
}

impl MemoryId {
    fn new(id: usize) -> Self {
        Self { id }
    }
}

pub(crate) struct MemoryMetadata {
    layout: alloc::Layout,
    ptr: *mut u8,
    size: usize,
}

#[derive(Debug)]
pub(crate) enum MemoryError {
    InvalidLayout,
    InvalidId,
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

