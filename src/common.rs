
use std::{alloc, mem, ptr, collections::HashMap, slice};

use crate::intern;

// todo: Pointers have to be stored with the same 64 bit size as the compiled version,
//       so a pointer is like (u32: MemoryId, u32: Index)

/// provides safe allocation for the VM
/// uses the global allocator to allocate objects
pub(crate) struct Memory {
    objects: HashMap<u32, MemoryMetadata>,
    current_id: u32,
    // todo: add a field "last_valid" that caches the last accessed still valid MemoryPtr so we dont have to do a hashmap lookup every time
    //       this would be useful for byte-byte byte reads/writes
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


pub(crate) struct CommonState<'a> {
    pub arena: &'a intern::StrInterner,
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

    #[track_caller]
    pub fn pop(&mut self, size: usize) -> Vec<u8> { // todo: use StackVec
        self.stack.add_offset(- (size as isize));
        let slice = self.memory.access_mut(self.stack).expect("access stack memory");
        Vec::from(&slice[..size])
    }

    /// Shorten the stack by `size`
    /// Inside a debug build this still checks if the new pointer is valid to prevent an accessing error
    /// from occuring later
    #[track_caller]
    pub fn shrink_by(&mut self, size: usize) {
        self.stack.add_offset(- (size as isize));
        if cfg!(debug_assertions) {
            self.memory.access_mut(self.stack).expect("shrink: check stack memory");
        }
    }

    #[track_caller]
    pub fn dup(&mut self, size: usize) {

        self.stack.add_offset(- (size as isize));

        let slice = self.memory.access_mut(self.stack).expect("access stack memory");
        let (item, rest) = slice.split_at_mut(size);

        rest[..size].copy_from_slice(item);

        self.stack.add_offset(size as isize * 2);

    }

    /// size1 is the topmost element, size2 is the element that will be duped
    #[track_caller]
    pub fn over(&mut self, size1: usize, size2: usize) {

        self.stack.add_offset(- ((size1 + size2) as isize));

        let slice = self.memory.access_mut(self.stack).expect("access stack memory");
        let (items, rest) = slice.split_at_mut(size1 + size2);

        rest[..size2].copy_from_slice(&items[..size2]);

        self.stack.add_offset((size1 + size2 * 2) as isize);

    }

    /// size1 is the topmost element
    #[track_caller]
    pub fn swap(&mut self, size1: usize, size2: usize) {

        // optimized implemntation if the elements have the same size
        // otherwise a naive implementation
        if size1 == size2 {

            self.stack.add_offset(- (size1 as isize * 2));

            let slice = self.memory.access_mut(self.stack).expect("access stack memory");
            let (one, rest) = slice.split_at_mut(size1);
            let two = &mut rest[..size1];

            one.swap_with_slice(two);

            self.stack.add_offset(size1 as isize * 2);

        } else {

            let one = self.pop(size1);
            let two = self.pop(size2);
            self.push(one);
            self.push(two);

        }

    }

    /// size1 is the topmost element
    #[track_caller]
    pub fn rot3(&mut self, size1: usize, size2: usize, size3: usize) {

        // optimized implemntation if the elements have the same size
        // otherwise a naive implementation
        if size1 == size2 && size1 == size3 {

            self.stack.add_offset(- (size1 as isize * 3));

            let slice = self.memory.access_mut(self.stack).expect("access stack memory");

            let items = &mut slice[..size1 * 3];
            items.rotate_left(size1);

            self.stack.add_offset(size1 as isize * 3);

        } else {

            let one = self.pop(size1);
            let two = self.pop(size2);
            let three = self.pop(size3);
            self.push(one);
            self.push(three);
            self.push(two);

        }

    }

    /// size1 is the topmost element
    // A B C D => D A B C
    #[track_caller]
    pub fn rot4(&mut self, size1: usize, size2: usize, size3: usize, size4: usize) {

        // optimized implemntation if the elements have the same size
        // otherwise a naive implementation
        if size1 == size2 && size1 == size3 && size1 == size4 {

            self.stack.add_offset(- (size1 as isize * 4));

            let slice = self.memory.access_mut(self.stack).expect("access stack memory");

            let items = &mut slice[..size1 * 4];
            items.rotate_left(size1);

            self.stack.add_offset(size1 as isize * 4);

        } else {

            let one = self.pop(size1);
            let two = self.pop(size2);
            let three = self.pop(size3);
            let four = self.pop(size4);
            self.push(one);
            self.push(four);
            self.push(three);
            self.push(two);

        }

    }

    /// determines the sizes by the rust types passed as closure parameters
    #[track_caller]
    pub fn math_op_1(&mut self, op: impl FnOnce(usize, usize) -> usize) {

        let size = mem::size_of::<usize>(); // todo: this will not always be 8 (same as when pushing??)
        self.stack.add_offset(- (size as isize * 2));

        let slice = self.memory.access_mut(self.stack).expect("access stack memory");
        let (ptr2, rest) = slice.split_at_mut(size);
        let ptr1 = &rest[..size];

        let item2 = usize::from_ne_bytes(ptr2.try_into().unwrap()); // todo: update integer type to be u64 or at least a custom alias like RhInt
        let item1 = usize::from_ne_bytes(ptr1.try_into().unwrap());
        let result = op(item2, item1);

        ptr2.copy_from_slice(&result.to_ne_bytes());

        self.stack.add_offset(size as isize);

    }

    /// Pushes the 0th tuple value onto the stack first
    #[track_caller]
    pub fn math_op_2(&mut self, op: impl FnOnce(usize, usize) -> (usize, usize)) {

        let size = mem::size_of::<usize>(); // todo: this will not always be 8 (same as when pushing??)
        self.stack.add_offset(- (size as isize * 2));

        let slice = self.memory.access_mut(self.stack).expect("access stack memory");
        let (ptr2, rest) = slice.split_at_mut(size);
        let ptr1 = &mut rest[..size];

        let item2 = usize::from_ne_bytes(ptr2.try_into().unwrap()); // todo: update integer type to be u64 or at least a custom alias like RhInt
        let item1 = usize::from_ne_bytes(ptr1.try_into().unwrap());
        let (result2, result1) = op(item2, item1);

        ptr2.copy_from_slice(&result2.to_ne_bytes());
        ptr1.copy_from_slice(&result1.to_ne_bytes());

        self.stack.add_offset(size as isize * 2);

    }

    /// determines the sizes by the rust types passed as closure parameters
    #[track_caller]
    pub fn bool_op_1(&mut self, op: impl FnOnce(bool, bool) -> bool) {

        let size = mem::size_of::<bool>();
        self.stack.add_offset(- (size as isize * 2));

        let slice = self.memory.access_mut(self.stack).expect("access stack memory"); // todo: make access_mut accept a size (length of the returned slice) so bounds checking errors happend directly
        let (ptr2, rest) = slice.split_at_mut(size);
        let ptr1 = &rest[..size];

        let item2 = ptr1[0] != 0; // convert to bool
        let item1 = ptr2[0] != 0; // convert to bool
        let result = op(item2, item1);

        ptr2[0] = result as u8;

        self.stack.add_offset(size as isize);

    }

}

