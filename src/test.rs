
use crate::common;

#[test]
fn memory() {

    let mut memory = common::Memory::new();
    let id = memory.alloc(1024, 1).unwrap();
    let slice = memory.access_mut(id).unwrap();
    slice.fill(0x41);
    let more_slice = memory.access(id).unwrap();
    let text = unsafe { std::str::from_utf8_unchecked(more_slice) };
    assert!(text == "A".repeat(1024));
    memory.dealloc(id).unwrap();
    assert!(memory.allocations() == 0);

}

