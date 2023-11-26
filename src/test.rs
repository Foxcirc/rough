
use crate::eval;

#[test]
fn memory() {

    let mut memory = eval::Memory::new();
    let id = memory.alloc(1024, 1).unwrap();
    let slice = memory.access(id).unwrap();
    slice.fill(0x31);
    let more_slice = memory.access(id).unwrap();
    let text = unsafe { std::str::from_utf8_unchecked(more_slice) };
    println!("{}", text);
    memory.dealloc(id).unwrap();

}

