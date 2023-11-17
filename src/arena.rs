
use std::{fmt, cell::{RefCell, Ref}};

const INVALID_ID: &str = "invalid id for this arena";

#[derive(Default)]
pub(crate) struct StrArena {
    pub buffer: RefCell<Vec<u8>>, // todo: do not use RefCell here, but wrap the StrArena inside a RefCrell when we use it inside the parser
}

impl StrArena {

    pub fn put(&self, val: &str) -> Id {
        let mut mut_buffer = self.buffer.borrow_mut();
        let from = mut_buffer.len() as u32;
        let to   = from + val.len() as u32 - 1;
        mut_buffer.extend_from_slice(val.as_bytes());
        dbg!(from, to, val.len());
        Id { from, to }
    }

    pub fn get<'a>(&'a self, id: Id) -> Ref<'a, str> {
        let buffer = self.buffer.borrow();
        dbg!(id.from, id.to);
        Ref::map(buffer, |buf| {
            std::str::from_utf8(
                buf.get(id.from as usize .. id.to as usize).expect(INVALID_ID)
            ).expect(INVALID_ID)
        })
    }

}

impl fmt::Debug for StrArena {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("StrArena").finish_non_exhaustive()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] // todo: remove eq, hash etc. derive
pub(crate) struct Id {
    from: u32,
    to: u32,
}

