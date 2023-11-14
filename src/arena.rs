
use std::fmt;

const INVALID_ID: &str = "invalid id for this arena";

#[derive(Default)]
pub(crate) struct StrArena {
    buffer: Vec<u8>,
}

impl StrArena {

    pub fn with_capacity(cap: usize) -> Self {
        Self {
            buffer: Vec::with_capacity(cap),
        }
    }

    pub fn put(&mut self, val: &str) -> Id {
        let from = self.buffer.len() as u32;
        let to   = from + val.len() as u32;
        self.buffer.copy_from_slice(val.as_bytes());
        Id { from, to }
    }

    pub fn get(&self, id: Id) -> &str {
        std::str::from_utf8(
            &self.buffer.get(id.from as usize .. id.to as usize).expect(INVALID_ID)
        ).expect(INVALID_ID)
    }

}

impl fmt::Debug for StrArena {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("StrArena").finish_non_exhaustive()
    }
}

pub(crate) struct Id {
    from: u32,
    to: u32,
}

