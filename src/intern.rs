
use std::{hash::Hash, collections::HashMap, sync::atomic::{AtomicUsize, Ordering}};

#[cfg(debug_assertions)] static IDS: AtomicUsize = AtomicUsize::new(0);

// An arena that interns strings.
#[derive(Default)]
pub(crate) struct StrInterner {
    #[cfg(debug_assertions)] id: usize,
    lookup: HashMap<usize, (u32, u32)>,
    data: Vec<u8>,
}

impl StrInterner {

    pub fn new() -> Self {
        Self {
            #[cfg(debug_assertions)] id: IDS.fetch_add(1, Ordering::SeqCst),
            lookup: HashMap::new(),
            data: Vec::new()
        }
    }

    pub fn put(&mut self, value: &str) -> InternedStr {
        let key = fxhash::hash(&value);
        let id = if let Some(id) = self.lookup.get(&key) {
            *id
        } else {
            let from = self.data.len() as u32;
            let to = from + value.len() as u32;
            let id = (from, to);
            self.data.extend_from_slice(value.as_bytes());
            self.lookup.insert(key, id);
            id
        };
        InternedStr {
            inner: id,
            #[cfg(debug_assertions)] interner: self.id,
        }
    }

    pub fn get<'b>(&self, interned: &'b InternedStr) -> &str {
        #[cfg(debug_assertions)]
        if interned.interner != self.id {
            panic!("tried to get item that was put into another interner")
        };
        std::str::from_utf8(
            &self.data[interned.inner.0 as usize .. interned.inner.1 as usize]
        ).unwrap()
    }

}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct InternedStr {
    pub inner: (u32, u32),
    #[cfg(debug_assertions)] interner: usize,
}

