
//! Functionality that allows calling and typechecking rust functions from within rough.
//! This is used for implementing builtins and intrisics.
//! Also contains the famous sign function.

use std::{iter::zip, mem::MaybeUninit, array};
use crate::typegen::{TypeId, Item, ComptimeInfo};

pub(crate) trait Bridged {
    const TYPEID: TypeId;
    fn from_rough(item: Item) -> Self;
    fn into_rough(this: Self) -> Item;
}

impl Bridged for Item {
    const TYPEID: TypeId = 0; // todo: don't hardcode (0 is special typeid for any type, which is like `any`)
    fn from_rough(item: Item) -> Self {
        item
    }
    fn into_rough(this: Self) -> Item {
        this
    }
}

impl Bridged for u64 { // todo: ususe smth like RhInt alis
    const TYPEID: TypeId = 2; // todo: don't hardcode typeid
    fn from_rough(item: Item) -> Self {
        u64::from_ne_bytes(item.data.try_into().unwrap())
    }
    fn into_rough(this: Self) -> Item {
        Item::comptime(2, u64::to_ne_bytes(this).to_vec()) // todo: dont hardcode typeid

    }
}

pub(crate) trait BridgedCompound {
    const TYPEIDS: &'static [TypeId];
    fn from_rough(items: Vec<Item>) -> Self;
    fn into_rough(this: Self) -> Vec<Item>;
}

impl<T: Bridged> BridgedCompound for T {

    const TYPEIDS: &'static [TypeId] = &[T::TYPEID];

    fn from_rough(mut items: Vec<Item>) -> Self {
        debug_assert!(items.len() == 1);
        T::from_rough(
            items.pop().unwrap()
        )
    }

    fn into_rough(this: Self) -> Vec<Item> {
        Vec::from([
            T::into_rough(this)
        ])
    }

}

impl<const LEN: usize, T: Bridged> BridgedCompound for [T; LEN] {

    const TYPEIDS: &'static [TypeId] = &[T::TYPEID; LEN];

    fn from_rough(items: Vec<Item>) -> Self {
        debug_assert!(items.len() == LEN); // removing would make this unsound
        let mut result = array::from_fn(|_| MaybeUninit::uninit());
        for (src, dst) in zip(items, result.iter_mut()) {
            dst.write(T::from_rough(src));
        }
        result.map(|it| unsafe { it.assume_init() })
    }

    fn into_rough(this: Self) -> Vec<Item> {
        Vec::from(
            this.map(|it| T::into_rough(it))
        )
    }

}

pub(crate) mod calc {

    use crate::typegen::Item;

    pub(crate) fn add([lhs, rhs]: [u64; 2]) -> u64 {
        lhs + rhs
    }

    pub(crate) fn swap([lhs, rhs]: [Item; 2]) -> [Item; 2] {
        [rhs, lhs]
    }

}

// pub(crate) fn sign(items: &[ComptimeInfo], signature: &[ComptimeInfo]) -> bool {
// 
//     let mut valid = false;
// 
//     for (want, got) in zip(signature, items.iter().rev()) {
// 
//         debug_assert!(want.typeid == 1, "must be of type `type`"); // todo: don't hardcode 1 here
//         let typeid = u64::from_ne_bytes(want.data.try_into().unwrap()); // todo: use bridged conversion function to convert to TypeId
//         valid &= typeid == got.typeid;
// 
//     }
// 
//     valid
// 
// }

pub(crate) fn builtin_signature<const LEN: usize>(typeids: [TypeId; LEN]) -> [ComptimeInfo; LEN] {
    typeids.map(|typeid|
        ComptimeInfo::full(1, true, typeid.to_ne_bytes().to_vec()) // todo: use bridged conversion to convert to typeId
    ) // todo: dont hardcode 1
}

