use std::sync::{Mutex, MutexGuard};

pub trait IgnoreMutexPoison<T> {
    /// Equivalent to `.lock().unwrap()`, for when handling poisoning is not needed.
    fn lock_unpoisoned(&self) -> MutexGuard<T>;

    /// Equivalent to `.into_inner().unwrap()`, for when handling poisoning is not needed.
    fn into_inner_unpoisoned(self) -> T;
}

impl<T> IgnoreMutexPoison<T> for Mutex<T> {
    fn lock_unpoisoned(&self) -> MutexGuard<T> {
        self.lock().expect("unpoisoned mutex")
    }

    fn into_inner_unpoisoned(self) -> T {
        self.into_inner().expect("unpoisoned mutex")
    }
}

pub mod serde_u128_as_hex {
    use serde::{Deserializer, Serializer};

    pub fn serialize<S: Serializer>(val: &u128, serializer: S) -> Result<S::Ok, S::Error> {
        hex::serialize(val.to_le_bytes(), serializer)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(deserializer: D) -> Result<u128, D::Error> {
        hex::deserialize(deserializer).map(u128::from_le_bytes)
    }
}
