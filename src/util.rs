use std::sync::{Mutex, MutexGuard};

pub trait IgnoreMutexPoison<T> {
    /// Equivalent to `.lock().unwrap()`, for when handling poisoning is not needed.
    fn lock_unpoisoned(&self) -> MutexGuard<T>;
}

impl<T> IgnoreMutexPoison<T> for Mutex<T> {
    fn lock_unpoisoned(&self) -> MutexGuard<T> {
        self.lock().expect("unpoisoned mutex")
    }
}
