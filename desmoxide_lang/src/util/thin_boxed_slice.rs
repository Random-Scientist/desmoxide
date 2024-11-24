use core::borrow::Borrow;
use core::cmp::max;
use core::hash::{Hash, Hasher};
use core::marker::PhantomData;
use core::mem::{align_of, size_of};
use core::ops::{Deref, DerefMut};
use core::ptr::NonNull;
use core::slice;
use std::alloc;

// from: https://github.com/seekstar/thin-boxed-slice/blob/main/src/lib.rs (with some cleanup and modifications)

#[derive(Debug)]
pub struct ThinBoxedSlice<T> {
    p: NonNull<u8>,
    phantom: PhantomData<T>,
}

impl<T> ThinBoxedSlice<T> {
    const fn array_offset() -> usize {
        let align = align_of::<T>();
        let misalign = size_of::<usize>() % align;
        let padding = if misalign == 0 { 0 } else { align - misalign };
        size_of::<usize>() + padding
    }
    fn layout(n: usize) -> alloc::Layout {
        let alloc_len = Self::array_offset() + n * size_of::<T>();
        let align = max(align_of::<usize>(), align_of::<T>());
        alloc::Layout::from_size_align(alloc_len, align).unwrap()
    }
    fn array_ptr(&self) -> *mut T {
        unsafe { self.p.as_ptr().add(Self::array_offset()) as *mut T }
    }
    fn len(&self) -> usize {
        unsafe { self.p.cast::<usize>().as_ptr().read() }
    }
}

impl<T: Clone> ThinBoxedSlice<T> {
    pub fn new(s: &[T]) -> Self {
        let layout = Self::layout(s.len());
        unsafe {
            let p = NonNull::new(alloc::alloc(layout)).unwrap();
            let ret = Self {
                p,
                phantom: PhantomData,
            };

            p.cast::<usize>().as_ptr().write(s.len());
            let mut v = ret.array_ptr();

            for item in s.iter().cloned() {
                v.write(item);
                v = v.add(1);
            }
            ret
        }
    }
}

impl<T> Drop for ThinBoxedSlice<T> {
    fn drop(&mut self) {
        unsafe {
            alloc::dealloc(self.p.as_ptr(), Self::layout(self.len()));
        }
    }
}

impl<T: Clone> From<&[T]> for ThinBoxedSlice<T> {
    fn from(value: &[T]) -> Self {
        Self::new(value)
    }
}

impl<T: Clone, const N: usize> From<&[T; N]> for ThinBoxedSlice<T> {
    fn from(value: &[T; N]) -> Self {
        Self::from(value.as_slice())
    }
}

impl<T> Deref for ThinBoxedSlice<T> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        unsafe { slice::from_raw_parts(self.array_ptr(), self.len()) }
    }
}

impl<T> DerefMut for ThinBoxedSlice<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { slice::from_raw_parts_mut(self.array_ptr(), self.len()) }
    }
}

impl<T> Borrow<[T]> for ThinBoxedSlice<T> {
    fn borrow(&self) -> &[T] {
        self.deref()
    }
}

impl<T: PartialEq> PartialEq for ThinBoxedSlice<T> {
    fn eq(&self, other: &Self) -> bool {
        self.deref() == other.deref()
    }
}

impl<T: PartialEq> Eq for ThinBoxedSlice<T> {
    fn assert_receiver_is_total_eq(&self) {}
}

impl<T: Hash> Hash for ThinBoxedSlice<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.deref().hash(state);
    }
}
impl<T: Clone> Clone for ThinBoxedSlice<T> {
    fn clone(&self) -> Self {
        self.deref().into()
    }
}
