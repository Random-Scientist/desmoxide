use core::borrow::Borrow;
use core::cmp::max;
use core::hash::{Hash, Hasher};
use core::marker::PhantomData;
use core::mem::{align_of, size_of};
use core::ops::{Deref, DerefMut};
use core::ptr::NonNull;
use core::slice;

use allocator_api2::alloc::{self, Allocator, Global};

// from: https://github.com/seekstar/thin-boxed-slice/blob/main/src/lib.rs (with some cleanup and minor modifications)

#[derive(Debug)]
pub struct ThinBoxedSlice<T, A: Allocator = Global> {
    p: NonNull<u8>,
    allocator: A,
    phantom: PhantomData<T>,
}

impl<T, A: Allocator> ThinBoxedSlice<T, A> {
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

impl<T: Clone, A: Allocator> ThinBoxedSlice<T, A> {
    pub fn new_in(s: &[T], allocator: A) -> Self {
        let layout = Self::layout(s.len());
        unsafe {
            let p = allocator.allocate(layout).unwrap().cast::<u8>();
            let ret = Self {
                p,
                allocator,
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

impl<T, A: Allocator> Drop for ThinBoxedSlice<T, A> {
    fn drop(&mut self) {
        unsafe {
            self.allocator.deallocate(self.p, Self::layout(self.len()));
        }
    }
}

impl<T: Clone, A: Allocator + Default> From<&[T]> for ThinBoxedSlice<T, A> {
    fn from(value: &[T]) -> Self {
        Self::new_in(value, A::default())
    }
}

impl<T: Clone, A: Allocator + Default, const N: usize> From<&[T; N]> for ThinBoxedSlice<T, A> {
    fn from(value: &[T; N]) -> Self {
        Self::from(value.as_slice())
    }
}

impl<T, A: Allocator> Deref for ThinBoxedSlice<T, A> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        unsafe { slice::from_raw_parts(self.array_ptr(), self.len()) }
    }
}

impl<T, A: Allocator> DerefMut for ThinBoxedSlice<T, A> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { slice::from_raw_parts_mut(self.array_ptr(), self.len()) }
    }
}

impl<T, A: Allocator> Borrow<[T]> for ThinBoxedSlice<T, A> {
    fn borrow(&self) -> &[T] {
        self.deref()
    }
}

impl<T: PartialEq, A: Allocator> PartialEq for ThinBoxedSlice<T, A> {
    fn eq(&self, other: &Self) -> bool {
        self.deref() == other.deref()
    }
}

impl<T: PartialEq, A: Allocator> Eq for ThinBoxedSlice<T, A> {
    fn assert_receiver_is_total_eq(&self) {}
}

impl<T: Hash, A: Allocator> Hash for ThinBoxedSlice<T, A> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.deref().hash(state);
    }
}
impl<T: Clone, A: Allocator + Default> Clone for ThinBoxedSlice<T, A> {
    fn clone(&self) -> Self {
        self.deref().into()
    }
}
