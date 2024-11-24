use std::{marker::PhantomData, mem};

pub(crate) type PhantomInvariant<'a> = PhantomData<fn(&'a ()) -> &'a ()>;

mod sealed {
    use super::{
        Branded, BrandedImpl, CheckIndex, CheckIndexMut, Freeze, HasInvariantLifetime, Yes,
    };

    pub trait TypeEqSealed<T> {}
    impl<T> TypeEqSealed<T> for T {}
    pub trait BrandedSealed<'brand>: Branded + Freeze + HasInvariantLifetime<'brand> {}
    impl<'a, T> BrandedSealed<'a> for T where T: Branded + Freeze + HasInvariantLifetime<'a> {}
    pub trait IndexCheckMonotonic {}
    impl IndexCheckMonotonic for Yes {}
    pub trait BlessSealed<'a, T: BrandedImpl<'a>>: CheckIndex<'a, T> {}
    impl<'a, T, U> BlessSealed<'a, U> for T
    where
        T: CheckIndex<'a, U>,
        U: BrandedImpl<'a>,
    {
    }
    pub trait BlessMutSealed<'a, T: BrandedImpl<'a>>: CheckIndexMut<'a, T> {}
    impl<'a, T, U> BlessMutSealed<'a, U> for T
    where
        T: CheckIndexMut<'a, U>,
        U: BrandedImpl<'a>,
    {
    }
}
use sealed::{BlessMutSealed, BlessSealed, BrandedSealed, IndexCheckMonotonic, TypeEqSealed};

/// Marker trait for non-interior mutable types
/// # Safety
/// `Self` **must not** be interior-mutable
pub unsafe trait Freeze {}

// Safety: Copy types cannot be interior mutable
unsafe impl<T: Copy> Freeze for T {}

/// Asserts that two types are equal and provides methods to unify them
pub trait TypeEq<T>: TypeEqSealed<T> {
    fn this(self) -> T;
    fn this_ref(&self) -> &T;
    fn this_mut(&mut self) -> &mut T;
}
impl<T> TypeEq<T> for T {
    #[inline(always)]
    fn this(self) -> T {
        self
    }
    #[inline(always)]
    fn this_ref(&self) -> &T {
        self
    }
    #[inline(always)]
    fn this_mut(&mut self) -> &mut T {
        self
    }
}

/// # Safety
/// * the source of `'lt` must be **invariant**
pub unsafe trait HasInvariantLifetime<'lt>: TypeEq<Self::Downcast<'lt>> {
    type Downcast<'downcast>;
}

pub trait Branded: Sized {}
pub trait BrandedImpl<'brand>: BrandedSealed<'brand> {
    unsafe fn rebrand<'new>(self) -> Self::Downcast<'new> {
        unsafe { mem::transmute::<Self::Downcast<'brand>, Self::Downcast<'new>>(self.this()) }
    }
    fn unbrand(self) -> Self::Downcast<'static> {
        unsafe { self.rebrand::<'static>() }
    }
    unsafe fn rebrand_ref<'new>(&self) -> &Self::Downcast<'new> {
        unsafe { mem::transmute::<&Self::Downcast<'brand>, &Self::Downcast<'new>>(self.this_ref()) }
    }
    fn unbrand_ref(&self) -> &Self::Downcast<'static> {
        unsafe { self.rebrand_ref() }
    }
    unsafe fn rebrand_mut<'new>(&mut self) -> &mut Self::Downcast<'new> {
        unsafe {
            mem::transmute::<&mut Self::Downcast<'brand>, &mut Self::Downcast<'new>>(
                self.this_mut(),
            )
        }
    }
}
impl<'a, T> BrandedImpl<'a> for T where T: BrandedSealed<'a> {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct BrandedToken<'brand>(PhantomInvariant<'brand>);
pub(crate) trait BrandSource<'_brand>: BrandedImpl<'_brand> {
    fn with_tok<Func, Ret>(&self, f: Func) -> Ret
    where
        Func: for<'brand> FnOnce(&Self::Downcast<'brand>, BrandedToken<'brand>) -> Ret,
        // disallow nested with_tok calls without brand erasure
        '_brand: 'static,
    {
        f(self.this_ref(), BrandedToken(PhantomData))
    }
    fn with_tok_mut<Func, Ret>(&mut self, f: Func) -> Ret
    where
        Func: for<'brand> FnOnce(&Self::Downcast<'brand>, BrandedToken<'brand>) -> Ret,
        '_brand: 'static,
    {
        f(self.this_mut(), BrandedToken(PhantomData))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CheckedIndex<'brand, T>(T, BrandedToken<'brand>);
pub trait BrandedCollection<'a, Item: BrandedImpl<'a>>: BrandSource<'a> {
    type Index: Freeze;
    /// # Safety
    /// must not cause UB for valid indices
    unsafe fn get_unchecked_index(&self, idx: Self::Index) -> &Item;
    /// # Safety
    /// must not cause UB for valid indices
    unsafe fn get_unchecked_mut_index(&mut self, idx: Self::Index) -> &mut Item;
    #[inline]
    fn get(&self, idx: Self::Index) -> Option<&Item>
    where
        Self: Bless<'a, Item>,
    {
        // Safety: index is checked prior to usage
        self.check_index(idx).map(
            #[inline]
            |idx| unsafe { self.get_unchecked_index(idx) },
        )
    }
    #[inline]
    fn get_mut(&mut self, idx: Self::Index) -> Option<&mut Item>
    where
        Self: BlessMut<'a, Item>,
    {
        // Safety: index is checked prior to usage
        self.check_index_mut(idx).map(
            #[inline]
            |idx| unsafe { self.get_unchecked_mut_index(idx) },
        )
    }
    fn get_unchecked(&mut self, idx: CheckedIndex<'a, Self::Index>) -> &Item
    // require that Self is actually capable of `bless`ing this index
    where
        Self: Bless<'a, Item>,
    {
        // Safety: construction invariant of CheckedIndex requires that this index has been checked before and is not yet invalid
        unsafe { self.get_unchecked_index(idx.0) }
    }
    fn get_unchecked_mut(&mut self, idx: CheckedIndex<'a, Self::Index>) -> &mut Item
    // require that Self is actually capable of `bless`ing this index
    where
        Self: BlessMut<'a, Item>,
    {
        // Safety: construction invariant of CheckedIndex requires that this index has been checked before and is not yet invalid
        unsafe { self.get_unchecked_mut_index(idx.0) }
    }
}

/// # Safety
/// Implementations of this trait must ensure that if `check_index_mut` returns `Some(i)`, calling `self.get_unchecked`
/// or `self.get_unchecked_mut` with `i` will not cause UB for the duration of `'brand`
pub unsafe trait CheckIndexMut<'brand, T: BrandedImpl<'brand>>:
    BrandedCollection<'brand, T>
{
    fn check_index_mut(&mut self, idx: Self::Index) -> Option<Self::Index>;
}
unsafe impl<'brand, U: BrandedImpl<'brand>, T: CheckIndex<'brand, U>> CheckIndexMut<'brand, U> for T
where
    T::MutableMonotonic: IndexCheckMonotonic,
{
    fn check_index_mut(&mut self, idx: Self::Index) -> Option<Self::Index> {
        self.check_index(idx)
    }
}
/// # Safety
/// Implementations of this trait must ensure that if `check_index` returns `Some(i)`, calling `self.get_unchecked`
/// or `self.get_unchecked_mut` with `i` will not cause UB for the duration of `'brand`
pub unsafe trait CheckIndex<'brand, T: BrandedImpl<'brand>>:
    BrandedCollection<'brand, T>
{
    /// Set to [`Yes`] if indices checked by check_index are guaranteed to be monotonic under mutation
    /// (i.e. any possible mutation of the collection will not invalidate previously checked indices)
    /// Note that this forms part of the `unsafe` guarantees made by implementations of this trait
    type MutableMonotonic;
    /// check that an `Index` is valid for the duration of 'brand, returning `Some(idx)` or `None` if it is not valid
    fn check_index(&self, idx: Self::Index) -> Option<Self::Index>;
}
pub struct Yes;
pub struct No;
pub trait Bless<'brand, T: BrandedImpl<'brand>>: BlessSealed<'brand, T> {
    #[inline]
    fn bless(
        &self,
        tok: BrandedToken<'brand>,
        idx: Self::Index,
    ) -> Option<CheckedIndex<'brand, Self::Index>> {
        self.check_index(idx).map(|idx| CheckedIndex(idx, tok))
    }
}
impl<'brand, T, U> Bless<'brand, U> for T
where
    T: BlessSealed<'brand, U>,
    U: BrandedImpl<'brand>,
{
}
pub trait BlessMut<'brand, T: BrandedImpl<'brand>>: BlessMutSealed<'brand, T> {
    #[inline]
    fn bless_mut(
        &mut self,
        tok: BrandedToken<'brand>,
        idx: Self::Index,
    ) -> Option<CheckedIndex<'brand, Self::Index>> {
        self.check_index_mut(idx).map(|idx| CheckedIndex(idx, tok))
    }
}
impl<'brand, T, U> BlessMut<'brand, U> for T
where
    T: BlessMutSealed<'brand, U>,
    U: BrandedImpl<'brand>,
{
}
