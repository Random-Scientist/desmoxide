use std::{marker::PhantomData, mem, ops::Index};

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
    pub trait BlessSealed<'a, T>: CheckIndex<'a, T> {}
    impl<'a, T, U> BlessSealed<'a, U> for T where T: CheckIndex<'a, U> {}
    pub trait BlessMutSealed<'a, T>: CheckIndexMut<'a, T> {}
    impl<'a, T, U> BlessMutSealed<'a, U> for T where T: CheckIndexMut<'a, U> {}
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
pub trait BrandSource<'_brand>: BrandedImpl<'_brand> {
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
pub trait BrandedCollection<'a, Item>: BrandedImpl<'a> {
    type Index: Freeze;
    /// # Safety
    /// must not cause UB for valid indices
    unsafe fn get_unchecked_index(&self, idx: Self::Index) -> &Item;
    /// # Safety
    /// must not cause UB for valid indices
    unsafe fn get_unchecked_mut_index(&mut self, idx: Self::Index) -> &mut Item;
    /// # Safety
    /// * must never cause UB for values that have been checked
    /// * returned index must be valid for `get_unchecked_*`
    unsafe fn insert_unchecked_value(&mut self, val: Item) -> Self::Index;
    #[inline]
    fn get(&self, idx: Self::Index) -> Option<&Item>
    where
        Self: BlessIndex<'a, Item>,
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
        Self: BlessIndexMut<'a, Item>,
    {
        // Safety: index is checked prior to usage
        self.check_index_mut(idx).map(
            #[inline]
            |idx| unsafe { self.get_unchecked_mut_index(idx) },
        )
    }
    fn get_unchecked(&self, idx: CheckedIndex<'a, Self::Index>) -> &Item
    // require that Self is actually capable of `bless`ing this index
    where
        Self: BlessIndex<'a, Item>,
    {
        // Safety: construction invariant of CheckedIndex requires that this index has been checked before and is not yet invalid
        unsafe { self.get_unchecked_index(idx.0) }
    }
    fn get_unchecked_mut(&mut self, idx: CheckedIndex<'a, Self::Index>) -> &mut Item
    // require that Self is actually capable of `bless`ing this index
    where
        Self: BlessIndexMut<'a, Item>,
    {
        // Safety: construction invariant of CheckedIndex requires that this index has been checked before and is not yet invalid
        unsafe { self.get_unchecked_mut_index(idx.0) }
    }
    #[inline]
    fn try_insert(&mut self, val: Item) -> Option<Self::Index>
    where
        Item: CheckableItem<'static, Self::Downcast<'static>>,
        Self::Downcast<'static>: BrandedCollection<'static, Item>,
    {
        // Safety: checking is done unconditionally here so we can safely temporarily unbrand Self to 'static
        val.check(unsafe { self.rebrand_mut() })
            .map(|v| unsafe { self.insert_unchecked_value(v) })
    }
}
impl<'a, T> BrandSource<'a> for T where T: BrandedImpl<'a> {}

/// # Safety
/// Implementations of this trait must ensure that if `check_index_mut` returns `Some(i)`, calling `self.get_unchecked`
/// or `self.get_unchecked_mut` with `i` will not cause UB for the duration of `'brand`
pub unsafe trait CheckIndexMut<'brand, T>: BrandedCollection<'brand, T> {
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
pub unsafe trait CheckIndex<'brand, T>: BrandedCollection<'brand, T> {
    /// Set to [`Yes`] if indices checked by check_index are guaranteed to be monotonic under mutation
    /// (i.e. any possible mutation of the collection will not invalidate previously checked indices)
    /// Note that this forms part of the `unsafe` guarantees made by implementations of this trait
    type MutableMonotonic;
    /// check that an `Index` is valid for the duration of 'brand, returning `Some(idx)` or `None` if it is not valid
    fn check_index(&self, idx: Self::Index) -> Option<Self::Index>;
}
pub struct Yes;
pub struct No;
pub trait BlessIndex<'brand, T>: BlessSealed<'brand, T> {
    #[inline]
    fn bless(
        &self,
        idx: Self::Index,
        tok: BrandedToken<'brand>,
    ) -> Option<CheckedIndex<'brand, Self::Index>> {
        self.check_index(idx).map(|idx| CheckedIndex(idx, tok))
    }
    #[inline]
    fn bless_branded(
        &self,
        idx: Self::Index,
        tok: BrandedToken<'brand>,
    ) -> CheckedIndex<'brand, <Self::Index as HasInvariantLifetime<'brand>>::Downcast<'static>>
    where
        Self::Index: BrandedImpl<'brand>,
    {
        CheckedIndex(idx.unbrand(), tok)
    }
}
impl<'brand, T, U> BlessIndex<'brand, U> for T where T: BlessSealed<'brand, U> {}
pub trait BlessIndexMut<'brand, T>: BlessMutSealed<'brand, T> {
    #[inline]
    fn bless_mut(
        &mut self,
        idx: Self::Index,
        tok: BrandedToken<'brand>,
    ) -> Option<CheckedIndex<'brand, Self::Index>> {
        self.check_index_mut(idx).map(|idx| CheckedIndex(idx, tok))
    }
}
impl<'brand, T, U> BlessIndexMut<'brand, U> for T where T: BlessMutSealed<'brand, U> {}
/// # Safety
/// implementations must ensure that:
///
/// * If a type `T` implements [`BrandedCollection<'_, Self>`] `where T::Index = Self::Index`, then **all** values of type `T::Index` contained in this
/// instance of `Self` must be present in the slice provided to the callback in [`with_children`](CheckByChildIndices::with_children)
/// * index validation is the only invariant required for `Self` to be valid for insertion to `T`
pub unsafe trait CheckByChildIndices<'brand>: BrandedImpl<'brand> {
    type Index: Freeze;
    fn with_children<R, T: FnOnce(&[Self::Index]) -> R>(&self, f: T) -> Option<R>;
}
/// # Safety
/// returning `Some(v)` from [`check`](CheckItem::check) unconditionally asserts that `v` is valid for insertion into this collection
pub unsafe trait CheckableItem<'brand, Parent: BrandedCollection<'brand, Self>>:
    BrandedImpl<'brand>
{
    fn check(self, parent_collection: &mut Parent) -> Option<Self>;
}
// Safety: invariants on CheckByChildIndices
unsafe impl<'brand, T, U, I: Freeze + Copy> CheckableItem<'brand, T> for U
where
    T: BrandedCollection<'brand, Self, Index = I> + BlessIndexMut<'brand, Self>,
    Self: CheckByChildIndices<'brand, Index = I>,
{
    fn check(self, parent_collection: &mut T) -> Option<Self> {
        if self
            .with_children(
                #[inline]
                |children| {
                    children
                        .iter()
                        .copied()
                        .any(|v| parent_collection.check_index_mut(v).is_none())
                },
            )
            .is_some_and(unit)
        {
            None
        } else {
            Some(self)
        }
    }
}

#[inline]
fn unit<T>(v: T) -> T {
    v
}

#[cfg(test)]
mod tests {
    use std::marker::PhantomData;

    use crate::util::branded::BlessIndex;

    use super::{
        BrandSource, Branded, BrandedCollection, CheckIndex, Freeze, HasInvariantLifetime, No,
        PhantomInvariant,
    };

    struct Test<'a>(Vec<u32>, PhantomInvariant<'a>);
    unsafe impl Freeze for Test<'_> {}
    unsafe impl<'a> HasInvariantLifetime<'a> for Test<'a> {
        type Downcast<'downcast> = Test<'downcast>;
    }
    // "turns on" the sealed blanket impls
    impl Branded for Test<'_> {}
    impl<'a> BrandedCollection<'a, u32> for Test<'a> {
        type Index = usize;

        unsafe fn get_unchecked_index(&self, idx: Self::Index) -> &u32 {
            unsafe { self.0.get_unchecked(idx) }
        }

        unsafe fn get_unchecked_mut_index(&mut self, idx: Self::Index) -> &mut u32 {
            unsafe { self.0.get_unchecked_mut(idx) }
        }

        unsafe fn insert_unchecked_value(&mut self, val: u32) -> Self::Index {
            let i = self.0.len();
            self.0.push(val);
            i
        }
    }
    unsafe impl<'a> CheckIndex<'a, u32> for Test<'a> {
        type MutableMonotonic = No;
        fn check_index(&self, idx: Self::Index) -> Option<Self::Index> {
            if idx < self.0.len() {
                Some(idx)
            } else {
                None
            }
        }
    }
    #[test]
    fn using_test() {
        let v = Test(vec![1, 2, 3, 4, 8, 10], PhantomData);
        v.with_tok(|v, tok| {
            assert!(v.bless(10, tok).is_none());
            let Some(checked) = v.bless(3, tok) else {
                panic!("the world is ending")
            };
            // Look ma, no bounds-checking
            dbg!(v.get_unchecked(checked));
        });
    }
}
