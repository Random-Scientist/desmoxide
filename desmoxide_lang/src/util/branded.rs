use std::{
    marker::PhantomData,
    mem,
    ops::{Deref, Index},
};

pub(crate) type PhantomInvariant<'a> = PhantomData<fn(&'a ()) -> &'a ()>;

mod sealed {
    use super::{CheckIndex, CheckIndexMut, HasBrandLifetime, NotInteriorMutable, Yes};

    pub trait TypeEqSealed<T> {}
    impl<T> TypeEqSealed<T> for T {}
    pub trait BrandedSealed<'brand>: Sized + NotInteriorMutable + HasBrandLifetime<'brand> {}
    impl<'a, T> BrandedSealed<'a> for T where T: Sized + NotInteriorMutable + HasBrandLifetime<'a> {}
    pub trait IndexCheckMonotonic {}
    impl IndexCheckMonotonic for Yes {}
    pub trait BlessSealed<'a, Idx>: CheckIndex<'a, Idx> {}
    impl<'a, T, U> BlessSealed<'a, U> for T where T: CheckIndex<'a, U> {}
    pub trait BlessMutSealed<'a, Idx>: CheckIndexMut<'a, Idx> {}
    impl<'a, T, U> BlessMutSealed<'a, U> for T where T: CheckIndexMut<'a, U> {}
}
use sealed::{BlessMutSealed, BlessSealed, BrandedSealed, IndexCheckMonotonic, TypeEqSealed};

/// # Safety
/// * `Self` **must not** be interior-mutable
pub unsafe trait NotInteriorMutable {}

// Safety: Copy types cannot be interior mutable
unsafe impl<T: Copy> NotInteriorMutable for T {}

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
/// implementation ensures:
/// * the source of `'lt` is **invariant**
/// * the definition of `Self` may **not** contain any fields whose type names [`BrandedToken<'lt>`].
/// * all safe constructors of `Self` must return `Self<'static>` (i.e. it must be impossible to safely construct a `Self` with a non-`'static` brand lifetime)
pub unsafe trait HasBrandLifetime<'lt>: TypeEq<Self::Downcast<'lt>> {
    type Downcast<'downcast>;
}

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
pub trait BrandSource: BrandedImpl<'static> {
    fn with_tok<Func, Ret>(&self, f: Func) -> Ret
    where
        Func: for<'unique> FnOnce(&Self::Downcast<'unique>, BrandedToken<'unique>) -> Ret,
        // disallow nested with_tok calls without brand erasure
    {
        f(self.this_ref(), BrandedToken(PhantomData))
    }
    fn with_tok_mut<Func, Ret>(&mut self, f: Func) -> Ret
    where
        Func: for<'unique> FnOnce(&Self::Downcast<'unique>, BrandedToken<'unique>) -> Ret,
    {
        f(self.this_mut(), BrandedToken(PhantomData))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CheckedIndex<'brand, T>(T, BrandedToken<'brand>);
impl<'brand, 'index, T> CheckedIndex<'brand, T>
where
    T: BrandedImpl<'index>,
{
    fn rebrand_into_inner(self) -> T::Downcast<'brand> {
        // Safety: this is valid due to the construction invariant of T: HasBrandLifetime (implied by the T: BrandedImpl bound).
        // Essentially: if T is only safely constructible with a brand of `'static`, then safely constructing a T<'brand> such that
        // BrandedToken<'brand> exists is not possible, i.e. safely constructing an unchecked index directly is impossible
        unsafe { self.0.rebrand() }
    }
}
pub trait BrandedCollection<'a, Index>: BrandedImpl<'a> {
    type Item;
    /// # Safety
    /// must not cause UB for valid indices
    unsafe fn get_unchecked_index(&self, idx: Index) -> &Self::Item;
    /// # Safety
    /// must not cause UB for valid indices
    unsafe fn get_unchecked_mut_index(&mut self, idx: Index) -> &mut Self::Item;
    /// # Safety
    /// * must never cause UB for values that have been checked
    /// * returned index must be valid for `get_unchecked_*`
    unsafe fn insert_unchecked_value(&mut self, val: Self::Item) -> Index;
    #[inline]
    fn get(&self, idx: Index) -> Option<&Self::Item>
    where
        Self: BlessIndex<'a, Index>,
    {
        // Safety: index is checked prior to usage
        self.check_index(idx).map(
            #[inline]
            |idx| unsafe { self.get_unchecked_index(idx) },
        )
    }
    #[inline]
    fn get_mut(&mut self, idx: Index) -> Option<&mut Self::Item>
    where
        Self: BlessIndexMut<'a, Index>,
    {
        // Safety: index is checked prior to usage
        self.check_index_mut(idx).map(
            #[inline]
            |idx| unsafe { self.get_unchecked_mut_index(idx) },
        )
    }
    fn get_unchecked(&self, idx: CheckedIndex<'a, Index>) -> &Self::Item
    // require that Self is actually capable of `bless`ing this index
    where
        Self: BlessIndex<'a, Index>,
    {
        // Safety: construction invariant of CheckedIndex requires that this index has been checked before and is not yet invalid
        unsafe { self.get_unchecked_index(idx.0) }
    }
    fn get_unchecked_mut(&mut self, idx: CheckedIndex<'a, Index>) -> &mut Self::Item
    // require that Self is actually capable of `bless`ing this index
    where
        Self: BlessIndexMut<'a, Index>,
    {
        // Safety: construction invariant of CheckedIndex requires that this index has been checked before and is not yet invalid
        unsafe { self.get_unchecked_mut_index(idx.0) }
    }
    #[inline]
    fn try_insert(&mut self, val: Self::Item) -> Option<Index>
    where
        Self::Item: CheckableItem<'static, Index, Self::Downcast<'static>>,
        Self::Downcast<'static>: BrandedCollection<'static, Index, Item = Self::Item>,
    {
        // Safety: checking is done unconditionally here so we can safely temporarily unbrand Self to 'static
        val.check(unsafe { self.rebrand_mut() })
            .map(|v| unsafe { self.insert_unchecked_value(v) })
    }
}
impl<T> BrandSource for T where T: BrandedImpl<'static> {}

/// # Safety
/// Implementations of this trait must ensure that if `check_index_mut` returns `Some(i)`, calling `self.get_unchecked`
/// or `self.get_unchecked_mut` with `i` will not cause UB for the duration of `'brand`
pub unsafe trait CheckIndexMut<'brand, Idx>: BrandedCollection<'brand, Idx> {
    fn check_index_mut(&mut self, idx: Idx) -> Option<Idx>;
}
unsafe impl<'brand, Idx, T: CheckIndex<'brand, Idx>> CheckIndexMut<'brand, Idx> for T
where
    T::MutableMonotonic: IndexCheckMonotonic,
{
    fn check_index_mut(&mut self, idx: Idx) -> Option<Idx> {
        self.check_index(idx)
    }
}
/// # Safety
/// Implementations of this trait must ensure that if `check_index` returns `Some(i)`, calling `self.get_unchecked`
/// or `self.get_unchecked_mut` with `i` will not cause UB for the duration of `'brand`
pub unsafe trait CheckIndex<'brand, Idx>: BrandedCollection<'brand, Idx> {
    /// Set to [`Yes`] if indices checked by check_index are guaranteed to be monotonic under mutation
    /// (i.e. any possible mutation of the collection will not invalidate previously checked indices)
    /// Note that this forms part of the `unsafe` guarantees made by implementations of this trait
    type MutableMonotonic;
    /// check that an `Index` is valid for the duration of 'brand, returning `Some(idx)` or `None` if it is not valid
    fn check_index(&self, idx: Idx) -> Option<Idx>;
}
pub struct Yes;
pub struct No;
pub trait BlessIndex<'brand, Index>: BlessSealed<'brand, Index> {
    #[inline]
    fn bless(&self, idx: Index, tok: BrandedToken<'brand>) -> Option<CheckedIndex<'brand, Index>> {
        self.check_index(idx).map(|idx| CheckedIndex(idx, tok))
    }
}
impl<'brand, T, Idx> BlessIndex<'brand, Idx> for T where T: BlessSealed<'brand, Idx> {}
pub trait BlessIndexMut<'brand, Idx>: BlessMutSealed<'brand, Idx> {
    #[inline]
    fn bless_mut(
        &mut self,
        idx: Idx,
        tok: BrandedToken<'brand>,
    ) -> Option<CheckedIndex<'brand, Idx>> {
        self.check_index_mut(idx).map(|idx| CheckedIndex(idx, tok))
    }
}
impl<'brand, T, U> BlessIndexMut<'brand, U> for T where T: BlessMutSealed<'brand, U> {}
/// # Safety
/// implementations must ensure that:
///
/// * If a type `T` implements [`BrandedCollection<'_, Self>`] `where T::Index = Self::Index`, then **all** values of type `T::Index` contained in this
///     instance of `Self` must be present in the slice provided to the callback in [`with_children`](CheckByChildIndices::with_children)
/// * index validation is the only invariant required for `Self` to be valid for insertion to `T`
pub unsafe trait CheckByChildIndices<'brand>: BrandedImpl<'brand> {
    type Index: NotInteriorMutable;
    fn with_children<R, T: FnOnce(&[Self::Index]) -> R>(&self, f: T) -> Option<R>;
}
/// # Safety
/// returning `Some(v)` from [`check`](CheckableItem::check) unconditionally asserts that `v` is valid for insertion into this collection
pub unsafe trait CheckableItem<'brand, Idx, Parent: BrandedCollection<'brand, Idx, Item = Self>>:
    BrandedImpl<'brand>
{
    fn check(self, parent_collection: &mut Parent) -> Option<Self>;
}

unsafe impl<'brand, Val, Collection, Index: NotInteriorMutable + Copy>
    CheckableItem<'brand, Index, Collection> for Val
where
    Val: CheckByChildIndices<'brand, Index = Index>,
    Collection: BrandedCollection<'brand, Index, Item = Val> + CheckIndexMut<'brand, Index>,
{
    fn check(self, parent_collection: &mut Collection) -> Option<Self> {
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
struct LocalCollectionRef<'borrow, T>(&'borrow T);
impl<T> Deref for LocalCollectionRef<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}
impl<'borrow, 'brand, T: BrandedImpl<'brand>> From<&'borrow T> for LocalCollectionRef<'borrow, T> {
    fn from(value: &'borrow T) -> Self {
        Self(value)
    }
}
impl<'brand, Val, Collection, Idx> Index<CheckedIndex<'brand, Idx>>
    for LocalCollectionRef<'_, Collection>
where
    Collection: BrandedCollection<'brand, Idx, Item = Val>,
    Collection: CheckIndex<'brand, Idx>,
    Idx: NotInteriorMutable,
{
    type Output = Val;

    fn index(&self, index: CheckedIndex<'brand, Idx>) -> &Self::Output {
        self.get_unchecked(index)
    }
}

#[inline]
fn unit<T>(v: T) -> T {
    v
}

#[cfg(test)]
mod tests {

    use inner::Test;

    use crate::util::branded::{BlessIndex, BrandedImpl};

    use super::{BrandSource, BrandedCollection};
    mod inner {

        use crate::util::branded::{BrandedCollection, CheckIndex, No, NotInteriorMutable};

        #[desmoxide_derive::inject_brand_lifetime('a)]
        mod defs {
            pub struct Test<'a>(Vec<u32>);
        }
        unsafe impl NotInteriorMutable for BrandedTest<'_> {}
        // "turns on" the sealed blanket impls
        impl<'a> BrandedCollection<'a, usize> for BrandedTest<'a> {
            type Item = u32;

            unsafe fn get_unchecked_index(&self, idx: usize) -> &u32 {
                unsafe { self.0.get_unchecked(idx) }
            }

            unsafe fn get_unchecked_mut_index(&mut self, idx: usize) -> &mut u32 {
                unsafe { self.0.get_unchecked_mut(idx) }
            }

            unsafe fn insert_unchecked_value(&mut self, val: u32) -> usize {
                let i = self.0.len();
                self.0.push(val);
                i
            }
        }
        unsafe impl<'a> CheckIndex<'a, usize> for BrandedTest<'a> {
            type MutableMonotonic = No;
            fn check_index(&self, idx: usize) -> Option<usize> {
                if idx < self.0.len() {
                    Some(idx)
                } else {
                    None
                }
            }
        }
    }

    #[test]
    fn using_test() {
        let v = Test::new(vec![1, 2, 3, 4, 8, 10]);
        v.with_tok(|v, tok| {
            assert!(v.bless(10, tok).is_none());
            let Some(checked) = v.bless(3, tok) else {
                panic!("the world is ending")
            };
            // Look ma, no bounds-checking
            assert_eq!(v.get_unchecked(checked), &4);
            assert_eq!(v.unbrand_ref().get(3), Some(&4));
        });
    }
}
