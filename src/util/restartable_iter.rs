pub(crate) struct RestartableIter<T: Iterator + Sized> {
    inner: T,
    store: Vec<T::Item>,
}
pub(crate) trait RestartableIterExt: Sized + Iterator {
    fn restartable(self) -> RestartableIter<Self>;
}
impl<T: Sized + Iterator> RestartableIterExt for T {
    fn restartable(self) -> RestartableIter<Self> {
        let hint_lower = self.size_hint().0;
        RestartableIter {
            inner: self,
            store: Vec::with_capacity(hint_lower / 2),
        }
    }
}
impl<T: Iterator> Iterator for RestartableIter<T>
where
    T::Item: Clone,
{
    type Item = T::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.inner.next();
        if let Some(item) = next.clone() {
            self.store.push(item);
        }
        next
    }
}
impl<T: Iterator> RestartableIter<T> {
    pub(crate) fn restart(self) -> impl Iterator<Item = T::Item> {
        self.store.into_iter().chain(self.inner)
    }
}
