pub trait SchemeEqual<T> {
    fn is_eq(&self, other: &T) -> bool;
    fn is_eqv(&self, other: &T) -> bool;
    fn is_equal(&self, other: &T) -> bool;
}
