use crate::vm::value::equality::SchemeEqual;

pub type ByteVector = Vec<u8>;

impl SchemeEqual<ByteVector> for ByteVector {
    fn is_eq(&self, other: &ByteVector) -> bool {
        self.is_equal(other)
    }

    fn is_eqv(&self, other: &ByteVector) -> bool {
        self.is_equal(other)
    }

    fn is_equal(&self, other: &ByteVector) -> bool {
        if self.len() != other.len() {
            return false;
        } else {
            self.iter().zip(other.iter()).all(|(a, b)| a == b)
        }
    }
}
