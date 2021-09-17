use super::Value;
use crate::vm::value::access::Reference;
use crate::vm::value::equality::SchemeEqual;

pub type Vector = Vec<Reference<Value>>;

impl SchemeEqual<Vector> for Vector {
    fn is_eq(&self, other: &Vector) -> bool {
        if self.len() != other.len() {
            return false;
        } else {
            self.iter().zip(other.iter()).all(|(a, b)| a.is_eq(b))
        }
    }

    fn is_eqv(&self, other: &Vector) -> bool {
        if self.len() != other.len() {
            return false;
        } else {
            self.iter().zip(other.iter()).all(|(a, b)| a.is_eqv(b))
        }
    }

    fn is_equal(&self, other: &Vector) -> bool {
        if self.len() != other.len() {
            return false;
        } else {
            self.iter().zip(other.iter()).all(|(a, b)| a.is_equal(b))
        }
    }
}
