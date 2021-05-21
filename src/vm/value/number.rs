use super::equality::SchemeEqual;
use num::BigInt;

#[derive(Debug, PartialEq, Clone)]
pub enum Number {
    FixNum(i64),
    Integer(BigInt),
}
impl SchemeEqual<Number> for Number {
    fn is_eq(&self, other: &Number) -> bool {
        match (self, other) {
            (Number::FixNum(lhs), Number::FixNum(rhs)) => lhs == rhs,
            (Number::Integer(lhs), Number::Integer(rhs)) => lhs == rhs,
            _ => false,
        }
    }

    fn is_eqv(&self, other: &Number) -> bool {
        self.is_eq(other)
    }

    fn is_equal(&self, other: &Number) -> bool {
        self.is_eq(other)
    }
}
