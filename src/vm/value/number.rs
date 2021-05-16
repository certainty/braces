use super::equality::SchemeEqual;

#[derive(Debug, PartialEq, Clone)]
pub enum Number {
    FixNum(i64),
}
impl SchemeEqual<Number> for Number {
    fn is_eq(&self, other: &Number) -> bool {
        match (self, other) {
            (Number::FixNum(lhs), Number::FixNum(rhs)) => lhs == rhs,
        }
    }

    fn is_eqv(&self, other: &Number) -> bool {
        self.is_eq(other)
    }

    fn is_equal(&self, other: &Number) -> bool {
        self.is_eq(other)
    }
}
