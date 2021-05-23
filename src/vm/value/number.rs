use super::equality::SchemeEqual;
use num::BigInt;
use num::BigRational;

#[derive(Debug, PartialEq, Clone)]
pub enum Number {
    Real(RealNumber),
    // Complex
}

#[derive(Debug, PartialEq, Clone)]
pub enum RealNumber {
    Fixnum(BigInt),
    Flonum(f64),
    //    Rational(BigRational),
}

impl<I: Into<BigInt>> From<I> for RealNumber {
    fn from(n: I) -> RealNumber {
        RealNumber::Fixnum(n.into())
    }
}

impl SchemeEqual<RealNumber> for RealNumber {
    fn is_eq(&self, other: &RealNumber) -> bool {
        match (self, other) {
            (RealNumber::Fixnum(lhs), RealNumber::Fixnum(rhs)) => lhs == rhs,
            _ => false,
        }
    }

    fn is_eqv(&self, other: &RealNumber) -> bool {
        self.is_eq(other)
    }

    fn is_equal(&self, other: &RealNumber) -> bool {
        self.is_eq(other)
    }
}

impl SchemeEqual<Number> for Number {
    fn is_eq(&self, other: &Number) -> bool {
        match (self, other) {
            (Number::Real(lhs), Number::Real(rhs)) => lhs.is_eq(rhs),
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
