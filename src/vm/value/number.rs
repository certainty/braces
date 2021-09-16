use super::equality::SchemeEqual;
use super::error::{self, RuntimeError};
use rug::Integer;
pub mod fixnum;
pub mod flonum;
pub mod rational;
pub mod real;
use az::CheckedAs;
use std::ops::{Add, Div, Mul, Sub};

type ArithResult<T> = std::result::Result<T, RuntimeError>;

#[derive(Debug, PartialEq, Clone)]
pub enum Exactness {
    Inexact,
    Exact,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Sign {
    Plus,
    Minus,
}

pub trait SchemeNumber {
    fn sign(self, sign: Sign) -> Self;
    fn is_exact(&self) -> bool;
    fn is_inexact(&self) -> bool;
    fn is_rational(&self) -> bool;
    fn is_integer(&self) -> bool;
    fn is_real(&self) -> bool;
    fn is_complex(&self) -> bool;
    fn is_finite(&self) -> bool;
    fn is_infinite(&self) -> bool;
    fn is_neg_infinite(&self) -> bool;
    fn is_nan(&self) -> bool;
}

pub trait SchemeNumberExactness {
    fn to_inexact(self) -> flonum::Flonum;
    fn to_exact(self) -> Option<Number>;
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Number {
    Real(real::RealNumber),
    // Complex
}

impl Number {
    // Value constructors
    pub fn inifinity() -> Self {
        Self::Real(real::RealNumber::Flonum(flonum::Flonum::infinity()))
    }

    pub fn neg_inifinity() -> Self {
        Self::Real(real::RealNumber::Flonum(flonum::Flonum::neg_infinity()))
    }

    pub fn nan() -> Self {
        Self::Real(real::RealNumber::Flonum(flonum::Flonum::nan()))
    }

    pub fn flonum<I: Into<flonum::Flonum>>(num: I) -> Number {
        Self::Real(real::RealNumber::Flonum(num.into()))
    }

    pub fn fixnum<I: Into<fixnum::Fixnum>>(num: I) -> Number {
        Self::Real(real::RealNumber::Fixnum(num.into()))
    }

    pub fn fraction<N: Into<Integer>, D: Into<Integer>>(numer: N, denom: D) -> Number {
        Self::Real(real::RealNumber::Rational(rational::Rational::from((
            numer.into(),
            denom.into(),
        ))))
    }

    pub fn to_u8(&self) -> Option<u8> {
        match self {
            Self::Real(r) => r
                .clone()
                .checked_as::<fixnum::Fixnum>()
                .and_then(|fx| fx.as_inner().to_u8()),
        }
    }

    pub fn to_usize(&self) -> Option<usize> {
        match self {
            Self::Real(r) => r
                .clone()
                .checked_as::<fixnum::Fixnum>()
                .and_then(|fx| fx.as_inner().to_usize()),
        }
    }
}

impl ToString for Number {
    fn to_string(&self) -> String {
        match self {
            Number::Real(r) => r.to_string(),
        }
    }
}

// conversions and casts

impl<I: Into<real::RealNumber>> From<I> for Number {
    fn from(n: I) -> Number {
        Number::Real(n.into())
    }
}

impl SchemeEqual<Number> for Number {
    fn is_eq(&self, other: &Number) -> bool {
        match (self, other) {
            (Number::Real(lhs), Number::Real(rhs)) => lhs.is_eq(rhs),
        }
    }

    fn is_eqv(&self, other: &Number) -> bool {
        self.is_eq(other)
    }

    fn is_equal(&self, other: &Number) -> bool {
        self.is_eq(other)
    }
}

impl SchemeNumber for Number {
    fn sign(self, sign: Sign) -> Self {
        match self {
            Self::Real(n) => Self::Real(n.sign(sign)),
        }
    }

    fn is_exact(&self) -> bool {
        match self {
            Self::Real(n) => n.is_exact(),
        }
    }
    fn is_inexact(&self) -> bool {
        !self.is_exact()
    }

    fn is_rational(&self) -> bool {
        match self {
            Self::Real(n) => n.is_rational(),
        }
    }
    fn is_integer(&self) -> bool {
        match self {
            Self::Real(n) => n.is_integer(),
        }
    }
    fn is_real(&self) -> bool {
        match self {
            Self::Real(n) => n.is_real(),
        }
    }

    fn is_complex(&self) -> bool {
        false
    }

    fn is_finite(&self) -> bool {
        match self {
            Self::Real(n) => n.is_finite(),
        }
    }

    fn is_infinite(&self) -> bool {
        match self {
            Self::Real(n) => n.is_infinite(),
        }
    }

    fn is_neg_infinite(&self) -> bool {
        match self {
            Self::Real(n) => n.is_neg_infinite(),
        }
    }
    fn is_nan(&self) -> bool {
        match self {
            Self::Real(n) => n.is_nan(),
        }
    }
}
// make lifting scheme numbers to rug numbers easier

// implement arithmetic
impl Add<Number> for Number {
    type Output = ArithResult<Number>;

    fn add(self, rhs: Number) -> Self::Output {
        match (self, rhs) {
            (Number::Real(lhs), Number::Real(rhs)) => Ok(Number::Real(lhs.add(rhs)?)),
        }
    }
}

impl Sub<Number> for Number {
    type Output = ArithResult<Number>;

    fn sub(self, rhs: Number) -> Self::Output {
        match (self, rhs) {
            (Number::Real(lhs), Number::Real(rhs)) => Ok(Number::Real(lhs.sub(rhs)?)),
        }
    }
}

impl Div<Number> for Number {
    type Output = ArithResult<Number>;

    fn div(self, rhs: Number) -> Self::Output {
        match (self, rhs) {
            (Number::Real(lhs), Number::Real(rhs)) => Ok(Number::Real(lhs.div(rhs)?)),
        }
    }
}

impl Mul<Number> for Number {
    type Output = ArithResult<Number>;

    fn mul(self, rhs: Number) -> Self::Output {
        match (self, rhs) {
            (Number::Real(lhs), Number::Real(rhs)) => Ok(Number::Real(lhs.mul(rhs)?)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn all_is_real() {
        assert!(Number::fixnum(1).is_real());
        assert!(Number::fraction(1, 2).is_real());
    }

    #[test]
    fn is_rational() {
        assert!(!Number::fixnum(1).is_rational());
        assert!(Number::fraction(1, 2).is_rational());
    }

    #[test]
    fn test_oddities() {
        assert!(Number::nan().is_nan());
        assert!(Number::inifinity().is_infinite());
        assert!(!Number::inifinity().is_finite());
        assert!(Number::neg_inifinity().is_infinite());
        assert!(!Number::neg_inifinity().is_finite());
    }

    #[test]
    fn nan_is_not_rational() {
        assert!(
            !Number::nan().is_rational(),
            "Expected nan not to be rational"
        )
    }

    #[test]
    fn inf_is_not_rational() {
        assert!(
            !Number::inifinity().is_rational(),
            "Expected infinity not to be rational"
        );

        assert!(
            !Number::neg_inifinity().is_rational(),
            "Expected -infinity not to be rational"
        )
    }
}
