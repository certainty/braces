use super::equality::SchemeEqual;
use super::error::{self, RuntimeError};
use rug::Integer;
pub mod fixnum;
pub mod flonum;
pub mod rational;
pub mod real;

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

type ArithResult<T> = std::result::Result<T, RuntimeError>;

pub trait SchemeNumberArith {
    fn abs(&self) -> ArithResult<Number>;
    fn neg(&self) -> ArithResult<Number>;
    fn plus(&self, other: Number) -> ArithResult<Number>;
    fn minus(&self, other: Number) -> ArithResult<Number>;
    fn mul(&self, other: Number) -> ArithResult<Number>;
    fn div(&self, other: Number) -> ArithResult<Number>;
    fn modulo(&self, other: Number) -> ArithResult<Number>;
}

pub trait SchemeNumberExactness {
    fn to_inexact(&self) -> ArithResult<flonum::Flonum>;
    fn to_exact(&self) -> ArithResult<Number>;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Number {
    Real(real::RealNumber),
    // Complex
}

impl Number {
    /*
    pub fn cmp(&self, other: Self) -> Ordering {
        match (self, other) {
            (Self::Real(lhs), Self::Real(rhs)) => lhs.cmp(rhs),
        }
    }*/

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

    pub fn big<I: Into<Integer>>(num: I) -> Number {
        Self::Real(num.into().into())
    }

    pub fn fraction<N: Into<Integer>, D: Into<Integer>>(numer: N, denom: D) -> Number {
        Self::Real(real::RealNumber::Rational(rational::Rational::from((
            numer.into(),
            denom.into(),
        ))))
    }

    pub fn i8(num: i8) -> Number {
        Self::Real(num.into())
    }

    pub fn i16(num: i16) -> Number {
        Self::Real(num.into())
    }

    pub fn i32(num: i32) -> Number {
        Self::Real(num.into())
    }

    pub fn i64(num: i64) -> Number {
        Self::Real(num.into())
    }

    pub fn f32(num: f32) -> Number {
        Self::Real(real::RealNumber::Flonum(num.into()))
    }

    pub fn f64(num: f64) -> Number {
        Self::Real(real::RealNumber::Flonum(num.into()))
    }

    // Coerce two numbers so that they can be applied to the same operation
    // We apply the following coercion rules because they are simple (not necessarily performant)
    //
    // 1. If `lhs` and `rhs` are both integers of the same type, just return both
    // 2. If `lhs` and `rhs` are both integers of different types, then the smaller type will be cast to the bigger type
    // 3. If `lhs` and `rhs` are both floats of the same type, just return both
    // 4. If `lhs` and `rhs` are both floats of different types, then the smaller type will be cast to the bigger
    // 5. If `lhs` and `rhs` are both rational, just return both
    // 6. If `lhs` and `rhs` are of types integer and float, the integer will be cast to a float. Should the integer be too big for the float, an error will be raised.
    // 7. If `lhs` and `rhs` are of types integer and rational, the integer will be converted to a rational.
    // 8. If `lhs` and `rhs` are of types float and rational, the float will be converted to a rational.
    pub fn coerce(lhs: Number, rhs: Number) -> ArithResult<(Number, Number)> {
        match (lhs, rhs) {
            (
                Number::Real(real::RealNumber::Fixnum(lhs)),
                Number::Real(real::RealNumber::Fixnum(rhs)),
            ) => {
                let (coerced_lhs, coerced_rhs) = fixnum::Fixnum::coerce(lhs, rhs);

                Ok((
                    Number::Real(real::RealNumber::Fixnum(coerced_lhs)),
                    Number::Real(real::RealNumber::Fixnum(coerced_rhs)),
                ))
            }
            (
                Number::Real(real::RealNumber::Flonum(lhs)),
                Number::Real(real::RealNumber::Flonum(rhs)),
            ) => {
                let (coerced_lhs, coerced_rhs) = flonum::Flonum::coerce(lhs, rhs);

                Ok((
                    Number::Real(real::RealNumber::Flonum(coerced_lhs)),
                    Number::Real(real::RealNumber::Flonum(coerced_rhs)),
                ))
            }
            (
                Number::Real(real::RealNumber::Rational(lhs)),
                Number::Real(real::RealNumber::Rational(rhs)),
            ) => Ok((
                Number::Real(real::RealNumber::Rational(lhs)),
                Number::Real(real::RealNumber::Rational(rhs)),
            )),

            // Fixnum / Flonum
            (
                Number::Real(real::RealNumber::Fixnum(lhs)),
                Number::Real(real::RealNumber::Flonum(rhs)),
            ) => {
                let (coerced_lhs, coerced_rhs) = flonum::Flonum::coerce(lhs.to_inexact()?, rhs);

                Ok((
                    Number::Real(real::RealNumber::Flonum(coerced_lhs)),
                    Number::Real(real::RealNumber::Flonum(coerced_rhs)),
                ))
            }

            (
                Number::Real(real::RealNumber::Flonum(lhs)),
                Number::Real(real::RealNumber::Fixnum(rhs)),
            ) => {
                let (coerced_lhs, coerced_rhs) = flonum::Flonum::coerce(lhs, rhs.to_inexact()?);

                Ok((
                    Number::Real(real::RealNumber::Flonum(coerced_lhs)),
                    Number::Real(real::RealNumber::Flonum(coerced_rhs)),
                ))
            }

            // Fixnum / Rational
            (
                Number::Real(real::RealNumber::Rational(lhs)),
                Number::Real(real::RealNumber::Fixnum(rhs)),
            ) => Ok((
                Number::Real(real::RealNumber::Rational(lhs)),
                Number::Real(real::RealNumber::Rational(rational::Rational::from(
                    rhs.as_big(),
                ))),
            )),

            (
                Number::Real(real::RealNumber::Fixnum(lhs)),
                Number::Real(real::RealNumber::Rational(rhs)),
            ) => Ok((
                Number::Real(real::RealNumber::Rational(rational::Rational::from(
                    lhs.as_big(),
                ))),
                Number::Real(real::RealNumber::Rational(rhs)),
            )),

            // Flonum / Rational
            (
                Number::Real(real::RealNumber::Rational(lhs)),
                Number::Real(real::RealNumber::Flonum(rhs)),
            ) => Ok((
                Number::Real(real::RealNumber::Rational(lhs)),
                rhs.to_exact()?,
            )),

            (
                Number::Real(real::RealNumber::Flonum(lhs)),
                Number::Real(real::RealNumber::Rational(rhs)),
            ) => Ok((
                lhs.to_exact()?,
                Number::Real(real::RealNumber::Rational(rhs)),
            )),
        }
    }
}

impl<I: Into<real::RealNumber>> From<I> for Number {
    fn from(n: I) -> Number {
        Number::Real(n.into())
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

    fn is_nan(&self) -> bool {
        match self {
            Self::Real(n) => n.is_nan(),
        }
    }
    fn is_neg_infinite(&self) -> bool {
        match self {
            Self::Real(n) => n.is_neg_infinite(),
        }
    }
}

// implement SchemeEqual

#[cfg(test)]
mod tests {
    use super::*;

    fn all_is_real() {
        assert!(Number::f64(0.1).is_real());
        assert!(Number::f32(0.1).is_real());
        assert!(Number::i8(1).is_real());
        assert!(Number::i16(1).is_real());
        assert!(Number::i32(1).is_real());
        assert!(Number::i64(1).is_real());
        assert!(Number::big(1).is_real());
        assert!(Number::fraction(1, 2).is_real());
    }

    #[test]
    fn is_rational() {
        assert!(!Number::f64(0.1).is_rational());
        assert!(!Number::f32(0.1).is_rational());
        assert!(!Number::i8(1).is_rational());
        assert!(!Number::i16(1).is_rational());
        assert!(!Number::i32(1).is_rational());
        assert!(!Number::i64(1).is_rational());
        assert!(!Number::big(1).is_rational());
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
