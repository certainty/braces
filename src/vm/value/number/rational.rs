use super::error::{self, RuntimeError};
use super::*;
use crate::vm::value::equality::SchemeEqual;
use num::BigRational;

#[repr(transparent)]
#[derive(Debug, Clone, PartialEq)]
pub struct Rational(BigRational);

impl<I: Into<BigRational>> From<I> for Rational {
    fn from(n: I) -> Rational {
        Rational(n.into())
    }
}

impl SchemeNumberExactness for BigRational {
    fn to_inexact(&self) -> ArithResult<flonum::Flonum> {
        // TODO: implement this
        Err(error::arithmetic_error(
            "Can't create inexact value from rational",
        ))
    }

    fn to_exact(&self) -> ArithResult<Number> {
        Ok(Number::Real(RealNumber::Rational(self.clone())))
    }
}

impl SchemeEqual<Rational> for Rational {
    fn is_eq(&self, other: &Rational) -> bool {
        self.0 == other.0
    }

    fn is_eqv(&self, other: &Rational) -> bool {
        self.is_eq(other)
    }

    fn is_equal(&self, other: &Rational) -> bool {
        self.is_eq(other)
    }
}

impl SchemeNumber for Rational {
    fn sign(self, sign: Sign) -> Self {
        if let Sign::Minus = sign {
            Rational(self.0.neg())
        } else {
            self
        }
    }

    fn is_exact(&self) -> bool {
        true
    }
    fn is_inexact(&self) -> bool {
        false
    }
    fn is_rational(&self) -> bool {
        true
    }
    fn is_integer(&self) -> bool {
        false
    }
    fn is_real(&self) -> bool {
        true
    }
    fn is_complex(&self) -> bool {
        false
    }
    fn is_finite(&self) -> bool {
        true
    }

    fn is_infinite(&self) -> bool {
        false
    }

    fn is_nan(&self) -> bool {
        false
    }
    fn is_neg_infinite(&self) -> bool {
        false
    }
}
