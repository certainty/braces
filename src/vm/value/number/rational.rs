use super::real;
use super::*;
use crate::vm::value::equality::SchemeEqual;
use std::ops::Neg;

#[repr(transparent)]
#[derive(Debug, Clone, PartialEq)]
pub struct Rational {
    pub(crate) inner: rug::Rational,
}

impl<I: Into<rug::Rational>> From<I> for Rational {
    fn from(n: I) -> Rational {
        Rational { inner: n.into() }
    }
}

impl SchemeNumberExactness for Rational {
    fn to_inexact(&self) -> ArithResult<flonum::Flonum> {
        let inexact = rug::Float::with_val(200, self.inner.numer().clone())
            / rug::Float::with_val(200, self.inner.denom().clone());
        Ok(flonum::Flonum::from(inexact))
    }

    fn to_exact(&self) -> ArithResult<Number> {
        Ok(Number::Real(real::RealNumber::Rational(self.clone())))
    }
}

impl SchemeEqual<Rational> for Rational {
    fn is_eq(&self, other: &Rational) -> bool {
        self.inner == other.inner
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
            Rational {
                inner: self.inner.neg(),
            }
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
