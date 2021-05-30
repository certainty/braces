use super::real;
use super::*;
use crate::vm::value::equality::SchemeEqual;
use az::{CheckedAs, CheckedCast};
use std::ops::Add;
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
    fn to_inexact(self) -> flonum::Flonum {
        let inexact = rug::Float::with_val(200, self.inner.numer().clone())
            / rug::Float::with_val(200, self.inner.denom().clone());
        flonum::Flonum::from(inexact)
    }

    fn to_exact(self) -> Option<Number> {
        Some(Number::Real(real::RealNumber::Rational(self.clone())))
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

impl CheckedCast<flonum::Flonum> for Rational {
    fn checked_cast(self) -> Option<flonum::Flonum> {
        let inexact = rug::Float::with_val(flonum::PRECISION, self.inner.numer().clone())
            / rug::Float::with_val(200, self.inner.denom().clone());

        Some(flonum::Flonum::from(inexact))
    }
}

impl From<fixnum::Fixnum> for Rational {
    fn from(n: fixnum::Fixnum) -> Rational {
        match n {
            fixnum::Fixnum::Big(n) => Self::from(n),
            fixnum::Fixnum::I8(n) => Self::from(n),
            fixnum::Fixnum::I16(n) => Self::from(n),
            fixnum::Fixnum::I32(n) => Self::from(n),
            fixnum::Fixnum::I64(n) => Self::from(n),
        }
    }
}

impl Add<Rational> for Rational {
    type Output = ArithResult<Rational>;

    fn add(self, rhs: Rational) -> Self::Output {
        Ok(Rational {
            inner: self.inner.add(rhs.inner),
        })
    }
}

impl Add<fixnum::Fixnum> for Rational {
    type Output = ArithResult<Rational>;

    fn add(self, rhs: fixnum::Fixnum) -> Self::Output {
        self.add(Self::from(rhs))
    }
}

impl Add<flonum::Flonum> for Rational {
    type Output = ArithResult<flonum::Flonum>;

    fn add(self, rhs: flonum::Flonum) -> Self::Output {
        if let Some(lhs) = self.checked_as::<flonum::Flonum>() {
            lhs.add(rhs)
        } else {
            Err(error::arithmetic_error("Can't convert rational to flonum"))
        }
    }
}
