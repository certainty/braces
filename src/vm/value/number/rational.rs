use super::real;
use super::*;
use crate::vm::value::equality::SchemeEqual;
use az::{CheckedAs, CheckedCast};
use std::ops::{Add, Div, Mul, Neg, Sub};

#[repr(transparent)]
#[derive(Debug, Clone, PartialEq)]
pub struct Rational {
    pub(crate) inner: rug::Rational,
}

impl Rational {
    pub fn new(inner: rug::Rational) -> Self {
        Self { inner }
    }

    pub fn as_inner(&self) -> &rug::Rational {
        &self.inner
    }

    pub fn map<Op: FnOnce(rug::Rational) -> rug::Rational>(&self, op: Op) -> Self {
        Self {
            inner: op(self.inner.clone()),
        }
    }
}

impl<I: Into<rug::Rational>> From<I> for Rational {
    fn from(n: I) -> Rational {
        Rational { inner: n.into() }
    }
}

impl From<fixnum::Fixnum> for Rational {
    fn from(n: fixnum::Fixnum) -> Rational {
        Self::from(n.as_inner())
    }
}

impl CheckedCast<flonum::Flonum> for Rational {
    fn checked_cast(self) -> Option<flonum::Flonum> {
        let numer_f = flonum::Flonum::from(self.inner.numer().clone());
        let denom_f = flonum::Flonum::from(self.inner.denom().clone());

        numer_f.div(denom_f).ok()
    }
}

impl SchemeNumberExactness for Rational {
    fn to_inexact(self) -> flonum::Flonum {
        self.checked_as::<flonum::Flonum>()
            .expect("division can't fail")
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

impl Mul<Rational> for Rational {
    type Output = ArithResult<Rational>;

    fn mul(self, rhs: Rational) -> Self::Output {
        Ok(Rational {
            inner: self.inner.mul(rhs.inner),
        })
    }
}

impl Mul<fixnum::Fixnum> for Rational {
    type Output = ArithResult<Rational>;

    fn mul(self, rhs: fixnum::Fixnum) -> Self::Output {
        self.mul(Self::from(rhs))
    }
}

impl Mul<flonum::Flonum> for Rational {
    type Output = ArithResult<flonum::Flonum>;

    fn mul(self, rhs: flonum::Flonum) -> Self::Output {
        if let Some(lhs) = self.checked_as::<flonum::Flonum>() {
            lhs.mul(rhs)
        } else {
            Err(error::arithmetic_error("Can't convert rational to flonum"))
        }
    }
}

impl Sub<Rational> for Rational {
    type Output = ArithResult<Rational>;

    fn sub(self, rhs: Rational) -> Self::Output {
        Ok(Rational {
            inner: self.inner.sub(rhs.inner),
        })
    }
}

impl Sub<fixnum::Fixnum> for Rational {
    type Output = ArithResult<Rational>;

    fn sub(self, rhs: fixnum::Fixnum) -> Self::Output {
        self.sub(Self::from(rhs))
    }
}

impl Sub<flonum::Flonum> for Rational {
    type Output = ArithResult<flonum::Flonum>;

    fn sub(self, rhs: flonum::Flonum) -> Self::Output {
        if let Some(lhs) = self.checked_as::<flonum::Flonum>() {
            lhs.sub(rhs)
        } else {
            Err(error::arithmetic_error("Can't convert rational to flonum"))
        }
    }
}

impl Div<Rational> for Rational {
    type Output = ArithResult<Rational>;

    fn div(self, rhs: Rational) -> Self::Output {
        Ok(Rational {
            inner: self.inner.div(rhs.inner),
        })
    }
}

impl Div<fixnum::Fixnum> for Rational {
    type Output = ArithResult<Rational>;

    fn div(self, rhs: fixnum::Fixnum) -> Self::Output {
        self.div(Self::from(rhs))
    }
}

impl Div<flonum::Flonum> for Rational {
    type Output = ArithResult<flonum::Flonum>;

    fn div(self, rhs: flonum::Flonum) -> Self::Output {
        if let Some(lhs) = self.checked_as::<flonum::Flonum>() {
            lhs.div(rhs)
        } else {
            Err(error::arithmetic_error("Can't convert rational to flonum"))
        }
    }
}
