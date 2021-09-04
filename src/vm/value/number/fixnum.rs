use super::*;
use crate::vm::value::equality::SchemeEqual;
use az::{CheckedAs, CheckedCast};
use rug::Integer;
use std::ops::{Add, Div, Mul, Neg, Sub};

#[repr(transparent)]
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Fixnum {
    inner: Integer,
}

impl Fixnum {
    pub fn new(inner: Integer) -> Self {
        Fixnum { inner }
    }

    pub fn as_inner(&self) -> &Integer {
        &self.inner
    }

    pub fn map<Op: FnOnce(&Integer) -> Integer>(&self, op: Op) -> Self {
        Self {
            inner: op(&self.inner),
        }
    }
}

impl ToString for Fixnum {
    fn to_string(&self) -> String {
        self.inner.to_string()
    }
}

impl<I: Into<Integer>> From<I> for Fixnum {
    fn from(num: I) -> Fixnum {
        Fixnum::new(num.into())
    }
}

// casts
impl CheckedCast<rational::Rational> for Fixnum {
    fn checked_cast(self) -> Option<rational::Rational> {
        Some(rational::Rational::from(self))
    }
}

impl CheckedCast<flonum::Flonum> for Fixnum {
    fn checked_cast(self) -> Option<flonum::Flonum> {
        let f = rug::Float::with_val(flonum::PRECISION, self.inner);
        Some(flonum::Flonum::new(f))
    }
}

impl SchemeNumber for Fixnum {
    fn sign(self, sign: Sign) -> Self {
        if let Sign::Minus = sign {
            self.map(|n| Integer::from(n.neg()))
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
        true
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

    fn is_neg_infinite(&self) -> bool {
        false
    }
    fn is_nan(&self) -> bool {
        false
    }
}

impl SchemeNumberExactness for Fixnum {
    fn to_inexact(self) -> flonum::Flonum {
        self.checked_as::<flonum::Flonum>().unwrap()
    }

    fn to_exact(self) -> Option<Number> {
        Some(Number::Real(real::RealNumber::Fixnum(self)))
    }
}

impl SchemeEqual<Fixnum> for Fixnum {
    fn is_eq(&self, other: &Fixnum) -> bool {
        self.inner == other.inner
    }

    fn is_eqv(&self, other: &Fixnum) -> bool {
        self.is_eq(other)
    }

    fn is_equal(&self, other: &Fixnum) -> bool {
        self.is_eq(other)
    }
}

// Arith
impl Add<Fixnum> for Fixnum {
    type Output = ArithResult<Fixnum>;

    fn add(self, rhs: Fixnum) -> Self::Output {
        Ok(Fixnum::new(self.inner + rhs.inner))
    }
}

impl Add<flonum::Flonum> for Fixnum {
    type Output = ArithResult<flonum::Flonum>;

    fn add(self, rhs: flonum::Flonum) -> Self::Output {
        rhs.add(self)
    }
}

impl Add<rational::Rational> for Fixnum {
    type Output = ArithResult<rational::Rational>;

    fn add(self, rhs: rational::Rational) -> Self::Output {
        rhs.add(self)
    }
}

impl Mul<Fixnum> for Fixnum {
    type Output = ArithResult<Fixnum>;

    fn mul(self, rhs: Fixnum) -> Self::Output {
        Ok(Fixnum::new(self.inner * rhs.inner))
    }
}

impl Mul<flonum::Flonum> for Fixnum {
    type Output = ArithResult<flonum::Flonum>;

    fn mul(self, rhs: flonum::Flonum) -> Self::Output {
        rhs.mul(self)
    }
}

impl Mul<rational::Rational> for Fixnum {
    type Output = ArithResult<rational::Rational>;

    fn mul(self, rhs: rational::Rational) -> Self::Output {
        rhs.mul(self)
    }
}

impl Sub<Fixnum> for Fixnum {
    type Output = ArithResult<Fixnum>;

    fn sub(self, rhs: Fixnum) -> Self::Output {
        Ok(Fixnum::new(self.inner - rhs.inner))
    }
}

impl Sub<flonum::Flonum> for Fixnum {
    type Output = ArithResult<flonum::Flonum>;

    fn sub(self, rhs: flonum::Flonum) -> Self::Output {
        self.to_inexact().sub(rhs)
    }
}

impl Sub<rational::Rational> for Fixnum {
    type Output = ArithResult<rational::Rational>;

    fn sub(self, rhs: rational::Rational) -> Self::Output {
        rational::Rational::from(self.inner.clone()).sub(rhs)
    }
}

impl Div<Fixnum> for Fixnum {
    type Output = ArithResult<Fixnum>;

    fn div(self, rhs: Fixnum) -> Self::Output {
        Ok(Fixnum::new(self.inner / rhs.inner))
    }
}

impl Div<flonum::Flonum> for Fixnum {
    type Output = ArithResult<flonum::Flonum>;

    fn div(self, rhs: flonum::Flonum) -> Self::Output {
        self.to_inexact().div(rhs)
    }
}

impl Div<rational::Rational> for Fixnum {
    type Output = ArithResult<rational::Rational>;

    fn div(self, rhs: rational::Rational) -> Self::Output {
        rational::Rational::from(self.inner.clone()).div(rhs)
    }
}
