use super::error;
use super::*;
use crate::vm::value::equality::SchemeEqual;
use az::{CheckedAs, CheckedCast};
use rug::Float;
use std::ops::{Add, Div, Neg};

pub const PRECISION: u32 = 200;

#[derive(Debug, PartialEq, Clone)]
pub struct Flonum {
    inner: Float,
}

impl Flonum {
    pub fn new(inner: Float) -> Self {
        Flonum { inner }
    }

    pub fn as_inner(&self) -> &Float {
        &self.inner
    }

    pub fn map<Op: FnOnce(&Float) -> Float>(&self, op: Op) -> Self {
        Self {
            inner: op(&self.inner),
        }
    }

    pub fn infinity() -> Self {
        Self::new(Float::with_val(PRECISION, f64::INFINITY))
    }

    pub fn neg_infinity() -> Self {
        Self::new(Float::with_val(PRECISION, f64::NEG_INFINITY))
    }

    pub fn nan() -> Self {
        Self::new(Float::with_val(PRECISION, f64::NAN))
    }
}

// cast and coerce
impl From<Integer> for Flonum {
    fn from(num: Integer) -> Flonum {
        Flonum::new(Float::with_val(PRECISION, num))
    }
}

impl From<Float> for Flonum {
    fn from(num: Float) -> Flonum {
        Flonum::new(num)
    }
}

impl From<f32> for Flonum {
    fn from(num: f32) -> Flonum {
        Flonum::new(Float::with_val(PRECISION, num))
    }
}

impl From<f64> for Flonum {
    fn from(num: f64) -> Flonum {
        Flonum::new(Float::with_val(PRECISION, num))
    }
}

// casts
impl CheckedCast<rational::Rational> for Flonum {
    fn checked_cast(self) -> Option<rational::Rational> {
        self.as_inner()
            .to_rational()
            .map(|r| rational::Rational { inner: r })
    }
}

impl SchemeNumber for Flonum {
    fn sign(self, sign: Sign) -> Self {
        if let Sign::Minus = sign {
            self.map(|n| n.as_neg().clone())
        } else {
            self
        }
    }

    fn is_exact(&self) -> bool {
        false
    }
    fn is_inexact(&self) -> bool {
        true
    }
    fn is_rational(&self) -> bool {
        !(self.is_nan() || self.is_infinite())
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
        self.inner.is_finite()
    }

    fn is_infinite(&self) -> bool {
        self.inner.is_infinite()
    }

    fn is_nan(&self) -> bool {
        self.inner.is_nan()
    }
    fn is_neg_infinite(&self) -> bool {
        self.is_infinite() && self.inner.is_sign_negative()
    }
}

impl SchemeNumberExactness for Flonum {
    fn to_inexact(self) -> Flonum {
        self
    }

    fn to_exact(self) -> Option<Number> {
        self.checked_as::<rational::Rational>()
            .map(|r| Number::Real(real::RealNumber::Rational(r)))
    }
}

impl SchemeEqual<Flonum> for Flonum {
    fn is_eq(&self, other: &Flonum) -> bool {
        self.inner == other.inner
    }

    fn is_eqv(&self, other: &Flonum) -> bool {
        self.is_eq(other)
    }

    fn is_equal(&self, other: &Flonum) -> bool {
        self.is_eq(other)
    }
}

// arithmetic
impl Add<Flonum> for Flonum {
    type Output = ArithResult<Flonum>;

    fn add(self, rhs: Flonum) -> Self::Output {
        Ok(Flonum::new(self.inner + rhs.inner))
    }
}

impl Add<fixnum::Fixnum> for Flonum {
    type Output = ArithResult<Flonum>;

    fn add(self, rhs: fixnum::Fixnum) -> Self::Output {
        if let Some(flo) = rhs.checked_as::<flonum::Flonum>() {
            self.add(flo)
        } else {
            Err(error::arithmetic_error("Can't convert fixnum to flonum"))
        }
    }
}

impl Add<rational::Rational> for Flonum {
    type Output = ArithResult<Flonum>;

    fn add(self, rhs: rational::Rational) -> Self::Output {
        if let Some(flo) = rhs.checked_as::<flonum::Flonum>() {
            self.add(flo)
        } else {
            Err(error::arithmetic_error("Can't convert rational to flonum"))
        }
    }
}

impl Div<Flonum> for Flonum {
    type Output = ArithResult<Flonum>;

    fn div(self, rhs: Flonum) -> Self::Output {
        Ok(Flonum::new(self.inner.div(rhs.inner)))
    }
}
