use super::error;
use super::*;
use crate::vm::value::equality::SchemeEqual;
use az::{CheckedAs, CheckedCast};
use rug::{float::SmallFloat, Float};
use std::ops::Add;
use std::ops::Neg;

pub const PRECISION: u32 = 200;

#[derive(Debug, PartialEq, Clone)]
pub enum Flonum {
    Big(Float),
    F32(f32),
    F64(f64),
}

impl Flonum {
    pub fn to_float(&self) -> Float {
        match self {
            Flonum::Big(inner) => inner.clone(),
            Flonum::F32(inner) => Float::with_val(PRECISION, inner),
            Flonum::F64(inner) => Float::with_val(PRECISION, inner),
        }
    }

    pub fn coerce(lhs: Flonum, rhs: Flonum) -> (Flonum, Flonum) {
        match (&lhs, &rhs) {
            (Flonum::Big(_), Flonum::Big(_)) => (lhs, rhs),
            (Flonum::Big(_), Flonum::F32(inner)) => (lhs, Flonum::from(*inner)),
            (Flonum::F32(inner), Flonum::Big(_)) => (Flonum::from(*inner), rhs),
            (Flonum::Big(_), Flonum::F64(inner)) => (lhs, Flonum::from(*inner)),
            (Flonum::F64(inner), Flonum::Big(_)) => (Flonum::from(*inner), rhs),
            (Flonum::F32(_), Flonum::F32(_)) => (lhs, rhs),
            (Flonum::F64(_), Flonum::F64(_)) => (lhs, rhs),
            (Flonum::F32(inner), Flonum::F64(_)) => (Flonum::F64(*inner as f64), rhs),
            (Flonum::F64(_), Flonum::F32(inner)) => (lhs, Flonum::F64(*inner as f64)),
        }
    }

    pub fn infinity() -> Self {
        Flonum::F64(f64::INFINITY)
    }

    pub fn neg_infinity() -> Self {
        Flonum::F64(f64::NEG_INFINITY)
    }

    pub fn nan() -> Self {
        Flonum::F64(f64::NAN)
    }
}

macro_rules! map_flonum {
    ($value:expr, $binding:ident, $op:expr) => {
        match $value {
            Flonum::Big($binding) => Flonum::Big($op),
            Flonum::F32($binding) => Flonum::F32($op),
            Flonum::F64($binding) => Flonum::F64($op),
        }
    };
}

macro_rules! with_flonum {
    ($value:expr, $binding:ident, $op:expr) => {
        match $value {
            Flonum::Big($binding) => $op,
            Flonum::F32($binding) => $op,
            Flonum::F64($binding) => $op,
        }
    };
}

macro_rules! binop {
    ($lhs:expr, $rhs:expr, $op:ident) => {
        match ($lhs, $rhs) {
            // big goes with everything
            (Flonum::Big(lhs), Flonum::Big(rhs)) => Ok(Flonum::Big(lhs.$op(rhs))),
            (Flonum::Big(lhs), Flonum::F32(rhs)) => Ok(Flonum::Big(lhs.$op(rhs))),
            (Flonum::Big(lhs), Flonum::F64(rhs)) => Ok(Flonum::Big(lhs.$op(rhs))),
            (Flonum::F32(lhs), Flonum::Big(rhs)) => Ok(Flonum::Big(lhs.$op(rhs))),
            (Flonum::F64(lhs), Flonum::Big(rhs)) => Ok(Flonum::Big(lhs.$op(rhs))),

            // machine types can't be mixed
            (Flonum::F32(lhs), Flonum::F32(rhs)) => Ok(Flonum::F32(lhs.$op(rhs))),
            (Flonum::F64(lhs), Flonum::F64(rhs)) => Ok(Flonum::F64(lhs.$op(rhs))),
            _ => Err(error::arithmetic_error(
                "Incompatible numeric types for addition",
            )),
        }
    };
}

impl From<rug::Integer> for Flonum {
    fn from(num: rug::Integer) -> Flonum {
        Flonum::Big(Float::with_val(200, num))
    }
}

impl From<f32> for Flonum {
    fn from(num: f32) -> Flonum {
        Flonum::Big(Float::with_val(PRECISION, num))
    }
}

impl From<f64> for Flonum {
    fn from(num: f64) -> Flonum {
        Flonum::Big(Float::with_val(PRECISION, num))
    }
}

impl From<Float> for Flonum {
    fn from(num: Float) -> Flonum {
        Flonum::Big(num)
    }
}

impl Add<Flonum> for Flonum {
    type Output = ArithResult<Flonum>;

    fn add(self, rhs: Flonum) -> Self::Output {
        binop!(self, rhs, add)
    }
}

impl Add<fixnum::Fixnum> for Flonum {
    type Output = ArithResult<Flonum>;

    fn add(self, rhs: fixnum::Fixnum) -> Self::Output {
        if let Some(flo) = rhs.checked_as::<flonum::Flonum>() {
            binop!(self, flo, add)
        } else {
            Err(error::arithmetic_error("Can't convert fixnum to flonum"))
        }
    }
}

impl Add<rational::Rational> for Flonum {
    type Output = ArithResult<Flonum>;

    fn add(self, rhs: rational::Rational) -> Self::Output {
        if let Some(flo) = rhs.checked_as::<flonum::Flonum>() {
            binop!(self, flo, add)
        } else {
            Err(error::arithmetic_error("Can't convert rational to flonum"))
        }
    }
}

impl SchemeNumber for Flonum {
    fn sign(self, sign: Sign) -> Self {
        if let Sign::Minus = sign {
            map_flonum!(self, n, n.neg())
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
        match self {
            Flonum::Big(_) => true,
            _ => with_flonum!(self, n, n.is_finite()),
        }
    }

    fn is_infinite(&self) -> bool {
        match self {
            Flonum::Big(_) => false,
            _ => with_flonum!(self, n, n.is_infinite()),
        }
    }

    fn is_nan(&self) -> bool {
        match self {
            Flonum::Big(_) => false,
            _ => with_flonum!(self, n, n.is_nan()),
        }
    }
    fn is_neg_infinite(&self) -> bool {
        if self.is_infinite() {
            with_flonum!(self, n, n.is_sign_negative())
        } else {
            false
        }
    }
}

impl SchemeNumberExactness for Flonum {
    fn to_inexact(self) -> Flonum {
        self
    }

    fn to_exact(self) -> Option<Number> {
        if let Some(r) = self.to_float().to_rational() {
            Some(Number::Real(real::RealNumber::Rational(
                rational::Rational::from(r),
            )))
        } else {
            None
        }
    }
}

impl SchemeEqual<Flonum> for Flonum {
    fn is_eq(&self, other: &Flonum) -> bool {
        match (self, other) {
            (Flonum::F64(x), Flonum::F64(y)) => x == y,
            (Flonum::F32(x), Flonum::F32(y)) => x == y,
            (Flonum::F64(x), Flonum::F32(y)) => *x == (*y as f64),
            _ => other.is_eq(self),
        }
    }

    fn is_eqv(&self, other: &Flonum) -> bool {
        self.is_eq(other)
    }

    fn is_equal(&self, other: &Flonum) -> bool {
        self.is_eq(other)
    }
}

// casts
impl CheckedCast<rational::Rational> for Flonum {
    fn checked_cast(self) -> Option<rational::Rational> {
        self.to_float()
            .to_rational()
            .map(|r| rational::Rational { inner: r })
    }
}
