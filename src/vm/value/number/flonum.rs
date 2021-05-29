use super::error::{self, RuntimeError};
use super::*;
use crate::vm::value::equality::SchemeEqual;
use num::BigRational;
use std::ops::Neg;

#[derive(Debug, PartialEq, Clone)]
pub enum Flonum {
    F32(f32),
    F64(f64),
}

impl Flonum {
    pub fn as_f64(&self) -> f64 {
        match self {
            Self::F32(x) => *x as f64,
            Self::F64(x) => *x,
        }
    }

    pub fn coerce(lhs: Flonum, rhs: Flonum) -> (Flonum, Flonum) {
        match (&lhs, &rhs) {
            (Flonum::F32(_), Flonum::F32(_)) => (lhs, rhs),
            (Flonum::F64(_), Flonum::F64(_)) => (lhs, rhs),
            (Flonum::F32(_), Flonum::F64(_)) => (Flonum::F64(lhs.as_f64()), rhs),
            (Flonum::F64(_), Flonum::F32(_)) => (lhs, Flonum::F64(rhs.as_f64())),
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
            Flonum::F32($binding) => Flonum::F32($op),
            Flonum::F64($binding) => Flonum::F64($op),
        }
    };
}

macro_rules! with_flonum {
    ($value:expr, $binding:ident, $op:expr) => {
        match $value {
            Flonum::F32($binding) => $op,
            Flonum::F64($binding) => $op,
        }
    };
}

impl From<f32> for Flonum {
    fn from(num: f32) -> Flonum {
        Flonum::F32(num)
    }
}

impl From<f64> for Flonum {
    fn from(num: f64) -> Flonum {
        Flonum::F64(num)
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
        with_flonum!(self, n, n.is_finite())
    }

    fn is_infinite(&self) -> bool {
        with_flonum!(self, n, n.is_infinite())
    }

    fn is_nan(&self) -> bool {
        with_flonum!(self, n, n.is_nan())
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
    fn to_inexact(&self) -> ArithResult<Flonum> {
        Ok(self.clone())
    }

    fn to_exact(&self) -> ArithResult<Number> {
        if let Some(r) = BigRational::from_float(self.as_f64()) {
            Ok(Number::Real(real::RealNumber::Rational(
                rational::Rational::from(r),
            )))
        } else {
            Err(error::arithmetic_error("Can't convert into exact number"))
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn exactness_test() {
        assert!(!Number::f64(0.1).is_exact());
        assert!(!Number::f32(0.1).is_exact());
    }

    #[test]
    fn all_is_real() {
        assert!(Number::f64(0.1).is_real());
        assert!(Number::f32(0.1).is_real());
    }

    #[test]
    fn is_rational() {
        assert!(!Number::f64(0.1).is_rational());
        assert!(!Number::f32(0.1).is_rational());
    }
}
