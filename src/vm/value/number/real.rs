use super::*;
use crate::vm::value::equality::SchemeEqual;
use az::CheckedCast;
use std::ops::{Add, Div, Mul, Sub};

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum RealNumber {
    Fixnum(fixnum::Fixnum),
    Flonum(flonum::Flonum),
    Rational(rational::Rational),
}

macro_rules! map_realnum {
    ($value:expr, $binding:ident, $op:expr) => {
        match $value {
            RealNumber::Fixnum($binding) => RealNumber::Fixnum($op),
            RealNumber::Flonum($binding) => RealNumber::Flonum($op),
            RealNumber::Rational($binding) => RealNumber::Rational($op),
        }
    };
}

macro_rules! with_realnum {
    ($value:expr, $binding:ident, $op:expr) => {
        match $value {
            RealNumber::Fixnum($binding) => $op,
            RealNumber::Flonum($binding) => $op,
            RealNumber::Rational($binding) => $op,
        }
    };
}

macro_rules! binop {
    ($lhs:expr, $rhs:expr, $op:ident) => {
        match ($lhs, $rhs) {
            (RealNumber::Fixnum(lhs), RealNumber::Fixnum(rhs)) => {
                Ok(RealNumber::Fixnum(lhs.$op(rhs)?))
            }
            (RealNumber::Fixnum(lhs), RealNumber::Flonum(rhs)) => {
                Ok(RealNumber::Flonum(lhs.$op(rhs)?))
            }
            (RealNumber::Fixnum(lhs), RealNumber::Rational(rhs)) => {
                Ok(RealNumber::Rational(lhs.$op(rhs)?))
            }
            (RealNumber::Flonum(lhs), RealNumber::Fixnum(rhs)) => {
                Ok(RealNumber::Flonum(lhs.$op(rhs)?))
            }
            (RealNumber::Flonum(lhs), RealNumber::Rational(rhs)) => {
                Ok(RealNumber::Flonum(lhs.$op(rhs)?))
            }
            (RealNumber::Flonum(lhs), RealNumber::Flonum(rhs)) => {
                Ok(RealNumber::Flonum(lhs.$op(rhs)?))
            }
            (RealNumber::Rational(lhs), RealNumber::Fixnum(rhs)) => {
                Ok(RealNumber::Rational(lhs.$op(rhs)?))
            }
            (RealNumber::Rational(lhs), RealNumber::Flonum(rhs)) => {
                Ok(RealNumber::Flonum(lhs.$op(rhs)?))
            }
            (RealNumber::Rational(lhs), RealNumber::Rational(rhs)) => {
                Ok(RealNumber::Rational(lhs.$op(rhs)?))
            }
        }
    };
}

impl CheckedCast<fixnum::Fixnum> for RealNumber {
    fn checked_cast(self) -> Option<fixnum::Fixnum> {
        match self {
            Self::Fixnum(n) => Some(n),
            _ => None,
        }
    }
}

impl SchemeNumberExactness for RealNumber {
    fn to_inexact(self) -> flonum::Flonum {
        with_realnum!(self, n, n.to_inexact())
    }

    fn to_exact(self) -> Option<Number> {
        with_realnum!(self, n, n.to_exact())
    }
}

impl ToString for RealNumber {
    fn to_string(&self) -> String {
        match self {
            RealNumber::Fixnum(f) => f.to_string(),
            RealNumber::Flonum(f) => f.to_string(),
            RealNumber::Rational(f) => f.to_string(),
        }
    }
}

impl<I: Into<fixnum::Fixnum>> From<I> for RealNumber {
    fn from(n: I) -> RealNumber {
        RealNumber::Fixnum(n.into().into())
    }
}

impl SchemeEqual<RealNumber> for RealNumber {
    fn is_eq(&self, other: &RealNumber) -> bool {
        match (self, other) {
            (RealNumber::Fixnum(lhs), RealNumber::Fixnum(rhs)) => lhs.is_eq(rhs),
            (RealNumber::Flonum(lhs), RealNumber::Flonum(rhs)) => lhs.is_eq(rhs),
            (RealNumber::Rational(lhs), RealNumber::Rational(rhs)) => lhs == rhs,
            _ => false,
        }
    }

    fn is_eqv(&self, other: &RealNumber) -> bool {
        self.is_eq(other)
    }

    fn is_equal(&self, other: &RealNumber) -> bool {
        self.is_eq(other)
    }
}

impl SchemeNumber for RealNumber {
    fn sign(self, sign: Sign) -> Self {
        map_realnum!(self, n, n.sign(sign))
    }

    fn is_exact(&self) -> bool {
        with_realnum!(self, n, n.is_exact())
    }

    fn is_inexact(&self) -> bool {
        !self.is_exact()
    }

    fn is_rational(&self) -> bool {
        match self {
            RealNumber::Rational(_) => true,
            _ => false,
        }
    }
    fn is_integer(&self) -> bool {
        match self {
            RealNumber::Fixnum(_) => true,
            _ => false,
        }
    }
    fn is_real(&self) -> bool {
        true
    }
    fn is_complex(&self) -> bool {
        false
    }
    fn is_finite(&self) -> bool {
        with_realnum!(self, n, n.is_finite())
    }

    fn is_infinite(&self) -> bool {
        with_realnum!(self, n, n.is_infinite())
    }

    fn is_neg_infinite(&self) -> bool {
        with_realnum!(self, n, n.is_neg_infinite())
    }
    fn is_nan(&self) -> bool {
        with_realnum!(self, n, n.is_nan())
    }
}

impl Add<RealNumber> for RealNumber {
    type Output = ArithResult<RealNumber>;

    fn add(self, rhs: RealNumber) -> Self::Output {
        binop!(self, rhs, add)
    }
}

impl Sub<RealNumber> for RealNumber {
    type Output = ArithResult<RealNumber>;

    fn sub(self, rhs: RealNumber) -> Self::Output {
        binop!(self, rhs, sub)
    }
}
impl Mul<RealNumber> for RealNumber {
    type Output = ArithResult<RealNumber>;

    fn mul(self, rhs: RealNumber) -> Self::Output {
        binop!(self, rhs, mul)
    }
}
impl Div<RealNumber> for RealNumber {
    type Output = ArithResult<RealNumber>;

    fn div(self, rhs: RealNumber) -> Self::Output {
        if let (RealNumber::Fixnum(lhs), RealNumber::Fixnum(rhs)) = (&self, &rhs) {
            let r = rational::Rational::from(lhs.as_inner());
            let result = r.div(rhs.clone())?;
            if result.inner.denom() == &rug::Integer::from(1) {
                Ok(RealNumber::Fixnum(fixnum::Fixnum::from(
                    result.inner.numer(),
                )))
            } else {
                Ok(RealNumber::Rational(result))
            }
        } else {
            binop!(self, rhs, div)
        }
    }
}
