use super::error::{self, RuntimeError};
use super::fixnum;
use super::flonum;
use super::rational;
use super::*;
use crate::vm::value::equality::SchemeEqual;
use std::ops::Add;

#[derive(Debug, PartialEq, Clone)]
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

impl SchemeNumberExactness for RealNumber {
    fn to_inexact(self) -> flonum::Flonum {
        with_realnum!(self, n, n.to_inexact())
    }

    fn to_exact(self) -> Option<Number> {
        with_realnum!(self, n, n.to_exact())
    }
}

impl RealNumber {
    /*
    pub fn lt(&self, other: Self) -> bool {
        match (self, other) {
            (Self::Fixnum(lhs), Self::Fixnum(rhs)) => lhs.lt(rhs),
            (Self::Fixnum(lhs), Self::Flonum(rhs)) => lhs.to_inexact().unwrap().lt(rhs),
            (Self::Flonum(lhs), Self::Flonum(rhs)) => lhs.cmp(rhs),
            (Self::Rational(lhs), Self::Rational(rhs)) => lhs.cmp(rhs),
        }
    }*/
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

    fn is_nan(&self) -> bool {
        with_realnum!(self, n, n.is_nan())
    }
    fn is_neg_infinite(&self) -> bool {
        with_realnum!(self, n, n.is_neg_infinite())
    }
}

impl Add<RealNumber> for RealNumber {
    type Output = ArithResult<RealNumber>;

    fn add(self, rhs: RealNumber) -> Self::Output {
        binop!(self, rhs, add)
    }
}
