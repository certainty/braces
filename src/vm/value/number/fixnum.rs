use super::*;
use crate::vm::value::equality::SchemeEqual;
use az::{CheckedAs, CheckedCast};
use rug::integer::SmallInteger;
use rug::Integer;
use std::ops::{Add, Neg};

#[derive(Debug, PartialEq, Clone)]
pub enum Fixnum {
    Big(Integer),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
}

impl Fixnum {
    pub fn as_big(&self) -> Integer {
        match self {
            Self::Big(n) => n.clone(),
            Self::I8(n) => Integer::from(*n),
            Self::I16(n) => Integer::from(*n),
            Self::I32(n) => Integer::from(*n),
            Self::I64(n) => Integer::from(*n),
        }
    }

    pub fn coerce(lhs: Fixnum, rhs: Fixnum) -> (Fixnum, Fixnum) {
        match (&lhs, &rhs) {
            (Fixnum::Big(_), Fixnum::Big(_)) => (lhs, rhs),
            (Fixnum::Big(_), other) => (lhs, Fixnum::Big(other.as_big())),
            (other, Fixnum::Big(_)) => ((Fixnum::Big(other.as_big()), rhs)),
            _ => todo!(),
        }
    }
}

macro_rules! with_fixnum {
    ($value:expr, $binding:ident, $op:expr) => {
        match $value {
            Fixnum::Big($binding) => $op,
            Fixnum::I8($binding) => $op,
            Fixnum::I16($binding) => $op,
            Fixnum::I32($binding) => $op,
            Fixnum::I64($binding) => $op,
        }
    };
}

macro_rules! map_fixnum {
    ($value:expr, $binding:ident, $op:expr) => {
        match $value {
            Fixnum::Big($binding) => Fixnum::Big($op),
            Fixnum::I8($binding) => Fixnum::I8($op),
            Fixnum::I16($binding) => Fixnum::I16($op),
            Fixnum::I32($binding) => Fixnum::I32($op),
            Fixnum::I64($binding) => Fixnum::I64($op),
        }
    };
}

macro_rules! binop {
    ($lhs:expr, $rhs:expr, $op:ident) => {
        match ($lhs, $rhs) {
            // big goes with everything
            (Fixnum::Big(lhs), Fixnum::Big(rhs)) => Ok(Fixnum::Big(lhs.$op(rhs))),
            (Fixnum::Big(lhs), Fixnum::I8(rhs)) => Ok(Fixnum::Big(lhs.$op(rhs))),
            (Fixnum::Big(lhs), Fixnum::I16(rhs)) => Ok(Fixnum::Big(lhs.$op(rhs))),
            (Fixnum::Big(lhs), Fixnum::I32(rhs)) => Ok(Fixnum::Big(lhs.$op(rhs))),
            (Fixnum::Big(lhs), Fixnum::I64(rhs)) => Ok(Fixnum::Big(lhs.$op(rhs))),
            (Fixnum::I8(lhs), Fixnum::Big(rhs)) => Ok(Fixnum::Big(lhs.$op(rhs))),
            (Fixnum::I16(lhs), Fixnum::Big(rhs)) => Ok(Fixnum::Big(lhs.$op(rhs))),
            (Fixnum::I32(lhs), Fixnum::Big(rhs)) => Ok(Fixnum::Big(lhs.$op(rhs))),
            (Fixnum::I64(lhs), Fixnum::Big(rhs)) => Ok(Fixnum::Big(lhs.$op(rhs))),

            // machine types can't be mixed
            (Fixnum::I64(lhs), Fixnum::I64(rhs)) => Ok(Fixnum::I64(lhs.$op(rhs))),
            (Fixnum::I32(lhs), Fixnum::I32(rhs)) => Ok(Fixnum::I32(lhs.$op(rhs))),
            (Fixnum::I16(lhs), Fixnum::I16(rhs)) => Ok(Fixnum::I16(lhs.$op(rhs))),
            (Fixnum::I8(lhs), Fixnum::I8(rhs)) => Ok(Fixnum::I8(lhs.$op(rhs))),
            _ => Err(error::arithmetic_error(
                "Incompatible numeric types for addition",
            )),
        }
    };
}

impl From<Integer> for Fixnum {
    fn from(num: Integer) -> Fixnum {
        Fixnum::Big(num)
    }
}

impl From<i8> for Fixnum {
    fn from(num: i8) -> Fixnum {
        Fixnum::I8(num)
    }
}

impl From<i16> for Fixnum {
    fn from(num: i16) -> Fixnum {
        Fixnum::I16(num)
    }
}

impl From<i32> for Fixnum {
    fn from(num: i32) -> Fixnum {
        Fixnum::I32(num)
    }
}

impl From<i64> for Fixnum {
    fn from(num: i64) -> Fixnum {
        Fixnum::I64(num)
    }
}

impl SchemeNumber for Fixnum {
    fn sign(self, sign: Sign) -> Self {
        if let Sign::Minus = sign {
            map_fixnum!(self, n, n.neg())
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
        false
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

    fn is_nan(&self) -> bool {
        false
    }
    fn is_neg_infinite(&self) -> bool {
        false
    }
}

impl SchemeNumberExactness for Fixnum {
    fn to_inexact(self) -> flonum::Flonum {
        match self {
            Self::Big(n) => flonum::Flonum::from(n.clone()),
            Self::I8(n) => flonum::Flonum::F32(n as f32),
            Self::I16(n) => flonum::Flonum::F32(n as f32),
            Self::I32(n) => flonum::Flonum::F64(n as f64),
            Self::I64(n) => flonum::Flonum::F64(n as f64),
        }
    }

    fn to_exact(self) -> Option<Number> {
        Some(Number::Real(real::RealNumber::Fixnum(self)))
    }
}

impl SchemeEqual<Fixnum> for Fixnum {
    fn is_eq(&self, other: &Fixnum) -> bool {
        match (self, other) {
            (Fixnum::Big(x), Fixnum::Big(y)) => x == y,

            (Fixnum::Big(x), Fixnum::I8(y)) => *x == *SmallInteger::from(*y),
            (Fixnum::Big(x), Fixnum::I16(y)) => *x == *SmallInteger::from(*y),
            (Fixnum::Big(x), Fixnum::I32(y)) => *x == *SmallInteger::from(*y),
            (Fixnum::Big(x), Fixnum::I64(y)) => *x == *SmallInteger::from(*y),

            (Fixnum::I8(x), Fixnum::I8(y)) => x == y,
            (Fixnum::I8(x), Fixnum::I16(y)) => (*x as i16) == *y,
            (Fixnum::I8(x), Fixnum::I32(y)) => (*x as i32) == *y,
            (Fixnum::I8(x), Fixnum::I64(y)) => (*x as i64) == *y,

            (Fixnum::I16(x), Fixnum::I16(y)) => x == y,
            (Fixnum::I16(x), Fixnum::I8(y)) => *x == (*y as i16),
            (Fixnum::I16(x), Fixnum::I32(y)) => (*x as i32) == *y,
            (Fixnum::I16(x), Fixnum::I64(y)) => (*x as i64) == *y,

            (Fixnum::I32(x), Fixnum::I32(y)) => x == y,
            (Fixnum::I32(x), Fixnum::I8(y)) => *x == (*y as i32),
            (Fixnum::I32(x), Fixnum::I16(y)) => *x == (*y as i32),
            (Fixnum::I32(x), Fixnum::I64(y)) => (*x as i64) == *y,

            (Fixnum::I64(x), Fixnum::I64(y)) => x == y,
            (Fixnum::I64(x), Fixnum::I8(y)) => *x == (*y as i64),
            (Fixnum::I64(x), Fixnum::I16(y)) => *x == (*y as i64),
            (Fixnum::I64(x), Fixnum::I32(y)) => *x == (*y as i64),
            _ => other.is_eq(self),
        }
    }

    fn is_eqv(&self, other: &Fixnum) -> bool {
        self.is_eq(other)
    }

    fn is_equal(&self, other: &Fixnum) -> bool {
        self.is_eq(other)
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
        Some(match self {
            Self::Big(n) => flonum::Flonum::from(n.clone()),
            Self::I8(n) => flonum::Flonum::F32(n as f32),
            Self::I16(n) => flonum::Flonum::F32(n as f32),
            Self::I32(n) => flonum::Flonum::F64(n as f64),
            Self::I64(n) => flonum::Flonum::F64(n as f64),
        })
    }
}

// Arith
impl Add<Fixnum> for Fixnum {
    type Output = ArithResult<Fixnum>;

    fn add(self, rhs: Fixnum) -> Self::Output {
        binop!(self, rhs, add)
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
