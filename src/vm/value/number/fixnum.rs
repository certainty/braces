use super::*;
use crate::vm::value::equality::SchemeEqual;
use num::BigInt;
use std::ops::Neg;

#[derive(Debug, PartialEq, Clone)]
pub enum Fixnum {
    Big(BigInt),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
}

impl Fixnum {
    pub fn as_big(&self) -> BigInt {
        match self {
            Self::Big(n) => n.clone(),
            Self::I8(n) => BigInt::from(*n),
            Self::I16(n) => BigInt::from(*n),
            Self::I32(n) => BigInt::from(*n),
            Self::I64(n) => BigInt::from(*n),
        }
    }

    pub fn coerce(lhs: Fixnum, rhs: Fixnum) -> (Fixnum, Fixnum) {
        match (&lhs, &rhs) {
            (Fixnum::Big(_), Fixnum::Big(_)) => todo!(),
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

impl From<BigInt> for Fixnum {
    fn from(num: BigInt) -> Fixnum {
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
    fn to_inexact(&self) -> ArithResult<flonum::Flonum> {
        match self {
            Self::Big(n) => Err(error::arithmetic_error(
                "Can't create inexact value of non-machine sized int",
            )),
            Self::I8(n) => Ok(flonum::Flonum::F32(*n as f32)),
            Self::I16(n) => Ok(flonum::Flonum::F32(*n as f32)),
            Self::I32(n) => Ok(flonum::Flonum::F64(*n as f64)),
            Self::I64(n) => Ok(flonum::Flonum::F64(*n as f64)),
        }
    }

    fn to_exact(&self) -> ArithResult<Number> {
        Ok(Number::Real(real::RealNumber::Fixnum(self.clone())))
    }
}

impl SchemeEqual<Fixnum> for Fixnum {
    fn is_eq(&self, other: &Fixnum) -> bool {
        match (self, other) {
            (Fixnum::Big(x), Fixnum::Big(y)) => x == y,

            (Fixnum::Big(x), Fixnum::I8(y)) => *x == BigInt::from(*y),
            (Fixnum::Big(x), Fixnum::I16(y)) => *x == BigInt::from(*y),
            (Fixnum::Big(x), Fixnum::I32(y)) => *x == BigInt::from(*y),
            (Fixnum::Big(x), Fixnum::I64(y)) => *x == BigInt::from(*y),

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn exactness_test() {
        assert!(Number::i8(1).is_exact());
        assert!(Number::i16(1).is_exact());
        assert!(Number::i32(1).is_exact());
        assert!(Number::i64(1).is_exact());
        assert!(Number::big(1).is_exact());
    }

    #[test]
    fn all_is_real() {
        assert!(Number::i8(1).is_real());
        assert!(Number::i16(1).is_real());
        assert!(Number::i32(1).is_real());
        assert!(Number::i64(1).is_real());
        assert!(Number::big(1).is_real());
    }

    #[test]
    fn is_rational() {
        assert!(!Number::i8(1).is_rational());
        assert!(!Number::i16(1).is_rational());
        assert!(!Number::i32(1).is_rational());
        assert!(!Number::i64(1).is_rational());
        assert!(!Number::big(1).is_rational());
    }
}
