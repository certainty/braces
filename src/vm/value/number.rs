use super::equality::SchemeEqual;
use num::BigInt;
use num::BigRational;
use std::ops::Neg;

#[derive(Debug, PartialEq, Clone)]
pub enum Exactness {
    Inexact,
    Exact,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Sign {
    Plus,
    Minus,
}

pub trait SchemeNumber {
    fn sign(self, sign: Sign) -> Self;
    fn is_exact(&self) -> bool;
    fn is_inexact(&self) -> bool;
    fn is_rational(&self) -> bool;
    fn is_integer(&self) -> bool;
    fn is_real(&self) -> bool;
    fn is_complex(&self) -> bool;
    fn is_finite(&self) -> bool;
    fn is_infinite(&self) -> bool;
    fn is_neg_infinite(&self) -> bool;
    fn is_nan(&self) -> bool;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Number {
    Real(RealNumber),
    // Complex
}

#[derive(Debug, PartialEq, Clone)]
pub enum RealNumber {
    Fixnum(Fixnum),
    Flonum(Flonum),
    Rational(BigRational),
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

#[derive(Debug, PartialEq, Clone)]
pub enum Flonum {
    //    Big(BigDecimal),
    F32(f32),
    F64(f64),
}

impl Flonum {
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

#[derive(Debug, PartialEq, Clone)]
pub enum Fixnum {
    Big(BigInt),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
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

impl Number {
    pub fn inifinity() -> Self {
        Self::Real(RealNumber::Flonum(Flonum::infinity()))
    }

    pub fn neg_inifinity() -> Self {
        Self::Real(RealNumber::Flonum(Flonum::neg_infinity()))
    }

    pub fn nan() -> Self {
        Self::Real(RealNumber::Flonum(Flonum::nan()))
    }

    pub fn big<I: Into<BigInt>>(num: I) -> Number {
        Self::Real(num.into().into())
    }

    pub fn fraction<N: Into<BigInt>, D: Into<BigInt>>(numer: N, denom: D) -> Number {
        Self::Real(RealNumber::Rational(BigRational::from((
            numer.into(),
            denom.into(),
        ))))
    }

    pub fn i8(num: i8) -> Number {
        Self::Real(num.into())
    }

    pub fn i16(num: i16) -> Number {
        Self::Real(num.into())
    }

    pub fn i32(num: i32) -> Number {
        Self::Real(num.into())
    }

    pub fn i64(num: i64) -> Number {
        Self::Real(num.into())
    }

    pub fn f32(num: f32) -> Number {
        Self::Real(RealNumber::Flonum(num.into()))
    }

    pub fn f64(num: f64) -> Number {
        Self::Real(RealNumber::Flonum(num.into()))
    }
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

impl<I: Into<Fixnum>> From<I> for RealNumber {
    fn from(n: I) -> RealNumber {
        RealNumber::Fixnum(n.into().into())
    }
}

impl<I: Into<RealNumber>> From<I> for Number {
    fn from(n: I) -> Number {
        Number::Real(n.into())
    }
}

impl SchemeEqual<RealNumber> for RealNumber {
    fn is_eq(&self, other: &RealNumber) -> bool {
        match (self, other) {
            (RealNumber::Fixnum(lhs), RealNumber::Fixnum(rhs)) => lhs == rhs,
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

impl SchemeEqual<Number> for Number {
    fn is_eq(&self, other: &Number) -> bool {
        match (self, other) {
            (Number::Real(lhs), Number::Real(rhs)) => lhs.is_eq(rhs),
            _ => false,
        }
    }

    fn is_eqv(&self, other: &Number) -> bool {
        self.is_eq(other)
    }

    fn is_equal(&self, other: &Number) -> bool {
        self.is_eq(other)
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

impl SchemeNumber for BigRational {
    fn sign(self, sign: Sign) -> Self {
        if let Sign::Minus = sign {
            self.neg()
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

impl SchemeNumber for Number {
    fn sign(self, sign: Sign) -> Self {
        match self {
            Self::Real(n) => Self::Real(n.sign(sign)),
        }
    }

    fn is_exact(&self) -> bool {
        match self {
            Self::Real(n) => n.is_exact(),
        }
    }
    fn is_inexact(&self) -> bool {
        !self.is_exact()
    }

    fn is_rational(&self) -> bool {
        match self {
            Self::Real(n) => n.is_rational(),
        }
    }
    fn is_integer(&self) -> bool {
        match self {
            Self::Real(n) => n.is_integer(),
        }
    }
    fn is_real(&self) -> bool {
        match self {
            Self::Real(n) => n.is_real(),
        }
    }

    fn is_complex(&self) -> bool {
        false
    }

    fn is_finite(&self) -> bool {
        match self {
            Self::Real(n) => n.is_finite(),
        }
    }

    fn is_infinite(&self) -> bool {
        match self {
            Self::Real(n) => n.is_infinite(),
        }
    }

    fn is_nan(&self) -> bool {
        match self {
            Self::Real(n) => n.is_nan(),
        }
    }
    fn is_neg_infinite(&self) -> bool {
        match self {
            Self::Real(n) => n.is_neg_infinite(),
        }
    }
}

// implement SchemeEqual

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn exactness_test() {
        assert!(!Number::f64(0.1).is_exact());
        assert!(!Number::f32(0.1).is_exact());
        assert!(Number::i8(1).is_exact());
        assert!(Number::i16(1).is_exact());
        assert!(Number::i32(1).is_exact());
        assert!(Number::i64(1).is_exact());
        assert!(Number::big(1).is_exact());
        assert!(Number::fraction(1, 2).is_exact());
    }

    #[test]
    fn all_is_real() {
        assert!(Number::f64(0.1).is_real());
        assert!(Number::f32(0.1).is_real());
        assert!(Number::i8(1).is_real());
        assert!(Number::i16(1).is_real());
        assert!(Number::i32(1).is_real());
        assert!(Number::i64(1).is_real());
        assert!(Number::big(1).is_real());
        assert!(Number::fraction(1, 2).is_real());
    }

    #[test]
    fn is_rational() {
        assert!(!Number::f64(0.1).is_rational());
        assert!(!Number::f32(0.1).is_rational());
        assert!(!Number::i8(1).is_rational());
        assert!(!Number::i16(1).is_rational());
        assert!(!Number::i32(1).is_rational());
        assert!(!Number::i64(1).is_rational());
        assert!(!Number::big(1).is_rational());
        assert!(Number::fraction(1, 2).is_rational());
    }

    #[test]
    fn test_oddities() {
        assert!(Number::nan().is_nan());
        assert!(Number::inifinity().is_infinite());
        assert!(!Number::inifinity().is_finite());
        assert!(Number::neg_inifinity().is_infinite());
        assert!(!Number::neg_inifinity().is_finite());
    }

    #[test]
    fn nan_is_not_rational() {
        assert!(
            !Number::nan().is_rational(),
            "Expected nan not to be rational"
        )
    }

    #[test]
    fn inf_is_not_rational() {
        assert!(
            !Number::inifinity().is_rational(),
            "Expected infinity not to be rational"
        );

        assert!(
            !Number::neg_inifinity().is_rational(),
            "Expected -infinity not to be rational"
        )
    }
}
