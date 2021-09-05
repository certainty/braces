use super::frontend::error::Error;
use super::Result;
use crate::compiler::frontend::error::Detail;
use crate::compiler::source::Location;
use std::iter::FromIterator;

pub enum ParseResult<T> {
    Applicable(Result<T>),
    NonApplicable(String, Location),
}

impl<T> ParseResult<T> {
    pub fn collect_res(results: Vec<ParseResult<T>>) -> Vec<Result<T>> {
        let mut total = vec![];

        for res in results {
            match res {
                ParseResult::Applicable(res) => total.push(res),
                _ => (),
            }
        }

        total
    }

    pub fn accept(v: T) -> ParseResult<T> {
        ParseResult::Applicable(Ok(v))
    }

    pub fn error(e: Error) -> ParseResult<T> {
        ParseResult::Applicable(Err(e))
    }

    pub fn ignore<S: Into<String>, L: Into<Location>>(message: S, location: L) -> ParseResult<T> {
        ParseResult::NonApplicable(message.into(), location.into())
    }

    pub fn or<F: FnOnce() -> ParseResult<T>>(self, op: F) -> ParseResult<T> {
        match self {
            Self::NonApplicable(_, _) => op(),
            other => other,
        }
    }

    pub fn and<F: FnOnce() -> ParseResult<T>>(self, op: F) -> ParseResult<T> {
        match self {
            Self::Applicable(_ignored) => op(),
            other => other,
        }
    }

    pub fn is_err(self) -> bool {
        match self {
            Self::NonApplicable(_, _) => false,
            Self::Applicable(res) => res.is_err(),
        }
    }

    pub fn is_ok(self) -> bool {
        match self {
            Self::NonApplicable(_, _) => false,
            Self::Applicable(res) => res.is_ok(),
        }
    }

    pub fn map<R, F: FnOnce(T) -> R>(self, op: F) -> ParseResult<R> {
        match self {
            Self::Applicable(v) => ParseResult::<R>::Applicable(v.map(op)),
            Self::NonApplicable(m, l) => ParseResult::<R>::NonApplicable(m, l),
        }
    }

    pub fn flat_map<F: FnOnce(T) -> ParseResult<T>>(self, op: F) -> ParseResult<T> {
        match self {
            Self::Applicable(Ok(v)) => op(v),
            other => other,
        }
    }

    pub fn res(self) -> Result<T> {
        match self {
            Self::NonApplicable(message, location) => Err(Error::parse_error(
                &message,
                Detail::new("", location),
                vec![],
            )),
            Self::Applicable(res) => res,
        }
    }

    #[inline]
    pub fn is_applicable(self) -> bool {
        match self {
            Self::Applicable(_) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_non_applicable(self) -> bool {
        !self.is_applicable()
    }

    pub fn map_non_applicable(self, v: Result<T>) -> Result<T> {
        match self {
            Self::NonApplicable(_, _) => v,
            Self::Applicable(other) => other,
        }
    }
}

impl<T> From<Result<T>> for ParseResult<T> {
    fn from(value: Result<T>) -> Self {
        Self::Applicable(value)
    }
}

impl<T> From<T> for ParseResult<T> {
    fn from(value: T) -> Self {
        Self::Applicable(Ok(value))
    }
}

impl<T> From<ParseResult<T>> for Result<T> {
    fn from(res: ParseResult<T>) -> Self {
        res.res()
    }
}

impl<T> FromIterator<ParseResult<T>> for Result<Vec<T>> {
    fn from_iter<I: IntoIterator<Item = ParseResult<T>>>(iter: I) -> Self {
        iter.into_iter().map(|i| i.res()).collect()
    }
}

impl<T> FromIterator<ParseResult<T>> for Vec<Result<T>> {
    fn from_iter<I: IntoIterator<Item = ParseResult<T>>>(iter: I) -> Self {
        iter.into_iter().map(|i| i.res()).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_result_collect_err() {
        let res1: ParseResult<u32> = ParseResult::Applicable(Ok(10));
        let res2: ParseResult<u32> = ParseResult::Applicable(Ok(12));
        let res3: ParseResult<u32> = ParseResult::Applicable(Err(Error::parse_error(
            "couldn't parse",
            Detail::new("", 0..1),
            vec![],
        )));
        let res4: ParseResult<u32> = ParseResult::Applicable(Ok(20));
        let total: Result<Vec<u32>> = vec![res1, res2, res3, res4].into_iter().collect();

        assert!(total.is_err(), "expected error")
    }

    #[test]
    fn test_parse_result_collect_ok() {
        let res1: ParseResult<u32> = ParseResult::Applicable(Ok(10));
        let res2: ParseResult<u32> = ParseResult::Applicable(Ok(12));
        let res4: ParseResult<u32> = ParseResult::Applicable(Ok(20));
        let total: Result<Vec<u32>> = vec![res1, res2, res4].into_iter().collect();

        assert_eq!(total.unwrap(), vec![10, 12, 20])
    }

    #[test]
    fn test_parse_result_predicates() {
        let res: ParseResult<u32> = ParseResult::ignore("test", 0..1);
        assert!(res.is_non_applicable(), "Expected non-applicable");

        let res: ParseResult<u32> = ParseResult::ignore("test", 0..1);
        assert!(!res.is_applicable(), "Expected non-applicable");

        let res: ParseResult<u32> = ParseResult::Applicable(Ok(10));
        assert!(res.is_applicable(), "Expected applicable");

        let res: ParseResult<u32> = ParseResult::Applicable(Ok(10));
        assert!(!res.is_non_applicable(), "Expected applicable");
    }

    #[test]
    fn test_parse_result_and() {
        let res1: ParseResult<u32> = ParseResult::ignore("test", 0..1);
        let res2: ParseResult<u32> = ParseResult::Applicable(Ok(10));
        assert!(
            res1.and(|| res2).is_non_applicable(),
            "Expected non-applicable"
        );

        let res1: ParseResult<u32> = ParseResult::ignore("test", 0..1);
        let res2: ParseResult<u32> = ParseResult::Applicable(Ok(10));
        assert!(
            res2.and(|| res1).is_non_applicable(),
            "Expected non-applicable"
        );

        let res1: ParseResult<u32> = ParseResult::Applicable(Ok(5));
        let res2: ParseResult<u32> = ParseResult::Applicable(Ok(10));
        assert_eq!(res1.and(|| res2).res().unwrap(), 10)
    }

    #[test]
    fn test_parse_result_or() {
        let res1: ParseResult<u32> = ParseResult::ignore("test", 0..1);
        let res2: ParseResult<u32> = ParseResult::Applicable(Ok(10));
        assert_eq!(res1.or(|| res2).res().unwrap(), 10);

        let res1: ParseResult<u32> = ParseResult::ignore("test", 0..1);
        let res2: ParseResult<u32> = ParseResult::Applicable(Ok(10));
        assert_eq!(res1.or(|| res2).res().unwrap(), 10);

        let res1: ParseResult<u32> = ParseResult::ignore("test", 0..1);
        let res2: ParseResult<u32> = ParseResult::ignore("test", 0..1);

        assert!(
            res1.or(|| res2).is_non_applicable(),
            "Expected non-applicable parse result"
        )
    }
}
