use crate::compiler::frontend::reader::sexp::SExpression;
use crate::compiler::source::{HasSourceLocation, Location};
use crate::vm::value::number::Number;
use crate::vm::value::Value;
use std::fmt::Formatter;

/// `Datum` is what the reader creates from the textual input.
///
/// It's essentially an s-expression (`Sexp`) with a location,
/// which is tracked through the whole compilation pipeline.
#[derive(Debug, PartialEq, Clone)]
pub struct Datum {
    location: Location,
    s_expression: SExpression,
}

impl Datum {
    pub fn new(s_expression: SExpression, location: Location) -> Self {
        Self {
            s_expression,
            location,
        }
    }

    pub fn s_expression(&self) -> &SExpression {
        &self.s_expression
    }

    /// Create a symbol from a token in code
    ///
    /// This is the method that is most likely used by the reader itself.
    /// The `crate::compiler::frontend::expander::Expander` might inject symbols
    /// which are then tracked via `forged_symbol` rather
    pub fn symbol<S: Into<String>, L: Into<Location>>(val: S, location: L) -> Self {
        Self::new(SExpression::symbol(val), location.into())
    }
    /// Create a symbol that didn't exist in the source code
    ///
    /// The `crate::compiler::frontend::expander::Expander` might inject symbols as a result
    /// of a transformation phase.
    pub fn forged_symbol<S: Into<String>, L: Into<Location>>(val: S, location: L) -> Self {
        Self::new(SExpression::forged_symbol(val), location.into())
    }

    #[inline]
    pub fn boolean<L: Into<Location>>(v: bool, location: L) -> Self {
        Self::new(v.into(), location.into())
    }

    #[inline]
    pub fn character<L: Into<Location>>(v: char, location: L) -> Self {
        Self::new(v.into(), location.into())
    }

    #[inline]
    pub fn string<S: Into<String>, L: Into<Location>>(v: S, location: L) -> Self {
        Self::new(SExpression::from(v.into()), location.into())
    }

    #[inline]
    pub fn number<N: Into<Number>, L: Into<Location>>(v: N, location: L) -> Self {
        Self::new(SExpression::from(v.into()), location.into())
    }

    #[inline]
    pub fn list<I, L: Into<Location>>(elements: I, location: L) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Datum>,
    {
        Self::new(SExpression::list(elements), location.into())
    }

    pub fn improper_list<I, L: Into<Location>>(elements: I, element: Datum, location: L) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Datum>,
    {
        Self::new(
            SExpression::improper_list(elements, element),
            location.into(),
        )
    }

    pub fn vector<I, L: Into<Location>>(elements: I, location: L) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Datum>,
    {
        Self::new(SExpression::vector(elements), location.into())
    }

    /// Create an s-expression from a value, if that's possible.
    ///
    /// Code is data, but this implementation doesn't use the same representation
    /// for runtime values as it uses for s-expressions. This converts between the two worlds.
    pub fn from_value(v: &Value, location: Location) -> Option<Self> {
        match v {
            Value::Char(c) => Some(Self::character(c.clone(), location)),
            Value::Symbol(sym) => Some(Self::forged_symbol(sym.as_str(), location)),
            Value::InternedString(s) => Some(Self::string(s.as_str(), location)),
            Value::UninternedString(s) => Some(Self::string(s.clone(), location)),
            Value::Number(n) => Some(Self::number(n.clone(), location)),
            Value::Bool(b) => Some(Self::boolean(b.clone(), location)),
            Value::ProperList(ls) => {
                let elts: Option<Vec<_>> = ls
                    .iter()
                    .map(|e| Self::from_value(e, location.clone()))
                    .collect();
                elts.map(|e| Self::list(e, location))
            }
            _ => None,
        }
    }

    pub fn is_atom(&self) -> bool {
        match self.s_expression() {
            SExpression::Vector(_) => false,
            SExpression::List(_) => false,
            SExpression::ImproperList(_, _) => false,
            SExpression::ByteVector(_) => false,
            _ => true,
        }
    }

    pub fn is_symbol(&self) -> bool {
        match self.s_expression() {
            SExpression::Symbol(_) => true,
            _ => false,
        }
    }

    /// If the current datum is a proper list, return the slice of that list's elements
    ///
    /// This function is mostly used in the parser, which uses slice patterns extensively.
    pub fn list_slice(&self) -> Option<&[Datum]> {
        match self.s_expression() {
            SExpression::List(ls) => Some(&ls[..]),
            _ => None,
        }
    }
}
/// The `Display` instance for `Datum` provides
/// a representation that can be used in debug output and logs.
/// It's not as sophisticated as the `Writer`, so if you want a
/// full faithful, valid, external representation of s-expressions use the `Writer` instead.
impl std::fmt::Display for Datum {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "[{}..{}]@{}",
            self.s_expression().to_string(),
            self.source_location().span.start(),
            self.source_location().span.end()
        ))
    }
}

impl HasSourceLocation for Datum {
    fn source_location(&self) -> &Location {
        &self.location
    }
}
