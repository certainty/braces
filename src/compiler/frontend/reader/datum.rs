pub mod abbreviation;
pub mod boolean;
pub mod byte_vector;
pub mod character;
pub mod list;
pub mod number;
pub mod parser;
pub mod string;
pub mod symbol;
pub mod vector;
pub mod whitespace;

use crate::compiler::frontend::syntax::symbol::Symbol;
use crate::compiler::source::{HasSourceLocation, Location};
use crate::vm::value::number::Number;
use crate::vm::value::Value;
use std::fmt::Formatter;

pub use parser::*;

/// `Datum` is what the reader creates from the textual input which are s-expressions
///
/// **Important**:
/// Values of this type are not used as runtime representations of values in the VM.
/// They're optimised to be ergonomic for the different phases of the compiler.
#[derive(Debug, PartialEq, Clone)]
pub enum Datum {
    Bool(bool, Location),
    Symbol(Symbol, Location),
    String(String, Location),
    Char(char, Location),
    Number(Number, Location),
    List(Vec<Datum>, Location),
    ImproperList(Vec<Datum>, Box<Datum>, Location),
    Vector(Vec<Datum>, Location),
    ByteVector(Vec<u8>, Location),
}

impl Datum {
    /// Create a symbol from a token in code
    ///
    /// This is the method that is most likely used by the reader itself.
    /// The `crate::compiler::frontend::expander::Expander` might inject symbols
    /// which are then tracked via `forged_symbol` rather
    pub fn symbol<S: Into<String>, L: Into<Location>>(val: S, location: L) -> Self {
        Self::Symbol(Symbol::from_code(val), location.into())
    }
    /// Create a symbol that didn't exist in the source code
    ///
    /// The `crate::compiler::frontend::expander::Expander` might inject symbols as a result
    /// of a transformation phase.
    pub fn forged_symbol<S: Into<String>, L: Into<Location>>(val: S, location: L) -> Self {
        Self::Symbol(Symbol::forged(val), location.into())
    }

    #[inline]
    pub fn boolean<L: Into<Location>>(v: bool, location: L) -> Self {
        Self::Bool(v.into(), location.into())
    }

    #[inline]
    pub fn character<L: Into<Location>>(v: char, location: L) -> Self {
        Self::Char(v.into(), location.into())
    }

    #[inline]
    pub fn string<S: Into<String>, L: Into<Location>>(v: S, location: L) -> Self {
        Self::String(v.into(), location.into())
    }

    #[inline]
    pub fn number<N: Into<Number>, L: Into<Location>>(v: N, location: L) -> Self {
        Self::Number(v.into(), location.into())
    }

    #[inline]
    pub fn list<I, L: Into<Location>>(elements: I, location: L) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Datum>,
    {
        Self::List(
            elements.into_iter().map(Into::into).collect(),
            location.into(),
        )
    }

    pub fn improper_list<I, L: Into<Location>>(elements: I, element: Datum, location: L) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Datum>,
    {
        let head = elements.into_iter().map(Into::into).collect();
        Self::ImproperList(head, Box::new(element), location.into())
    }

    pub fn vector<I, L: Into<Location>>(elements: I, location: L) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Datum>,
    {
        Self::Vector(
            elements.into_iter().map(Into::into).collect(),
            location.into(),
        )
    }

    pub fn byte_vector<I, L: Into<Location>>(bytes: I, location: L) -> Self
    where
        I: IntoIterator,
        I::Item: Into<u8>,
    {
        Self::ByteVector(bytes.into_iter().map(Into::into).collect(), location.into())
    }

    /// Create an s-expression from a value, if that's possible.
    ///
    /// Code is data, but this implementation doesn't use the same representation
    /// for runtime values as it uses for s-expressions. This converts between the two worlds.
    pub fn from_value(v: &Value, location: Location) -> Option<Self> {
        match v {
            Value::Syntax(datum) => Some(datum.clone()),
            Value::Char(c) => Some(Self::character(c.clone(), location)),
            Value::Symbol(sym) => Some(Self::forged_symbol(sym.as_str(), location)),
            Value::InternedString(s) => Some(Self::string(s.as_str(), location)),
            Value::UninternedString(s) => Some(Self::string(s.clone(), location)),
            Value::Number(n) => Some(Self::number(n.clone(), location)),
            Value::Bool(b) => Some(Self::boolean(b.clone(), location)),
            Value::ProperList(ls) => {
                let elts: Option<Vec<_>> = ls
                    .iter()
                    .map(|e| Self::from_value(&e.get_inner_ref(), location.clone()))
                    .collect();
                elts.map(|e| Self::list(e, location))
            }
            _ => None,
        }
    }

    pub fn is_atom(&self) -> bool {
        match self {
            Self::Vector(_, _) => false,
            Self::List(_, _) => false,
            Self::ImproperList(_, _, _) => false,
            Self::ByteVector(_, _) => false,
            _ => true,
        }
    }

    pub fn is_symbol(&self) -> bool {
        match self {
            Self::Symbol(_, _) => true,
            _ => false,
        }
    }

    pub fn is_proper_list(&self) -> bool {
        match self {
            Self::List(_, _) => true,
            _ => false,
        }
    }

    pub fn is_improper_list(&self) -> bool {
        match self {
            Self::ImproperList(_, _, _) => true,
            _ => false,
        }
    }

    pub fn improper_list_slice(&self) -> Option<(&[Datum], &Datum)> {
        match self {
            Self::ImproperList(head, tail, _) => Some((&head[..], &tail)),
            _ => None,
        }
    }

    /// If the current datum is a proper list, return the slice of that list's elements
    ///
    /// This function is mostly used in the parser, which uses slice patterns extensively.
    pub fn list_slice(&self) -> Option<&[Datum]> {
        match self {
            Self::List(ls, _) => Some(&ls[..]),
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
        let location = format!(
            "[{}..{}]",
            self.source_location().span.start(),
            self.source_location().span.end()
        );

        match self {
            Self::Bool(v, _) => f.write_fmt(format_args!("{}@{}", location, v)),
            Self::Symbol(v, _) => {
                f.write_fmt(format_args!("{}@{}", location, v.as_str().to_string()))
            }
            Self::String(v, _) => f.write_fmt(format_args!("{}@\"{}\"", location, v)),
            Self::Char(v, _) => f.write_fmt(format_args!("{}@{}", location, v.to_string())),
            Self::Number(n, _) => f.write_fmt(format_args!("{}@{}", location, n.to_string())),
            Self::List(inner, _) => {
                let elements: Vec<_> = inner.iter().map(|e| e.to_string()).collect();
                f.write_fmt(format_args!("{} @ ( {} )", location, elements.join(" ")))
            }
            Self::ImproperList(head, tail, _) => {
                let head_elements: Vec<_> = head.iter().map(|e| e.to_string()).collect();
                f.write_fmt(format_args!(
                    "{} @ ({} . {})",
                    location,
                    head_elements.join(" "),
                    tail.to_string()
                ))
            }
            Self::Vector(inner, _) => {
                let elements: Vec<_> = inner.iter().map(|e| e.to_string()).collect();
                f.write_fmt(format_args!("{} @ #({})", location, elements.join(" ")))
            }
            Self::ByteVector(inner, _) => {
                let elements: Vec<_> = inner.iter().map(|e| format!("{:x?}", e)).collect();
                f.write_fmt(format_args!("{} @ #u8({})", location, elements.join(" ")))
            }
        }
    }
}

impl HasSourceLocation for Datum {
    fn source_location(&self) -> &Location {
        match self {
            Datum::Bool(_, loc) => loc,
            Datum::Symbol(_, loc) => loc,
            Datum::String(_, loc) => loc,
            Datum::Char(_, loc) => loc,
            Datum::Number(_, loc) => loc,
            Datum::List(_, loc) => loc,
            Datum::ImproperList(_, _, loc) => loc,
            Datum::Vector(_, loc) => loc,
            Datum::ByteVector(_, loc) => loc,
        }
    }
}
