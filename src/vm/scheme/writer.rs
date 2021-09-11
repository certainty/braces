use crate::compiler::frontend::reader;
use crate::vm::value::number::fixnum::Fixnum;
use crate::vm::value::number::flonum::Flonum;
use crate::vm::value::number::{real::RealNumber, Number, SchemeNumber};
use crate::vm::value::procedure;
use crate::vm::value::{Factory, Value};
use std::collections::HashSet;

/// The scheme writer is responsible to create external representations
/// of any printable scheme value.
///
/// The current implementation is built for correctness and needs to be
/// tweaked a bit for better performance.

#[derive(Debug)]
pub struct Writer {
    symbol_special_initial: HashSet<char>,
}

impl Writer {
    pub fn new() -> Self {
        let special_initial: HashSet<char> =
            String::from(reader::datum::symbol::SYMBOL_SPECIAL_INITIAL)
                .chars()
                .collect();
        Writer {
            symbol_special_initial: special_initial,
        }
    }

    pub fn write(&self, v: &Value, values: &Factory) -> String {
        self.write_impl(v, &values, true)
    }

    fn write_impl(&self, v: &Value, values: &Factory, quote: bool) -> String {
        match v {
            Value::Bool(true) => "#t".to_string(),
            Value::Bool(false) => "#f".to_string(),
            Value::Symbol(s) => self.write_symbol(s.as_str(), quote),
            Value::Char(c) => self.write_char(*c),
            Value::Number(num) => self.write_number(num),
            Value::InternedString(s) => self.write_string(s.as_str()),
            Value::UninternedString(s) => self.write_string(&s),
            Value::Closure(closure) => {
                self.write_procedure(&procedure::Procedure::Native(closure.procedure_rc()))
            }
            Value::Procedure(proc) => self.write_procedure(&proc),
            Value::Vector(elts) => {
                let body: Vec<String> = elts
                    .iter()
                    .map(|e| self.write_impl(&e, values, false))
                    .collect();

                format!("#({})", body.join(" "))
            }
            Value::ByteVector(elts) => {
                let body: Vec<String> = elts.iter().map(|e| format!("{}", e)).collect();

                format!("#u8({})", body.join(" "))
            }
            Value::ProperList(elts) => {
                let body: Vec<String> = elts
                    .iter()
                    .map(|e| self.write_impl(&e, values, false))
                    .collect();

                format!("'({})", body.join(" "))
            }
            Value::ImproperList(head, tail) => {
                let head_body: Vec<String> = head
                    .iter()
                    .map(|e| self.write_impl(&e, values, false))
                    .collect();
                let tail_body = self.write_impl(&tail, values, false);

                format!("'({} . {})", head_body.join(" "), tail_body)
            }
            Value::Unspecified => "#<unspecified>".to_string(),
        }
    }

    fn write_number(&self, num: &Number) -> String {
        match num {
            Number::Real(RealNumber::Fixnum(v)) => self.write_fixnum(v),
            Number::Real(RealNumber::Flonum(v)) => self.write_flonum(v),
            Number::Real(RealNumber::Rational(v)) => format!("{}", v.inner),
        }
    }

    fn write_flonum(&self, num: &Flonum) -> String {
        match num {
            n if n.is_nan() => String::from("+nan.0"),
            n if n.is_neg_infinite() => String::from("-inf.0"),
            n if n.is_infinite() => String::from("+inf.0"),
            _ => format!("{}", num.as_inner()),
        }
    }

    fn write_fixnum(&self, num: &Fixnum) -> String {
        format!("{}", num.as_inner())
    }

    fn write_symbol(&self, sym: &str, quote: bool) -> String {
        if sym.len() == 0 {
            return self.add_quote(String::from("||"), quote);
        }

        let mut requires_delimiter = false;
        let mut external = String::new();

        for c in sym.chars() {
            if !char::is_ascii(&c) || char::is_whitespace(c) || c == '\'' {
                requires_delimiter = true;
            }
            self.escaped_symbol_char(&c, &mut external);
        }

        let first_char = &external.chars().next().unwrap();

        if !(char::is_alphabetic(*first_char) || self.symbol_special_initial.contains(first_char)) {
            requires_delimiter = true;
        }

        if requires_delimiter {
            self.add_quote(format!("|{}|", external), quote)
        } else {
            self.add_quote(external, quote)
        }
    }

    // create external representation of a single char, taking care of proper escaping rules
    fn write_char(&self, c: char) -> String {
        let mut b = [0; 4];
        let mut external = String::from("#\\");
        external.push_str(match c as u32 {
            // handle mnemonics
            0x0 => "null",
            0x7 => "alarm",
            0x8 => "backspace",
            0x9 => "tab",
            0x18 => "delete",
            0x1b => "escape",
            0x20 => "space",
            0xa => "newline",
            0xd => "return",
            // the rest is simply converted to a character
            _ => c.encode_utf8(&mut b),
        });
        external
    }

    fn write_string(&self, s: &str) -> String {
        let mut external = String::from("\"");

        for c in s.chars() {
            self.escaped_string_char(&c, &mut external);
        }

        external.push('"');
        external
    }

    fn write_procedure(&self, proc: &procedure::Procedure) -> String {
        match proc {
            procedure::Procedure::Native(proc) => format!(
                "#<procedure ({} {})>",
                proc.name().clone().unwrap_or(String::from("")),
                self.write_formals(&proc.arity)
            ),
            procedure::Procedure::Foreign(proc) => {
                format!(
                    "#<procedure ({} {})>",
                    proc.name,
                    self.write_formals(&proc.arity)
                )
            }
        }
    }

    fn write_formals(&self, arity: &procedure::Arity) -> String {
        match arity {
            procedure::Arity::Exactly(count) => (0..*count)
                .map(|i| format!("x{}", i))
                .collect::<Vec<String>>()
                .join(" "),
            procedure::Arity::AtLeast(count) => {
                let fixed_args: Vec<String> = (0..*count).map(|i| format!("x{}", i)).collect();
                format!("{} . rest", fixed_args.join(" "))
            }
            procedure::Arity::Many => ". args".to_string(),
        }
    }

    #[inline]
    fn add_quote(&self, s: String, quote: bool) -> String {
        if quote {
            format!("'{}", s)
        } else {
            s
        }
    }

    #[inline]
    fn escaped_string_char(&self, c: &char, buf: &mut String) {
        match c {
            '\n' => buf.push_str("\\n"),
            '\r' => buf.push_str("\\r"),
            '\t' => buf.push_str("\\t"),
            '"' => buf.push_str("\\\""),
            '\\' => buf.push_str("\\\\"),
            _ => buf.push(*c),
        }
    }

    #[inline]
    fn escaped_symbol_char(&self, c: &char, buf: &mut String) {
        match c {
            '\n' => buf.push_str("\\n"),
            '\r' => buf.push_str("\\r"),
            '\t' => buf.push_str("\\t"),
            '|' => buf.push_str("\\|"),
            '\\' => buf.push_str("\\x5c;"),
            c if !char::is_ascii(&c) => buf.push_str(&format!("\\x{:x};", *c as u32)),
            c if char::is_control(*c) => buf.push_str(&format!("\\x{:x};", *c as u32)),
            _ => buf.push(*c),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend;
    use crate::compiler::frontend::reader;
    use crate::compiler::source::{Registry, StringSource};
    use crate::vm::value::arbitrary::SymbolString;
    use crate::vm::value::Value;
    use quickcheck;

    #[test]
    fn test_write_bool() {
        let values = Factory::default();
        let writer = Writer::new();

        let v = values.bool_true();
        assert_eq!(writer.write(&v, &values), "#t");

        let v = values.bool_false();
        assert_eq!(writer.write(&v, &values), "#f");
    }

    #[test]
    fn test_write_char() {
        let values = Factory::default();
        let writer = Writer::new();

        let v = values.character('\u{0018}');
        assert_eq!(writer.write(&v, &values), "#\\delete");
    }

    #[test]
    fn test_write_symbol() {
        let mut values = Factory::default();
        let writer = Writer::new();

        let v = values.symbol("...");
        assert_eq!(writer.write(&v, &values), "'|...|");

        let v = values.symbol("foo bar");
        assert_eq!(writer.write(&v, &values), "'|foo bar|");

        let v = values.symbol("foo 💣 bar");
        assert_eq!(writer.write(&v, &values), "'|foo \\x1f4a3; bar|");

        let v = values.symbol("");
        assert_eq!(writer.write(&v, &values), "'||");

        let v = values.symbol("\t");
        assert_eq!(writer.write(&v, &values), "'|\\t|");

        let v = values.symbol("test \\| foo");
        assert_eq!(writer.write(&v, &values), "'|test \\x5c;\\| foo|");

        let v = values.symbol("test2 | foo");
        assert_eq!(writer.write(&v, &values), "'|test2 \\| foo|");

        let v = values.symbol("test2 \\ foo");
        assert_eq!(writer.write(&v, &values), "'|test2 \\x5c; foo|");

        // special initial or number as the first char
        let v = values.symbol("2foo");
        assert_eq!(writer.write(&v, &values), "'|2foo|");
    }

    #[test]
    fn test_write_proper_list() {
        let values = Factory::default();
        let writer = Writer::new();
        let elts = vec![values.bool_true().clone(), values.bool_false().clone()];
        let ls = values.proper_list(elts);

        assert_eq!(writer.write(&ls, &values), "'(#t #f)");
    }

    #[test]
    fn test_read_is_writer_inverse_bugs() {
        let mut values = Factory::default();

        let input = values.character('\r').clone();
        assert!(
            read_my_write(&input, &mut values).is_ok(),
            "expected read of write"
        );

        let input = values.symbol("@foo").clone();
        assert!(
            read_my_write(&input, &mut values).is_ok(),
            "expected read of write"
        );

        let input = values.symbol(".foo").clone();
        assert!(
            read_my_write(&input, &mut values).is_ok(),
            "expected read of write"
        );
    }

    #[quickcheck]
    fn test_read_is_write_inverse(val: Value) -> bool {
        let mut values = Factory::default();
        read_my_write(&val, &mut values).is_ok()
    }

    #[quickcheck]
    fn test_read_is_write_inverse_symbol(val: SymbolString) -> bool {
        let mut values = Factory::default();
        let sym = values.symbol(val.0);

        read_my_write(&sym, &mut values).is_ok()
    }

    fn read_my_write(val: &Value, values: &mut Factory) -> frontend::Result<Value> {
        let writer = Writer::new();
        let external = writer.write(&val, &values);
        let mut registry = Registry::new();
        let source = registry.add(&mut StringSource::new(&external)).unwrap();
        let reader = reader::Reader::new();
        let ast = reader.parse(&source)?;
        Ok(values.from_datum(ast.first()))
    }
}
