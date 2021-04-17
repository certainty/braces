use super::value::{Factory, Value};

/// The scheme writer is responsible to create external representations
/// of any printable scheme value.
///
/// The current implementation is built for correctness and needs to be
/// tweaked a bit for better performance.
///

#[derive(Debug)]
pub struct Writer;

impl Writer {
    pub fn new() -> Self {
        Writer
    }

    pub fn write(&self, v: &Value, values: &Factory) -> String {
        self.write_impl(v, &values, true)
    }

    fn write_impl(&self, v: &Value, values: &Factory, quote: bool) -> String {
        match v {
            Value::Bool(true) => "#t".to_string(),
            Value::Bool(false) => "#f".to_string(),
            Value::Symbol(_) => self.write_symbol(values.unintern(&v).unwrap(), quote),
            Value::Char(c) => self.write_char(*c),
            Value::InternedString(_) => self.write_string(values.unintern(&v).unwrap()),
            Value::UninternedString(s) => self.write_string(&s),
            Value::ProperList(elts) => {
                let body: Vec<String> = elts
                    .iter()
                    .map(|e| self.write_impl(&e, values, false))
                    .collect();

                format!("'({})", body.join(" "))
            }
            Value::Unspecified => "#<unspecified>".to_string(),
        }
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

        if char::is_ascii_digit(first_char) {
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
            _ => buf.push(*c),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::sexp;
    use crate::compiler::source::StringSource;
    use crate::vm::scheme::value::arbitrary::SymbolString;
    use crate::vm::scheme::value::Value;
    use quickcheck;

    #[test]
    fn test_write_bool() {
        let values = Factory::default();
        let writer = Writer::new();

        let v = values.bool_true();
        assert_eq!(writer.write(v, &values), "#t");

        let v = values.bool_false();
        assert_eq!(writer.write(v, &values), "#f");
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
        assert_eq!(writer.write(&v, &values), "'...");

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
        let input = Value::Char('\r');
        assert_eq!(read_my_write(&input).unwrap(), input);
    }

    #[quickcheck]
    fn test_read_is_write_inverse(val: Value) -> bool {
        read_my_write(&val).is_ok()
    }

    #[quickcheck]
    fn test_read_is_write_inverse_symbol(val: SymbolString) -> bool {
        let mut values = Factory::default();
        let writer = Writer::new();
        let sym = values.symbol(val.0);
        //let sym = values.symbol("\\'");
        let external = writer.write(&sym, &values);
        let mut source = StringSource::new(&external, "");

        sexp::parse(&mut source)
            .map(|e| values.from_datum(&e))
            .is_ok()
    }

    fn read_my_write(val: &Value) -> sexp::Result<Value> {
        let mut values = Factory::default();
        let writer = Writer::new();
        let external = writer.write(&val, &values);
        let mut source = StringSource::new(&external, "");

        sexp::parse(&mut source).map(|e| values.from_datum(&e))
    }
}
