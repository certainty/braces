use super::value::Value;

/// The scheme writer is responsible to create external representations
/// of any printable scheme value.
///
/// The current implementation is built for correctness and needs to be
/// tweaked a bit for better performance.
///

pub struct Writer;

// TODO: clean up the mess :D
impl Writer {
    pub fn new() -> Self {
        Writer
    }

    pub fn write(&self, v: &Value) -> String {
        self.write_impl(v, true)
    }

    fn write_impl(&self, v: &Value, quote: bool) -> String {
        match v {
            Value::Bool(true) => "#t".to_string(),
            Value::Bool(false) => "#f".to_string(),
            Value::Symbol(sym) => self.write_symbol(&sym.as_str(), quote),
            Value::Char(c) => self.write_char(*c),
            Value::String(s) => self.write_string(&s),
            Value::ProperList(elts) => {
                let body: Vec<String> = elts.iter().map(|e| self.write_impl(&e, false)).collect();

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
            if !char::is_ascii(&c) || char::is_whitespace(c) {
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

    fn write_string(&self, s: &String) -> String {
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
    use crate::compiler::source::StringSource;
    use crate::vm::scheme::reader;
    use crate::vm::scheme::value::Value;
    use crate::vm::scheme::writer;
    use quickcheck;

    #[test]
    fn test_write_bool() {
        let writer = Writer;

        assert_eq!(writer.write(&Value::Bool(true)), "#t");
        assert_eq!(writer.write(&Value::Bool(false)), "#f");
    }

    #[test]
    fn test_write_char() {
        let writer = Writer;

        assert_eq!(writer.write(&Value::Char('\u{0018}')), "#\\delete");
    }

    #[test]
    fn test_write_symbol() {
        let writer = Writer;
        assert_eq!(writer.write(&Value::Symbol("...".to_string())), "'...");

        assert_eq!(
            writer.write(&Value::symbol(&"foo bar".to_string())),
            "'|foo bar|"
        );

        assert_eq!(
            writer.write(&Value::symbol(&"foo 💣 bar".to_string())),
            "'|foo \\x1f4a3; bar|"
        );

        assert_eq!(writer.write(&Value::symbol(&"".to_string())), "'||");
        assert_eq!(writer.write(&Value::symbol(&"\t".to_string())), "'|\\t|");
        assert_eq!(
            writer.write(&Value::symbol(&"test \\| foo".to_string())),
            "'|test \\x5c;\\| foo|"
        );

        assert_eq!(
            writer.write(&Value::symbol(&"test2 | foo".to_string())),
            "'|test2 \\| foo|"
        );

        assert_eq!(
            writer.write(&Value::symbol(&"test2 \\ foo".to_string())),
            "'|test2 \\x5c; foo|"
        );

        // special initial or number as the first char
        assert_eq!(writer.write(&Value::symbol(&"2foo".to_string())), "'|2foo|");
    }

    #[test]
    fn test_write_proper_list() {
        let writer = Writer;
        let elts = vec![Value::boolean(true), Value::boolean(false)];
        let ls = Value::proper_list(elts);

        assert_eq!(writer.write(&ls), "'(#t #f)");
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

    fn read_my_write(val: &Value) -> reader::Result<Value> {
        let reader = reader::Reader::new();
        let writer = writer::Writer::new();
        let external = writer.write(&val);
        let mut source = StringSource::new(&external, "");

        reader.read(&mut source)
    }
}
