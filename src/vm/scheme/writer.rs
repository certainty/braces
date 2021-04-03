use super::value::Value;

pub struct Writer;

impl Writer {
    pub fn new() -> Self {
        Writer
    }

    pub fn write(&self, v: &Value) -> String {
        match v {
            Value::Bool(true) => "#t".to_string(),
            Value::Bool(false) => "#f".to_string(),
            Value::Symbol(sym) => self.write_symbol(&sym.as_str()),
            Value::Char(c) => self.write_char(*c),
            Value::String(s) => self.write_string(&s),
            Value::ProperList(elts) => {
                let body: Vec<String> = elts.iter().map(|e| self.write(&e)).collect();

                format!("'({})", body.join(" "))
            }
            Value::Unspecified => "#<unspecified>".to_string(),
        }
    }

    fn write_symbol(&self, sym: &str) -> String {
        let mut requires_delimiter = false;
        let mut external = String::new();

        for c in sym.chars() {
            if !char::is_ascii(&c) {
                requires_delimiter = true;
                external.push_str(&format!("\\x{:x};", c as u32));
            } else if char::is_whitespace(c) {
                requires_delimiter = true;
                external.push(c);
            } else {
                external.push(c);
            }
        }

        if requires_delimiter {
            format!("'|{}|", external)
        } else {
            format!("'{}", external)
        }
    }

    fn write_char(&self, c: char) -> String {
        let mut b = [0; 4];
        let external = match c as u32 {
            0x0 => "null",
            0x7 => "alarm",
            0x8 => "backspace",
            0x18 => "delete",
            0x1b => "escape",
            0x20 => "space",
            0xa => "newline",
            0xb => "tab",
            0xd => "return",
            _ => c.encode_utf8(&mut b),
        };

        format!("#\\{}", external)
    }

    fn write_string(&self, s: &String) -> String {
        let mut external = String::from("\"");

        for c in s.chars() {
            match c {
                '\n' => external.push_str("\\n"),
                '\r' => external.push_str("\\r"),
                '\t' => external.push_str("\\t"),
                '"' => external.push_str("\\\""),
                '\\' => external.push_str("\\\\"),
                _ => external.push(c),
            }
        }
        external.push('"');
        external
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::source::StringSource;
    use crate::vm::scheme::reader;
    use crate::vm::scheme::value::arbitrary;
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
        )
    }

    #[test]
    fn test_write_proper_list() {
        let writer = Writer;
        let elts = vec![Value::boolean(true), Value::boolean(false)];
        let ls = Value::proper_list(elts);

        assert_eq!(writer.write(&ls), "'(#t #f)");
    }

    #[quickcheck]
    fn test_read_is_write_inverse(val: Value) -> bool {
        let reader = reader::Reader::new();
        let writer = writer::Writer::new();
        let external = writer.write(&val);
        let mut source = StringSource::new(&external, "");
        let cpy = val.clone();

        println!("Ext: {}", external);

        match reader.read(&mut source) {
            Ok(v) if v == Some(val) => true,
            Ok(v) => {
                println!("Values aren't equal {:?} {:?}", v, cpy);
                false
            }
            Err(e) => {
                eprintln!("Error {}", e);
                false
            }
        }
    }
}
