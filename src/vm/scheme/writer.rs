use super::value::Value;

pub struct Writer;

impl Writer {
    pub fn external_representation(&self, v: &Value) -> String {
        match v {
            Value::Bool(true) => "#t".to_string(),
            Value::Bool(false) => "#f".to_string(),
            Value::Symbol(sym) => self.write_symbol(&sym.as_str()),
            Value::Char(c) => todo!(),
            Value::ProperList(elts) => {
                let body: Vec<String> = elts
                    .iter()
                    .map(|e| self.external_representation(&e))
                    .collect();

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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_write_bool() {
        let writer = Writer;

        assert_eq!(writer.external_representation(&Value::Bool(true)), "#t");
        assert_eq!(writer.external_representation(&Value::Bool(false)), "#f");
    }

    #[test]
    fn test_write_symbol() {
        let writer = Writer;
        assert_eq!(
            writer.external_representation(&Value::Symbol("...".to_string())),
            "'..."
        );

        assert_eq!(
            writer.external_representation(&Value::symbol(&"foo bar".to_string())),
            "'|foo bar|"
        );

        assert_eq!(
            writer.external_representation(&Value::symbol(&"foo 💣 bar".to_string())),
            "'|foo \\x1f4a3; bar|"
        )
    }

    #[test]
    fn test_write_proper_list() {
        let writer = Writer;
        let elts = vec![Value::boolean(true), Value::boolean(false)];
        let ls = Value::proper_list(elts);

        assert_eq!(writer.external_representation(&ls), "'(#t #f)");
    }
}
