use super::Value;
use quickcheck::Arbitrary;

#[derive(Clone, Debug)]
struct SymbolString(String);

impl Arbitrary for Value {
    fn arbitrary(gen: &mut quickcheck::Gen) -> Self {
        match gen.choose(&[1, 2, 3, 4, 5]) {
            Some(1) => Value::boolean(bool::arbitrary(gen)),
            Some(2) => Value::character(char::arbitrary(gen)),
            Some(3) => Value::string(&String::arbitrary(gen)),
            Some(4) => Value::symbol(&SymbolString::arbitrary(gen).0),
            _ => Value::boolean(false),
        }
    }
}

impl Arbitrary for SymbolString {
    fn arbitrary(gen: &mut quickcheck::Gen) -> Self {
        let problems = ["", "one two", "\t", "test with \\| escaped vertical lines"];

        match gen.choose(&problems) {
            Some(v) => SymbolString(v.to_owned().to_string()),
            None => SymbolString("foo".to_owned()),
        }
    }
}
