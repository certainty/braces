use super::Value;

use quickcheck::Arbitrary;

#[derive(Clone, Debug)]
pub struct SymbolString(pub String);

impl Arbitrary for Value {
    fn arbitrary(gen: &mut quickcheck::Gen) -> Self {
        let factory = super::Factory::default();

        match gen.choose(&[1, 2, 3, 4, 5]) {
            Some(1) if bool::arbitrary(gen) => factory.bool_true().clone(),
            Some(1) => factory.bool_false().clone(),
            Some(2) => factory.character(char::arbitrary(gen)).clone(),
            Some(3) => factory.string(&String::arbitrary(gen)).clone(),
            _ => factory.bool_false().clone(),
        }
    }
}

impl Arbitrary for SymbolString {
    fn arbitrary(gen: &mut quickcheck::Gen) -> Self {
        let problems = [
            "",
            "one two",
            "\t",
            "test with \\| escaped vertical lines",
            "foo \x20; bar",
        ];

        if let Some(true) = gen.choose(&[true, false]) {
            match gen.choose(&problems) {
                Some(v) => SymbolString(v.to_owned().to_string()),
                None => SymbolString(String::arbitrary(gen)),
            }
        } else {
            SymbolString(String::arbitrary(gen))
        }
    }
}
