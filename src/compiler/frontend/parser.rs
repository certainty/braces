pub mod expression;
pub mod sexp;
use crate::compiler::source::Source;
use expression::error::Error;
use rustc_hash::FxHashMap;
use sexp::datum::{Datum, Sexp};
use sexp::error::ReadError;

type Result<T> = std::result::Result<T, Error>;

pub struct Parser;

impl Parser {
    pub fn parse_datum<T: Source>(&self, source: &mut T) -> std::result::Result<Datum, ReadError> {
        sexp::parse(source)
    }

    pub fn parse_datum_sequence<T: Source>(
        &self,
        source: &mut T,
    ) -> std::result::Result<Vec<Datum>, ReadError> {
        sexp::parse_sequence(source)
    }

    pub fn parse_program<T: Source>(&self, source: &mut T) -> Result<Vec<expression::Expression>> {
        expression::Expression::parse_program(source)
    }

    pub fn parse_expression<T: Source>(&self, source: &mut T) -> Result<expression::Expression> {
        expression::Expression::parse_one(source)
    }
}

pub struct AlphaConversionPhase {
    syntax_env: FxHashMap<String, bool>,
}

impl AlphaConversionPhase {
    pub fn new() -> Self {
        Self {
            syntax_env: FxHashMap::default(),
        }
    }

    pub fn apply(&self, sexps: Vec<Datum>) -> Result<Vec<expression::Expression>> {
        sexps.iter().map(|e| self.analyze(e)).collect()
    }

    fn analyze(&self, datum: &Datum) -> Result<expression::Expression> {
        match datum.sexp() {
            Sexp::List(ls) => match &ls[..] {
                [op, args @ ..] => match &op.sexp {
                    Sexp::Symbol(s) if self.is_syntax(&s) => {
                        let id = expression::identifier::Identifier::new(s, op.location.clone());

                        Ok(expression::Expression::macro_use(
                            id,
                            args.to_vec(),
                            datum.location.clone(),
                        ))
                    }

                    _ => todo!(),
                },
                _ => todo!(),
            },
            _ => todo!(),
        }
    }

    fn head_symbol<'a>(ls: &'a Vec<Datum>) -> Option<&'a str> {
        match ls.first().map(|e| e.sexp()) {
            Some(Sexp::Symbol(s)) => Some(s.as_str()),
            _ => None,
        }
    }

    fn is_syntax(&self, identifier: &str) -> bool {
        self.syntax_env.contains_key(&String::from(identifier))
    }
}
