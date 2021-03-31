use super::error::Error;
use crate::compiler::source::{Source, SourceType};
use crate::compiler::source_location::SourceLocation;
use pest::iterators::{Pair, Pairs};
use pest::Parser;

type Result<T> = std::result::Result<T, Error>;

pub enum Datum {
    Boolean(bool, SourceLocation),
}

impl Datum {
    pub fn parse(source: &mut dyn Source) -> Result<Option<Datum>> {
        let source_type = source.source_type();
        let mut buffer = String::new();
        source.read_to_string(&mut buffer)?;

        match DataParser::parse(Rule::datum, &buffer) {
            Ok(pair) => {
                let mut ast = Self::to_ast_seq(pair, &source_type)?;
                Ok(ast.pop())
            }
            Err(e) => Error::syntax_error(&format!("{}", e), source_type.clone()),
        }
    }

    pub fn boolean(value: bool, source_location: SourceLocation) -> Datum {
        Self::Boolean(value, source_location)
    }

    fn to_ast_seq(pairs: Pairs<Rule>, source_type: &SourceType) -> Result<Vec<Datum>> {
        pairs.map(|p| Self::to_ast(&p, source_type)).collect()
    }

    fn to_ast(pair: &Pair<Rule>, source_type: &SourceType) -> Result<Datum> {
        let loc = Self::create_location(&pair, source_type);

        match pair.as_rule() {
            Rule::BOOL_TRUE => Ok(Self::boolean(true, loc)),
            Rule::BOOL_FALSE => Ok(Self::boolean(false, loc)),
            _ => Error::syntax_error("Unsupported external representation", source_type.clone()),
        }
    }

    fn create_location(pair: &Pair<Rule>, source_type: &SourceType) -> SourceLocation {
        let span = pair.as_span();
        let start = span.start_pos();
        let (line, col) = start.line_col();

        SourceLocation::new(source_type.clone(), line, col)
    }
}

#[derive(Parser)]
#[grammar = "compiler/frontend/parser/datum.pest"]
struct DataParser;
