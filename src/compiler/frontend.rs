pub mod expand_only;
pub mod expander;
pub mod parser;
pub mod reader;

#[cfg(test)]
pub mod test_helpers;

use crate::compiler::frontend::parser::ParserContext;
use crate::compiler::source::Source;
use parser::ast::Ast;
use reader::sexp::datum::Datum;

// Read sexps as data
pub fn read<T: Source>(source: &mut T) -> std::result::Result<Datum, reader::Error> {
    reader::parse(source)
}

pub fn read_all<T: Source>(source: &mut T) -> std::result::Result<Vec<Datum>, reader::Error> {
    reader::parse_sequence(source)
}

pub fn parse<T: Source>(source: &mut T) -> std::result::Result<Ast, parser::Error> {
    let mut ctx = ParserContext::default();
    let sexps = read(source)?;
    let parsed = parser::parse(sexps, &mut ctx)?;

    Ok(Ast {
        expressions: vec![parsed],
    })
}

pub fn parse_all<T: Source>(source: &mut T) -> std::result::Result<Ast, parser::Error> {
    let mut ctx = ParserContext::default();
    parser::parse_all(read_all(source)?, &mut ctx)
}
