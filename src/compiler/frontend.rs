pub mod expander;
pub mod parser;
pub mod reader;
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

// Read sexps as programs and create an AST of expressions
pub fn parse<T: Source>(source: &mut T) -> std::result::Result<Ast, parser::Error> {
    // TODO: use expander instead (which in turn parses to the core scheme forms)
    parser::parse(&read(source)?)
}

pub fn parse_all<T: Source>(source: &mut T) -> std::result::Result<Ast, parser::Error> {
    // TODO: use expander instead (which in turn parses to the core scheme forms)
    parser::parse_all(read_all(source)?)
}
