#[cfg(test)]
pub mod expressions {
    use crate::compiler::frontend::reader::sexp;
    use crate::compiler::frontend::reader::sexp::datum::{Datum, Sexp};
    use crate::compiler::source::SourceType;
    use crate::compiler::source::StringSource;
    use crate::compiler::source_location::SourceLocation;

    pub const TEST_HELPER_BUFFER_NAME: &str = "expression-test-helper";

    pub fn parse_datum(inp: &str) -> Datum {
        let mut source = StringSource::new(inp, &TEST_HELPER_BUFFER_NAME);
        sexp::parse(&mut source).unwrap()
    }

    pub fn make_datum(sexp: Sexp, line: usize, col: usize) -> Datum {
        Datum::new(sexp, location(line, col))
    }

    pub fn location(line: usize, col: usize) -> SourceLocation {
        SourceLocation::new(
            SourceType::Buffer(TEST_HELPER_BUFFER_NAME.to_string()),
            line,
            col,
        )
    }
}
