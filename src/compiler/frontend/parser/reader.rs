use super::lowlevel;
use super::source;
use super::syntax;
use pest::iterators::Pair;
use pest::iterators::Pairs;
use thiserror::Error;

// TODO: clean this up - too many unclear error variants
#[derive(Error, Debug)]
pub enum ReaderError {
    #[error("Generic reader error")]
    Generic(String),

    #[error("Read error")]
    ReadError { source: std::io::Error },

    #[error(transparent)]
    IoError(#[from] std::io::Error),

    #[error("Parse Error")]
    ParseError(#[from] pest::error::Error<lowlevel::Rule>),

    #[error("Unsupported Syntax")]
    UnsupportedSyntax(String),
}

type Result<T> = std::result::Result<T, ReaderError>;

pub fn read_datum<T: source::Source>(input: &mut T) -> Result<Option<syntax::Syntax>> {
    let mut ast = read_program(input)?;
    Ok(ast.pop())
}

pub fn read_program<T: source::Source>(input: &mut T) -> Result<Vec<syntax::Syntax>> {
    let mut buffer = String::new();
    input.read_to_string(&mut buffer)?;

    let mut program = lowlevel::parse_program(&buffer)?;
    let ast = to_ast_seq(&mut program)?;
    Ok(ast)
}

fn to_ast_seq(pairs: &mut Pairs<lowlevel::Rule>) -> Result<Vec<syntax::Syntax>> {
    pairs.map(to_ast).collect()
}

fn to_ast(pair: Pair<lowlevel::Rule>) -> Result<syntax::Syntax> {
    match pair.as_rule() {
        lowlevel::Rule::number => match pair.as_str().parse() {
            Ok(num) => Ok(syntax::fixnum(num)),
            Err(_e) => Err(ReaderError::Generic(String::from("Couldn't parse fixnum"))),
        },
        lowlevel::Rule::BOOL_TRUE => Ok(syntax::boolean(true)),
        lowlevel::Rule::BOOL_FALSE => Ok(syntax::boolean(false)),
        lowlevel::Rule::IDENTIFIER => Ok(syntax::symbol(pair.as_str())),
        lowlevel::Rule::DELIMITED_IDENTIFIER => {
            let s = pair.as_str();
            Ok(syntax::symbol(&s[1..s.len() - 1]))
        }
        lowlevel::Rule::PECULIAR_IDENTIFIER => Ok(syntax::symbol(pair.as_str())),
        lowlevel::Rule::vector => {
            let elements: Result<Vec<syntax::Syntax>> = pair.into_inner().map(to_ast).collect();
            Ok(syntax::vector(elements?))
        }
        lowlevel::Rule::proper_list => {
            let elements: Result<Vec<syntax::Syntax>> = pair.into_inner().map(to_ast).collect();
            Ok(syntax::proper_list(elements?))
        }
        lowlevel::Rule::improper_list => {
            let mut elements = pair.into_inner();
            let head = elements.next().unwrap();
            let tail = elements.next().unwrap();
            let head_elements: Result<Vec<syntax::Syntax>> =
                head.into_inner().map(to_ast).collect();
            let tail_element = to_ast(tail.into_inner().next().unwrap());

            Ok(syntax::improper_list(head_elements?, tail_element?))
        }
        synt => Err(ReaderError::UnsupportedSyntax(String::from(&format!(
            "{:?}",
            synt
        )))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_bug_parsing_numbers() {
        assert_eq!(
            read("(10 10 10)").unwrap(),
            Some(syntax::proper_list(vec![
                syntax::fixnum(10),
                syntax::fixnum(10),
                syntax::fixnum(10)
            ]))
        )
    }

    #[test]
    pub fn test_read_number() {
        assert_eq!(read("42").unwrap(), Some(syntax::fixnum(42)));
    }

    #[test]
    pub fn test_read_bool_true() {
        assert_eq!(read("#t").unwrap(), Some(syntax::boolean(true)));
        assert_eq!(read("#true").unwrap(), Some(syntax::boolean(true)))
    }

    #[test]
    pub fn test_read_symbol() {
        assert_eq!(read("foo").unwrap(), Some(syntax::symbol("foo")))
    }

    #[test]
    pub fn test_read_delimited_symbol() {
        assert_eq!(
            read("|complicated symbol foo|").unwrap(),
            Some(syntax::symbol("complicated symbol foo"))
        )
    }

    #[test]
    pub fn test_read_vector() {
        assert_eq!(
            read("#(10 #t)").unwrap(),
            Some(syntax::vector(vec![
                syntax::fixnum(10),
                syntax::boolean(true)
            ]))
        );

        assert_eq!(read("#()").unwrap(), Some(syntax::vector(vec![])))
    }

    #[test]
    pub fn test_read_proper_list() {
        assert_eq!(
            read("(10 foo)").unwrap(),
            Some(syntax::proper_list(vec![
                syntax::fixnum(10),
                syntax::symbol("foo")
            ]))
        );

        assert_eq!(
            read("((10 foo))").unwrap(),
            Some(syntax::proper_list(vec![syntax::proper_list(vec![
                syntax::fixnum(10),
                syntax::symbol("foo")
            ])]))
        )
    }

    #[test]
    pub fn test_read_improper_list() {
        assert_eq!(
            read("(10 10 . foo)").unwrap(),
            Some(syntax::improper_list(
                vec![syntax::fixnum(10), syntax::fixnum(10)],
                syntax::symbol("foo")
            ))
        )
    }

    #[test]
    pub fn test_read_comments() {
        assert_eq!(
            read("#(10);just a test\n\n").unwrap(),
            Some(syntax::vector(vec![syntax::fixnum(10)]))
        );

        assert_eq!(
            read(";just a test\n\n#(10)").unwrap(),
            Some(syntax::vector(vec![syntax::fixnum(10)]))
        )
    }

    fn read(inp: &str) -> Result<Option<syntax::Syntax>> {
        let mut source: source::StringSource = inp.into();
        read_datum(&mut source)
    }
}
