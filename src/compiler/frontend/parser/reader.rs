use super::lowlevel;
use super::source;
use super::syntax;
use super::Location;
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
    let loc = location(&pair);

    match pair.as_rule() {
        lowlevel::Rule::number => match pair.as_str().parse() {
            Ok(num) => Ok(syntax::fixnum(num, loc)),
            Err(_e) => Err(ReaderError::Generic(String::from("Couldn't parse fixnum"))),
        },
        lowlevel::Rule::BOOL_TRUE => Ok(syntax::boolean(true, loc)),
        lowlevel::Rule::BOOL_FALSE => Ok(syntax::boolean(false, loc)),
        lowlevel::Rule::IDENTIFIER => Ok(syntax::symbol(pair.as_str(), loc)),
        lowlevel::Rule::DELIMITED_IDENTIFIER => {
            let s = pair.as_str();
            Ok(syntax::symbol(&s[1..s.len() - 1], loc))
        }
        lowlevel::Rule::PECULIAR_IDENTIFIER => Ok(syntax::symbol(pair.as_str(), loc)),
        lowlevel::Rule::vector => {
            let elements: Result<Vec<syntax::Syntax>> = pair.into_inner().map(to_ast).collect();
            Ok(syntax::vector(elements?, loc))
        }
        lowlevel::Rule::proper_list => {
            let elements: Result<Vec<syntax::Syntax>> = pair.into_inner().map(to_ast).collect();
            Ok(syntax::proper_list(elements?, loc))
        }
        lowlevel::Rule::improper_list => {
            let mut elements = pair.into_inner();
            let head = elements.next().unwrap();
            let tail = elements.next().unwrap();
            let head_elements: Result<Vec<syntax::Syntax>> =
                head.into_inner().map(to_ast).collect();
            let tail_element = to_ast(tail.into_inner().next().unwrap());

            Ok(syntax::improper_list(head_elements?, tail_element?, loc))
        }
        synt => Err(ReaderError::UnsupportedSyntax(String::from(&format!(
            "{:?}",
            synt
        )))),
    }
}

// TODO: make location more useful than just the line
fn location(pair: &Pair<lowlevel::Rule>) -> Location {
    let span = pair.as_span();
    let start = span.start_pos();
    //let end = span.end_pos();
    let (line, _) = start.line_col();

    Location::new(line)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_location(line: usize) -> Location {
        Location::new(line)
    }

    #[test]
    pub fn test_bug_parsing_numbers() {
        assert_eq!(
            read("(10 10 10)").unwrap(),
            Some(syntax::proper_list(
                vec![
                    syntax::fixnum(10, make_location(1)),
                    syntax::fixnum(10, make_location(1)),
                    syntax::fixnum(10, make_location(1))
                ],
                make_location(1)
            ))
        )
    }

    #[test]
    pub fn test_read_number() {
        assert_eq!(
            read("42").unwrap(),
            Some(syntax::fixnum(42, make_location(1)))
        );
    }

    #[test]
    pub fn test_read_bool_true() {
        assert_eq!(
            read("#t").unwrap(),
            Some(syntax::boolean(true, make_location(1)))
        );
        assert_eq!(
            read("#true").unwrap(),
            Some(syntax::boolean(true, make_location(1)))
        )
    }

    #[test]
    pub fn test_read_symbol() {
        assert_eq!(
            read("foo").unwrap(),
            Some(syntax::symbol("foo", make_location(1)))
        )
    }

    #[test]
    pub fn test_read_delimited_symbol() {
        assert_eq!(
            read("|complicated symbol foo|").unwrap(),
            Some(syntax::symbol("complicated symbol foo", make_location(1)))
        )
    }

    #[test]
    pub fn test_read_vector() {
        assert_eq!(
            read("#(10 #t)").unwrap(),
            Some(syntax::vector(
                vec![
                    syntax::fixnum(10, make_location(1)),
                    syntax::boolean(true, make_location(1))
                ],
                make_location(1)
            ))
        );

        assert_eq!(
            read("#()").unwrap(),
            Some(syntax::vector(vec![], make_location(1)))
        )
    }

    #[test]
    pub fn test_read_proper_list() {
        assert_eq!(
            read("(10 foo)").unwrap(),
            Some(syntax::proper_list(
                vec![
                    syntax::fixnum(10, make_location(1)),
                    syntax::symbol("foo", make_location(1))
                ],
                make_location(1)
            ))
        );

        assert_eq!(
            read("((10 foo))").unwrap(),
            Some(syntax::proper_list(
                vec![syntax::proper_list(
                    vec![
                        syntax::fixnum(10, make_location(1)),
                        syntax::symbol("foo", make_location(1))
                    ],
                    make_location(1)
                )],
                make_location(1)
            ))
        )
    }

    #[test]
    pub fn test_read_improper_list() {
        assert_eq!(
            read("(10 10 . foo)").unwrap(),
            Some(syntax::improper_list(
                vec![
                    syntax::fixnum(10, make_location(1)),
                    syntax::fixnum(10, make_location(1))
                ],
                syntax::symbol("foo", make_location(1)),
                make_location(1)
            ))
        )
    }

    #[test]
    pub fn test_read_comments() {
        assert_eq!(
            read("#(10);just a test\n\n").unwrap(),
            Some(syntax::vector(
                vec![syntax::fixnum(10, make_location(1))],
                make_location(1)
            ))
        );

        assert_eq!(
            read(";just a test\n\n#(10)").unwrap(),
            Some(syntax::vector(
                vec![syntax::fixnum(10, make_location(3))],
                make_location(3)
            ))
        )
    }

    fn read(inp: &str) -> Result<Option<syntax::Syntax>> {
        let mut source: source::StringSource = inp.into();
        read_datum(&mut source)
    }
}
