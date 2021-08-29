use crate::compiler::frontend;
use crate::compiler::frontend::error::{Detail, Error};
use crate::compiler::frontend::parser::Expression;
use crate::compiler::frontend::reader::{datum::Datum, sexp::SExpression};
use crate::compiler::frontend::syntax;
use crate::compiler::frontend::syntax::environment::{Denotation, Special};
use crate::compiler::source::HasSourceLocation;

#[derive(Debug)]
pub struct CoreParser {
    pub environment: syntax::environment::SyntaxEnvironment,
}

impl CoreParser {
    pub fn new() -> Self {
        Self {
            environment: syntax::environment::SyntaxEnvironment::basic(),
        }
    }

    /// Parse a single datum into an expression
    ///
    ///
    /// Ref: r7rs 7.1.3
    /// ```grammar
    /// <expression> =>
    ///   <identifier>         |
    ///   <literal>            |
    ///   <procedure call>     |
    ///   <lambda expression>  |
    ///   <conditional>        |
    ///   <assignment>         |
    ///   <derived expression> |
    ///   <macro use>          |
    ///   <macro block>        |
    ///   <includer>           |
    /// ```

    pub fn parse(&mut self, datum: &Datum) -> frontend::Result<Expression> {
        log::trace!("parsing to core: {}", datum.to_string());

        match datum.s_expression() {
            SExpression::List(ls) => match &ls[..] {
                [operator, _operands @ ..] if operator.is_symbol() => {
                    let denotation = self.denotation_of(operator)?;
                    match denotation {
                        Denotation::Special(special) => self.parse_special(special, &datum),
                        Denotation::Global(_) => self.parse_apply(&datum).res(),
                        Denotation::Id => self.parse_apply(&datum).res(),
                        _ => {
                            return Err(Error::bug(&format!(
                                "Unexpected denotation for datum: {:?}",
                                denotation.clone()
                            )))
                        }
                    }
                }
                [_operator, _operands @ ..] => self.parse_apply(&datum).res(),
                _ => Err(Error::parse_error(
                    "Unexpected unquoted list",
                    Detail::new("found unquoted list", datum.source_location().clone()),
                    vec![],
                )),
            },
            sexp if sexp.is_symbol() => self.parse_identifier(&datum).res(),
            _lit => self.parse_literal(&datum).res(),
        }
    }

    fn parse_special(
        &mut self,
        special_form: Special,
        datum: &Datum,
    ) -> frontend::Result<Expression> {
        match special_form {
            Special::Define => self.parse_definition(&datum).res(),
            Special::Quote => self.parse_quote(&datum).res(),
            Special::Lambda => self.parse_lambda(&datum).res(),
            Special::Set => self.parse_set(&datum).res(),
            Special::Begin => self.parse_begin(&datum).res(),
            Special::If => self.parse_if(&datum).res(),
            _ => {
                return Err(Error::bug(&format!(
                    "Unexpected special form: {:?}",
                    special_form
                )))
            }
        }
    }

    pub fn denotation_of(&mut self, datum: &Datum) -> frontend::Result<Denotation> {
        if let SExpression::Symbol(sym) = datum.s_expression() {
            Ok(self.environment.get(&sym.clone().into()))
        } else {
            Err(Error::bug("unexpected symbol to determine denotation"))
        }
    }

    pub fn parse_list<'a>(&mut self, datum: &'a Datum) -> frontend::Result<&'a [Datum]> {
        log::trace!("trying to parse list: {:?}", datum);
        match datum.s_expression() {
            SExpression::List(ls) => Ok(&ls[..]),
            _ => Err(Error::parse_error(
                "Expected list",
                Detail::new("expected list", datum.source_location().clone()),
                vec![],
            )),
        }
    }
}
