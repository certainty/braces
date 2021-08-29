use super::{Error, Expander, Result};
use crate::compiler::frontend::reader::{datum::Datum, sexp::SExpression};
use crate::compiler::frontend::syntax::environment::{Denotation, Special};
use crate::compiler::frontend::syntax::symbol::Symbol;
use crate::compiler::source::{HasSourceLocation, Location};

pub enum QuoteJoin {
    Cons(Datum),
    Append(Datum),
}

impl Expander {
    pub fn expand_quasi_quotation(
        &mut self,
        datum: &Datum,
        _operator: &Datum,
        operands: &[Datum],
    ) -> Result<Datum> {
        if operands.len() != 1 {
            return Err(Error::expansion_error(
                "Invalid quasi-quoted expression",
                &datum,
            ));
        }

        let quasi_quoted_sexp = operands[0].clone();
        match quasi_quoted_sexp.s_expression() {
            // (quasi-quote (..))
            SExpression::List(elements) => {
                self.expand_quasi_quoted_list(elements, datum.source_location().clone())
            }
            // (quasi-quote (_ . _))
            SExpression::ImproperList(head, tail) => {
                self.expand_quasi_quoted_improper_list(head, tail, datum.source_location().clone())
            }
            // (quasi-quote #(..))
            SExpression::Vector(elements) => {
                self.expand_quasi_quoted_vector(elements, datum.source_location().clone())
            }
            // (quasi-quote datum)
            _ => Ok(self.quote_datum(quasi_quoted_sexp)),
        }
    }

    fn expand_quasi_quoted_list(&mut self, elements: &Vec<Datum>, loc: Location) -> Result<Datum> {
        let expanded: Result<Vec<QuoteJoin>> = elements
            .iter()
            .map(|e| self.expand_quasi_quoted_element(e))
            .collect();
        let identity = self.empty_list(loc.clone());
        let result = expanded?
            .iter()
            .rev()
            .fold(identity, |tail: Datum, e| match e {
                QuoteJoin::Cons(e) => self.build_apply(
                    Symbol::unforgeable("cons"),
                    vec![e.clone(), tail],
                    loc.clone(),
                ),
                QuoteJoin::Append(e) => self.build_apply(
                    Symbol::unforgeable("append"),
                    vec![e.clone(), tail],
                    loc.clone(),
                ),
            });
        Ok(result)
    }

    fn expand_quasi_quoted_vector(
        &mut self,
        elements: &Vec<Datum>,
        loc: Location,
    ) -> Result<Datum> {
        let expanded: Result<Vec<QuoteJoin>> = elements
            .iter()
            .map(|e| self.expand_quasi_quoted_element(e))
            .collect();

        let identity = self.empty_vector(loc.clone());
        let result = expanded?
            .iter()
            .rev()
            .fold(identity, |tail: Datum, e| match e {
                QuoteJoin::Cons(e) => self.build_apply(
                    Symbol::unforgeable("vector-cons"),
                    vec![e.clone(), tail],
                    loc.clone(),
                ),
                QuoteJoin::Append(e) => self.build_apply(
                    Symbol::unforgeable("vector-append"),
                    vec![e.clone(), tail],
                    loc.clone(),
                ),
            });
        Ok(result)
    }

    fn expand_quasi_quoted_improper_list(
        &mut self,
        head: &Vec<Datum>,
        tail: &Box<Datum>,
        _loc: Location,
    ) -> Result<Datum> {
        let expanded_head: Result<Vec<QuoteJoin>> = head
            .iter()
            .map(|e| self.expand_quasi_quoted_element(e))
            .collect();

        let expanded_tail = match self.expand_quasi_quoted_element(tail)? {
            QuoteJoin::Cons(d) => d,
            QuoteJoin::Append(d) => d,
        };

        let result = expanded_head?
            .iter()
            .rev()
            .fold(expanded_tail, |tail: Datum, e| match e {
                QuoteJoin::Cons(e) => self.build_apply(
                    Symbol::unforgeable("cons"),
                    vec![e.clone(), tail],
                    e.source_location().clone(),
                ),
                QuoteJoin::Append(e) => self.build_apply(
                    Symbol::unforgeable("append"),
                    vec![e.clone(), tail],
                    e.source_location().clone(),
                ),
            });

        Ok(result)
    }

    // unquote an element in a quasi-quoted list/cons/vector
    fn expand_quasi_quoted_element(&mut self, datum: &Datum) -> Result<QuoteJoin> {
        match datum.list_slice() {
            Some([operator, operand]) if operator.is_symbol() => {
                match self.denotation_of(operator)? {
                    Denotation::Special(Special::QuasiQuote) => Err(Error::expansion_error(
                        "quasi-quote can't be nested",
                        &datum,
                    )),
                    Denotation::Special(Special::Unquote) => {
                        Ok(QuoteJoin::Cons(self.expand(operand)?))
                    }
                    Denotation::Special(Special::UnquoteSplicing) => {
                        let expanded = self.expand(operand)?;
                        Ok(QuoteJoin::Append(expanded))
                    }
                    _ => Ok(QuoteJoin::Cons(datum.clone())),
                }
            }
            _ => Ok(QuoteJoin::Cons(self.quote_datum(datum.clone()))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::tests::*;
    use super::super::Result;

    #[test]
    fn test_expand_quasi_quote() -> Result<()> {
        assert_expands_equal("`3", "'3", false)?;
        assert_expands_equal("`(3 4 ,5)", "(cons '3 (cons '4 (cons 5 '())))", false)?;
        assert_expands_equal(
            "`(3 4 ,@(list 5 6 7 8) 9 10)",
            "(cons '3 (cons '4 (append (list 5 6 7 8) (cons '9 (cons '10 '())))))",
            false,
        )?;
        assert_expands_equal(
            "`(3 4 ,(car x y))",
            "(cons '3 (cons '4 (cons (car x y) '())))",
            false,
        )?;
        assert_expands_equal(
            "`(3 4 ,'(1 2))",
            "(cons '3 (cons '4 (cons '(1 2) '())))",
            false,
        )?;
        assert_expands_equal("''foo", "'(quote foo)", false)?;

        assert_expands_equal("`(1 . 2)", "(cons '1 '2)", false)?;
        assert_expands_equal("`(1 2 . 3)", "(cons '1 (cons '2 '3)) ", false)?;
        assert_expands_equal("`(1 . ,(list 2 2))", "(cons '1 (list 2 2))", false)?;

        assert_expands_equal("`(1 . ,(list 2 2))", "(cons '1 (list 2 2))", false)?;

        //assert_expands_equal("`#(1 2)", "(vector-cons '1 (vector-cons '2 #()))", false)?;

        assert!(expand_form("`(1 2 `3)").is_err(), "expected error");
        Ok(())
    }
}
