// Macro expander for our little scheme
//
// The capability to have user defined syntactic extensions is one of the core
// features of our scheme (any lisp for that matter). It introduces a weird twist
// for us as the language implementer as well as a user. It hoists compilation and even
// execution of parts of the program to compile time, where code is applied to itself
// to transform the program.
//
// Now the macro expansion we use here has some limitations. Some are present in all
// macro systems and some are only present in this implementation because it made the
// compiler simpler.
//
// 1. Macros can only be used after they have been defined (they have to be lexically seen before their use)
// 2. Macro definitions may only use predefined procedures, so no user-defined aux procedures. This limitation sounds worse than it is
//    since all the machinery you need to build the macros you need will be pre-defined (all the list procedures and quasi-quotes, etc)
// 3. We implement explicit renaming macros which are so called procedural macros. Syntax rules may come later and can be defined in terms of ER macros.
//
//
// ## Technical notes
//
// Macro expansion interleaves the whole compilation chain and execution with parsing.
// When this phase is done, it yields and AST of containing only core-forms where every lambda has been alpha converted.
// We maintain metadata to increase the ergonomic properties of the compiler and give better error messages.

use crate::compiler::backend::variables::{Variables, VariablesRef};
use crate::compiler::frontend::parser::syntax::environment::Environment;
use crate::compiler::frontend::reader::sexp::datum::{Datum, Sexp};
use crate::compiler::Compiler;
use crate::vm::scheme::ffi;
use crate::vm::value::procedure;
use crate::vm::value::procedure::Arity;
use crate::vm::value::{self, Value};
use crate::vm::VM;
use thiserror::Error;

type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug, Clone)]
pub enum Error {
    #[error("InvalidTransformer")]
    InvalidTransformer,
    #[error("SyntaxError")]
    ExpansionError,
    #[error("CompileError")]
    CompileError,
    #[error("UnsupportedExpander")]
    UnsupportedExpander,
}

pub struct MacroExpander {
    compiler: Compiler,
    expansion_env: Environment,
    variables: VariablesRef,
}

impl MacroExpander {
    pub fn new() -> MacroExpander {
        MacroExpander {
            compiler: Compiler::new(),
            expansion_env: Environment::new(),
            variables: Variables::top_level(),
        }
    }

    // main function converts a program into another program
    // macro definitions and macro uses will be removed from the AST
    // TODO: should we return a core-ast already?
    pub fn expand(program: Vec<Datum>) -> Result<Vec<Datum>> {
        let mut expander = Self::new();
        expander.expand_all(program)
    }

    pub fn expand_all(&mut self, data: Vec<Datum>) -> Result<Vec<Datum>> {
        let mut expanded = vec![];

        for datum in data {
            if let Some(expanded_datum) = self.expand_datum(datum)? {
                expanded.push(expanded_datum)
            }
        }

        Ok(expanded)
    }

    fn expand_datum(&mut self, datum: Datum) -> Result<Option<Datum>> {
        // there are really 3 main cases
        // 1. it's a syntax definition => we compile the definition and register it in the current expansion env
        // 2. it's syntax use => we apply the compiled transformer to the ast as arguments and splice the result in
        // 3. it's neither of the above => we recursively walk the datum if it's not an atom

        // fast exit
        if datum.is_atom() {
            return Ok(Some(datum));
        }

        // now let's get to work
        match datum.sexp() {
            // macro use or definition?
            Sexp::List(ls) => {
                match &ls[..] {
                    [operator, tail @ ..] if operator.is_atom() => {
                        if self.is_macro_definition(operator) {
                            // compiler and register expander
                            self.define_macro(ls)?;
                            Ok(None) // remove from AST
                        } else if self.is_macro_use(operator) {
                            self.apply_macro(operator, &datum)
                        } else if self.is_quote(operator) {
                            Ok(Some(self.expand_quote(datum)?))
                        } else if self.is_lambda(operator) {
                            Ok(Some(self.expand_lambda(datum)?))
                        } else {
                            // check the rest of the list
                            let expanded_tail = self.expand_all(tail.to_vec())?;
                            let mut new_ls: Vec<Datum> = vec![operator.clone()];
                            new_ls.extend(expanded_tail.iter().cloned());

                            Ok(Some(Datum::new(Sexp::list(new_ls), datum.location.clone())))
                        }
                    }
                    _ => Ok(Some(datum)),
                }
            }
            _ => Ok(Some(datum)),
        }
    }

    // check if the identifier is bound in the current environment
    fn is_rec_bound(&self, id: &str) -> bool {
        false
    }

    fn is_macro_definition(&self, op: &Datum) -> bool {
        match Self::match_symbol(op) {
            Some("define-syntax") if !self.is_rec_bound("define-syntax") => true,
            // local macro definitions come later
            // Some("let-syntax") if !self.is_rec_bound("define-syntax") => true,
            // Some("letrec-syntax") if !self.is_rec_bound("letrec-syntax") => true,
            _ => false,
        }
    }

    fn is_macro_use(&self, op: &Datum) -> bool {
        match Self::match_symbol(op) {
            Some(name) => self.expansion_env.is_bound(&name.to_string()),
            _ => false,
        }
    }

    fn is_lambda(&self, op: &Datum) -> bool {
        match Self::match_symbol(op) {
            Some("lambda") if !self.is_rec_bound("lambda") => true,
            _ => false,
        }
    }

    fn is_quote(&self, op: &Datum) -> bool {
        match Self::match_symbol(op) {
            Some("quote") if !self.is_rec_bound("quote") => true,
            _ => false,
        }
    }

    fn define_macro(&mut self, datum: &[Datum]) -> Result<()> {
        match datum {
            // (define-syntax foo (er-macro-transformer (lambda (exp rename compare?) ...))
            [_macro_def, name, def] => match (name.sexp(), def.sexp()) {
                (Sexp::Symbol(name), Sexp::List(ls)) => match &ls[..] {
                    [expander, lambda_def] => {
                        if let Some("er-macro-transformer") = Self::match_symbol(expander) {
                            println!("Compiling lambda def: {:#?}", lambda_def);
                            match self.compiler.compile_lambda(lambda_def) {
                                Ok(procedure) => {
                                    println!("Transformer closure: {:#?}", procedure);
                                    //self.expansion_env
                                    //   .bind(name.clone(), Value::Procedure(procedure));
                                    Ok(())
                                }
                                Err(_) => Err(Error::CompileError),
                            }
                        } else {
                            Err(Error::UnsupportedExpander)
                        }
                    }
                    _ => Err(Error::ExpansionError),
                },
                _ => Err(Error::ExpansionError),
            },
            _ => Err(Error::ExpansionError),
        }
    }

    fn expand_lambda(&mut self, lambda: Datum) -> Result<Datum> {
        Ok(lambda)
    }

    fn expand_quote(&mut self, quote: Datum) -> Result<Datum> {
        Ok(quote)
    }

    fn apply_macro(&mut self, name: &Datum, form: &Datum) -> Result<Option<Datum>> {
        match Self::match_symbol(name) {
            Some(name) => {
                let transformer = self.expansion_env.get(&name.to_string()).clone();
                todo!()

                /*
                match transformer {
                    Some(Value::Procedure(procedure)) => {
                        self.run_transformer(procedure.clone(), form)
                    }
                    _ => Err(Error::InvalidTransformer),
                }*/
            }
            other => {
                println!("{:#?}", other);
                Err(Error::ExpansionError)
            }
        }
    }

    fn run_transformer(
        &mut self,
        transformer: value::procedure::Procedure,
        exp: &Datum,
    ) -> Result<Option<Datum>> {
        let mut vm = VM::for_macro_expansion();
        match vm.interprete_macro_transformer(transformer, self.renamer(), self.comparator(), exp) {
            Ok(v) => Ok(Some(v)),
            Err(e) => {
                eprintln!("OH OH: {}", e);
                Err(Error::ExpansionError)
            }
        }
    }

    fn renamer(&self) -> value::procedure::Procedure {
        let renamer_proc = value::procedure::foreign::Procedure::new(
            "rename",
            |_args| Ok(Value::Bool(true)),
            Arity::Exactly(1),
        );

        value::procedure::Procedure::foreign(renamer_proc)
    }

    fn comparator(&self) -> value::procedure::Procedure {
        let compare_proc = value::procedure::foreign::Procedure::new(
            "compare",
            |_args| Ok(Value::Bool(false)),
            Arity::Exactly(2),
        );

        value::procedure::Procedure::foreign(compare_proc)
    }

    fn match_symbol<'a>(datum: &'a Datum) -> Option<&'a str> {
        match datum.sexp() {
            Sexp::Symbol(s) => Some(s.as_str()),
            _ => None,
        }
    }
}
