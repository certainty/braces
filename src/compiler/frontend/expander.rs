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
// ## Technical notes
//
// Macro expansion interleaves the whole compilation chain and execution with parsing.
// When this phase is done, it yields and AST of containing only core-forms where every lambda has been alpha converted.
// We maintain metadata to increase the ergonomic properties of the compiler and give better error messages.
//
// ## Implementation and design
// The expander is essentially a port of larcancie's macro expander: https://github.com/larcenists/larceny/blob/master/src/Compiler/expand.sch
// At least it borrows the same data structures and algorithms. Of course they are translated to a version that fits rust

use crate::compiler::frontend::parser::syntax::environment::Denotation;
use crate::compiler::frontend::parser::syntax::environment::{Renamer, SyntaxEnvironment};
use crate::compiler::frontend::parser::syntax::Transformer;
use crate::compiler::Expression;
use crate::vm::scheme::ffi::FunctionResult;

use crate::compiler::frontend::parser::syntax::Symbol;
use crate::compiler::frontend::reader::sexp::datum::{Datum, Sexp};
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};
use crate::vm::value::procedure::Arity;
use crate::vm::value::{self, Value};
use crate::vm::VM;
use thiserror::Error;

type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug, Clone)]
pub enum Error {
    #[error("InvalidTransformer")]
    InvalidTransformer,
    #[error("ExpansionError: {} {:#?}", 0, 1)]
    ExpansionError(String, Datum),
    #[error("CompileError")]
    CompileError,
    #[error("UnsupportedExpander")]
    UnsupportedExpander,
    #[error("ExpanderBug")]
    ExpanderBug(String),
}

impl Error {
    pub fn expansion_error<T: Into<String>, Out>(msg: T, datum: Datum) -> Result<Out> {
        Err(Error::ExpansionError(msg.into(), datum))
    }

    pub fn bug<T: Into<String>, Out>(msg: T) -> Result<Out> {
        Err(Error::ExpanderBug(msg.into()))
    }
}

pub struct MacroExpander {
    renamer: Renamer,
}

impl MacroExpander {
    pub fn new() -> MacroExpander {
        MacroExpander {
            renamer: Renamer::new(),
        }
    }

    pub fn register_transformers(&self, env: &mut SyntaxEnvironment) {
        let transformer = Transformer::ExplicitRenaming(value::procedure::Procedure::foreign(
            value::procedure::foreign::Procedure::new(
                "transformer_let",
                Self::let_transformer,
                Arity::Exactly(3),
            ),
        ));
        env.extend(Symbol::forged("let"), Denotation::Macro(transformer))
    }

    pub fn expand(&mut self, data: &Datum, transformer: Transformer) -> Result<Datum> {
        match transformer {
            Transformer::ExplicitRenaming(proc) => self.run_transformer(proc, &data),
        }
    }

    pub fn expand_define(&mut self, datum: &Datum) -> Result<Datum> {
        match datum.sexp() {
            Sexp::List(ls) => match &ls[..] {
                //(define (<IDENTIFIER> <def formals>) <body>)
                [define, definition, exprs @ ..] if definition.sexp().is_proper_list() => {
                    self.expand_procedure_definition(define, definition, exprs)
                }
                _ => Ok(datum.clone()),
            },
            _ => Error::expansion_error("Expected definition", datum.clone()),
        }
    }

    fn expand_procedure_definition(
        &mut self,
        define: &Datum,
        definition: &Datum,
        body: &[Datum],
    ) -> Result<Datum> {
        match definition.sexp() {
            Sexp::List(ls) => match &ls[..] {
                [identifier, def_formals @ ..] => {
                    let mut lambda = vec![
                        Datum::new(
                            Sexp::Symbol(Self::unforgeable_lambda()),
                            definition.source_location().clone(),
                        ),
                        Datum::new(
                            Sexp::List(def_formals.to_vec()),
                            definition.source_location().clone(),
                        ),
                    ];
                    lambda.extend_from_slice(body);

                    let lambda_datum =
                        Datum::new(Sexp::List(lambda), identifier.source_location().clone());

                    Ok(Datum::new(
                        Sexp::List(vec![define.clone(), identifier.clone(), lambda_datum]),
                        define.source_location().clone(),
                    ))
                }
                _ => Error::expansion_error(
                    "Invalid definition. Expected (define (<IDENTIFIER> <def formals>) <body>) ",
                    define.clone(),
                ),
            },
            _ => Error::bug("Invalid procedure definition in expander"),
        }
    }

    fn unforgeable_lambda() -> Symbol {
        Symbol::unforgeable("lambda")
    }

    fn unforgeable_set() -> Symbol {
        Symbol::unforgeable("set!")
    }

    // returns the expression in core scheme
    fn expand_to_core(&mut self, datum: &Datum) -> Result<Expression> {
        todo!()
        /*
        match datum.sexp() {
            Sexp::List(ls) => match &ls[..] {
                [operator, _operands @ ..] if operator.is_symbol() => {
                    match self.denotation_of(operator) {
                        Some(Denotation::Special(Special::Quote)) => self.expand_quote(&datum),
                        Some(Denotation::Special(Special::Lambda)) => self.expand_lambda(&datum),
                        Some(Denotation::Special(Special::LetSyntax)) => {
                            self.expand_let_syntax(&datum)
                        }
                        Some(Denotation::Special(Special::LetrecSyntax)) => {
                            self.expand_letrec_syntax(&datum)
                        }
                        Some(Denotation::Special(Special::Set)) => self.expand_set(&datum),
                        Some(Denotation::Special(Special::Begin)) => self.expand_begin(&datum),
                        Some(Denotation::Special(Special::If)) => self.expand_if(&datum),
                        Some(Denotation::Special(_)) => Err(Error::ExpansionError(
                            "Definition out of context".to_string(),
                            datum.clone(),
                        )),
                        Some(Denotation::Macro) => self.expand_macro(&datum),
                        Some(Denotation::Id(_)) => self.expand_application(&datum),
                        None => todo!(),
                    }
                }
                [operator, operands @ ..] => self.expand_application(&datum),
                _ => Err(Error::ExpansionError(
                    "Invalid definition".to_string(),
                    datum.clone(),
                )), //empty unquoted list list
            },
            _atom => {
                let core = Expression::parse(datum)?;
                Ok(core)
            }
        }*/
    }

    /*
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
            Some(name) => self.expansion_env.is_bound(&name.into()),
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
    */

    /*

    fn expand_lambda(&mut self, lambda: Datum) -> Result<Datum> {
        Ok(lambda)
    }

    fn expand_quote(&mut self, quote: Datum) -> Result<Datum> {
        Ok(quote)
    }

    fn apply_macro(&mut self, name: &Datum, form: &Datum) -> Result<Option<Datum>> {
        match Self::match_symbol(name) {
            Some(name) => {
                let transformer = self.expansion_env.get(&name.into()).clone();
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

    */

    fn run_transformer(
        &mut self,
        transformer: value::procedure::Procedure,
        exp: &Datum,
    ) -> Result<Datum> {
        let mut vm = VM::for_macro_expansion();
        match vm.interprete_macro_transformer(transformer, self.renamer(), self.comparator(), exp) {
            Ok(v) => Ok(v),
            Err(e) => {
                eprintln!("OH OH: {}", e);
                Error::expansion_error("Couldn't expand", exp.clone())
            }
        }
    }

    fn let_transformer(args: Vec<Value>) -> FunctionResult<Value> {
        println!("Running transformer");
        match &args[..] {
            [datum, rename, compare] => Ok(Value::Bool(true)),
            _ => Err(crate::vm::value::error::arity_mismatch(
                Arity::Exactly(3),
                args.len(),
            )),
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
}
