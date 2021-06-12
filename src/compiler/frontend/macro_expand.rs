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
use crate::compiler::frontend::parser::{
    sexp::datum::{Datum, Sexp},
    Parser,
};
use crate::compiler::Compiler;
use crate::vm::instance::Instance;
use thiserror::Error;

type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug, Clone)]
pub enum Error {}

pub struct MacroExpander {
    compiler: Compiler,
    // lexically track variables
    variables: VariablesRef,
}

impl MacroExpander {
    pub fn new() -> MacroExpander {
        MacroExpander {
            // maybe we need to specify the pipeline later
            compiler: Compiler::new(),
            variables: Variables::top_level(),
        }
    }

    // main function converts a program into another program
    // macro definitions and macro uses will be removed from the AST
    // TODO: should we return a core-ast already?
    pub fn expand(program: Vec<Datum>) -> Result<Vec<Datum>> {
        let mut expander = Self::new();
        expander.expand_program(program)
    }

    pub fn expand_program(&mut self, program: Vec<Datum>) -> Result<Vec<Datum>> {
        Ok(program)
    }

    fn expand_datum(&mut self, datum: Datum) -> Result<Datum> {
        Ok(datum)
    }
}
