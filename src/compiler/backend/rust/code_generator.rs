/// Generate rust code from the representation of our little scheme
use crate::compiler::backend::Result;
use crate::compiler::frontend::parser::{
    apply::ApplicationExpression,
    assignment::SetExpression,
    body::BodyExpression,
    conditional::IfExpression,
    define::DefinitionExpression,
    identifier::Identifier,
    lambda::{Formals, LambdaExpression},
    sequence::BeginExpression,
    Expression,
};
use crate::compiler::frontend::syntax::symbol::Symbol;
use crate::compiler::representation::CoreAST;
use crate::compiler::source::{HasSourceLocation, Location, Registry};

#[derive(Debug, Clone, PartialEq)]
pub enum Context {
    Tail,
    NonTail,
}

impl Context {
    pub fn is_tail_context(&self) -> bool {
        match self {
            Self::Tail => true,
            _ => false,
        }
    }
}

pub struct CodeGenerator<'a> {
    source_registry: &'a Registry,
    source: String,
}

impl<'a> CodeGenerator<'a> {
    pub fn new(source_registry: &'a Registry) -> Self {
        Self {
            source_registry,
            source: "".to_string(),
        }
    }

    pub fn generate(registry: &'a Registry, ast: &CoreAST) -> Result<String> {
        let mut generator = Self::new(registry);

        for expr in &ast.expressions {
            generator.emit(&expr, &Context::NonTail)?;
        }

        Ok(generator.source)
    }

    fn emit(&mut self, ast: &Expression, context: &Context) -> Result<()> {
        match ast {
            Expression::Identifier(id) => self.emit_get_variable(id)?,
            Expression::Assign(expr) => self.emit_set(expr, context)?,
            _ => (),
        }

        Ok(())
    }

    fn emit_set(expr: &Expression, context: &Context) -> Result<()> {
        todo!()
    }

    fn emit_get_variable(&mut self, id: &Identifier) -> Result<()> {
        let mangled = self.mangle_identifier(id.symbol());

        self.source.push(' ');
        self.source.push_str(&mangled);
        self.source.push(' ');

        Ok(())
    }

    fn mangle_identifier(&mut self, sym: &Symbol) -> String {
        sym.string().clone()
    }
}
