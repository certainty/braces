use super::syntax::environment::{Denotation, Special, SyntacticContext, SyntaxEnvironment};

pub struct ParserContext {
    syntax_ctx: SyntacticContext,
    expander: MacroExpander,
    compiler: Compiler,
}

impl ParserContext {
    pub fn new(ctx: SyntacticContext, expander: MacroExpander, compiler: Compiler) -> Self {
        let mut parser_ctx = ParserContext {
            syntax_ctx: ctx,
            expander: expander,
            compiler: compiler,
        };
        parser_ctx.register_transformers();
        parser_ctx
    }

    fn register_transformers(&mut self) {
        self.expander
            .register_transformers(self.syntax_ctx.usual_env());
    }

    pub fn denotation_of(&mut self, datum: &Datum) -> Denotation {
        if let Sexp::Symbol(sym) = datum.sexp() {
            self.syntax_ctx.usual_env().get(&sym.clone().into())
        } else {
            panic!("[BUG] expected symbol")
        }
    }

    pub fn expand_macro(
        &mut self,
        datum: &Datum,
        transformer: syntax::Transformer,
    ) -> Result<Datum> {
        let transformed = self.expander.expand(&datum, transformer)?;
        Ok(transformed)
    }

    pub fn define_syntax(
        &mut self,
        name: syntax::Symbol,
        transformer: syntax::Transformer,
    ) -> Result<()> {
        let denotation = Denotation::Macro(transformer);
        self.syntax_ctx.usual.extend(name, denotation);
        Ok(())
    }
}

impl Default for ParserContext {
    fn default() -> Self {
        ParserContext::new(
            SyntacticContext::default(),
            MacroExpander::new(),
            Compiler::new(),
        )
    }
}
