use super::expression;

#[derive(Debug, Clone)]
pub struct Ast {
    pub expressions: Vec<expression::Expression>,
}

impl Ast {
    pub fn singleton(&self) -> Option<&expression::Expression> {
        self.expressions.first()
    }
}
