pub trait CompilerPhase<From, To, Error> {
    fn apply(&mut self, from: &From) -> std::result::Result<To, Error>;
}

pub struct AndPhase<LFrom, LTo, LError, RTo, RError> {
    lhs: Box<CompilerPhase<LFrom, LTo, LError>>,
    rhs: Box<CompilerPhase<LTo, RTo, RError>>,
}

impl<LFrom, LTo, LError, RTo, RError> AndPhase<LFrom, LTo, LError, RTo, RError> {
    pub fn new<LHS, RHS>(lhs: LHS, rhs: RHS) -> Self
    where
        LHS: CompilerPhase<LFrom, LTo, LError> + 'static,
        RHS: CompilerPhase<LTo, RTo, RError> + 'static,
    {
        Self {
            lhs: Box::from(lhs),
            rhs: Box::from(rhs),
        }
    }
}
