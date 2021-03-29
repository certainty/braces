use crate::vm::byte_code::chunk;
use crate::vm::environment;
use crate::vm::value::procedure;

pub struct CallFrame<'a> {
    pub(crate) procedure: procedure::Lambda,
    pub(crate) env: &'a mut environment::Environment,
}

impl<'a> CallFrame<'a> {
    pub fn root(chunk: chunk::Chunk, root_env: &'a mut environment::Environment) -> Self {
        Self {
            procedure: procedure::Lambda::thunk(chunk),
            env: root_env,
        }
    }
}
