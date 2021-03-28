use crate::vm::byte_code::chunk;
use crate::vm::environment;
use crate::vm::value::procedure;

pub struct CallFrame {
    pub(crate) procedure: procedure::Lambda,
    pub(crate) env: environment::Environment,
}

impl CallFrame {
    pub fn root(chunk: chunk::Chunk, root_env: environment::Environment) -> Self {
        Self {
            procedure: procedure::Lambda::thunk(chunk),
            env: root_env,
        }
    }
}
