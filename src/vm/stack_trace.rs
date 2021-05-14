use crate::vm::instance::CallStack;

#[derive(Debug, Clone)]
pub struct StackInformation {
    pub closure_name: Option<String>,
    pub line_number: usize,
}

#[derive(Debug, Clone)]
pub struct StackTrace {
    frames: Vec<StackInformation>,
}

impl StackTrace {
    pub fn new(call_stack: &CallStack) -> Self {
        let mut info = vec![];

        for frame in call_stack.as_vec().iter() {
            let name = frame.closure.name();
            let line = frame.line_number_for_current_instruction().unwrap_or(0);
            info.push(StackInformation {
                closure_name: name.clone(),
                line_number: line,
            });
        }

        info.reverse();
        Self { frames: info }
    }

    pub fn as_string(&self) -> String {
        self.frames
            .iter()
            .map(|info| {
                format!(
                    "from {}:{}",
                    info.closure_name.clone().unwrap_or(String::from("closure")),
                    info.line_number
                )
            })
            .collect::<Vec<String>>()
            .join("\n")
    }
}

impl From<&CallStack> for StackTrace {
    fn from(call_stack: &CallStack) -> Self {
        StackTrace::new(call_stack)
    }
}
