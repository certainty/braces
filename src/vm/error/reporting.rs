use super::Error;
use crate::compiler::error::reporting::ErrorReporter as CompilerErrorReporter;
use crate::compiler::source::Registry;
use crate::vm::stack_trace::StackTrace;
use crate::vm::value::error::RuntimeError;

pub struct ErrorReporter<'a> {
    compiler_reporter: CompilerErrorReporter<'a>,
}

impl<'a> ErrorReporter<'a> {
    pub fn new(source_registry: Registry<'a>) -> Self {
        Self {
            compiler_reporter: CompilerErrorReporter::new(source_registry),
        }
    }
}

impl<'a> ErrorReporter<'a> {
    pub fn report_error(&self, e: Error) {
        match e {
            Error::CompilerError(e) => self.compiler_reporter.report_error(&e),
            Error::RuntimeError(e, _, stack_trace, label) => {
                self.report_runtime_error(e, stack_trace, label)
            }
        }
    }

    pub fn report_runtime_error(
        &self,
        e: RuntimeError,
        stack_trace: StackTrace,
        label: Option<String>,
    ) {
        println!(
            "{} \n stack trace: {}",
            self.runtime_error_message(e),
            stack_trace.as_string()
        )
    }

    pub fn runtime_error_message(&self, e: RuntimeError) -> String {
        match e {
            RuntimeError::ArgumentError(value, message) => {
                format!("ArgumentError: {:?} {}", value, message)
            }
            RuntimeError::ArithmeticError(message) => format!("ArithmeticError: {}", message),
            RuntimeError::ArityError(arity, argc) => format!("ArityError: {:?} {}", arity, argc),
            RuntimeError::NoncallableError(v) => format!("NonCallable: {:?}", v),
            RuntimeError::UndefinedVariable(s) => format!("UndefineVariabel: {:?}", s),
        }
    }
}
