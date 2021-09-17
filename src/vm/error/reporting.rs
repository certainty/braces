use super::Error;
use crate::compiler::error::reporting::ErrorReporter as CompilerErrorReporter;
use crate::vm::stack_trace::StackTrace;
use crate::vm::value::error::RuntimeError;

pub struct ErrorReporter<'a> {
    compiler_reporter: &'a CompilerErrorReporter<'a>,
}

impl<'a> ErrorReporter<'a> {
    pub fn new(reporter: &'a CompilerErrorReporter) -> Self {
        Self {
            compiler_reporter: reporter,
        }
    }
}

impl<'a> ErrorReporter<'a> {
    pub fn report_error(&self, e: &Error) {
        match e {
            Error::CompilerBug(msg) => eprintln!("Bug: {}", msg),
            Error::CompilerError(e) => self.compiler_reporter.report_error(&e),
            Error::RuntimeError(e, line, stack_trace, label) => {
                self.report_runtime_error(e, line.clone(), stack_trace.clone(), label.clone())
            }
        }
    }

    pub fn report_runtime_error(
        &self,
        e: &RuntimeError,
        line: usize,
        stack_trace: StackTrace,
        _label: Option<String>,
    ) {
        eprintln!(
            "{} on line {}\n{}",
            self.runtime_error_message(e),
            line,
            stack_trace.as_string()
        )
    }

    pub fn runtime_error_message(&self, e: &RuntimeError) -> String {
        match e {
            RuntimeError::ArgumentError(value, message) => {
                format!("ArgumentError: {:?} {}", value, message)
            }
            RuntimeError::ArithmeticError(message) => format!("ArithmeticError: {}", message),
            RuntimeError::ArityError(arity, argc) => format!("ArityError: {:?} {}", arity, argc),
            RuntimeError::NoncallableError(v) => format!("NonCallable: {:?}", v),
            RuntimeError::UndefinedVariable(s) => {
                format!("UndefinedVariable: Variable `{}` is undefined", s.as_str())
            }
            RuntimeError::OutOfBoundError(idx, accepted) => {
                format!("OutOfBoudError: can't access element at index `{}`. The allowed ranged is ({}..{})", idx, accepted.start, accepted.end)
            }
            RuntimeError::SyntaxError(msg) => {
                format!("Error during macro expansion: {}", msg)
            }
        }
    }
}
