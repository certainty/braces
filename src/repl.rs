pub mod command;
use crate::braces_config_directory;
use crate::compiler::error::UserMessage;
use crate::vm;
use crate::vm::value::Value;
use crate::vm::VM;
use anyhow::Result;
use rustyline::completion::{Completer, Pair};
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::validate::ValidationContext;
use rustyline::validate::ValidationResult;
use rustyline::validate::Validator;
use rustyline::{Editor, Helper};
use std::result::Result::Err;

pub struct Repl {
    vm: VM,
    editor: Editor<ReplHelper>,
}

pub struct ReplHelper {
    history_hinter: rustyline::hint::HistoryHinter,
    filename_completer: rustyline::completion::FilenameCompleter,
    bracket_validator: rustyline::validate::MatchingBracketValidator,
}

impl ReplHelper {
    pub fn new() -> Self {
        Self {
            history_hinter: rustyline::hint::HistoryHinter {},
            filename_completer: rustyline::completion::FilenameCompleter::default(),
            bracket_validator: rustyline::validate::MatchingBracketValidator::new(),
        }
    }
}

impl Helper for ReplHelper {}
impl Hinter for ReplHelper {
    type Hint = String;

    fn hint(&self, line: &str, pos: usize, ctx: &rustyline::Context) -> Option<Self::Hint> {
        self.history_hinter.hint(line, pos, ctx)
    }
}

impl Completer for ReplHelper {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        ctx: &rustyline::Context,
    ) -> rustyline::Result<(usize, Vec<Pair>)> {
        self.filename_completer.complete(line, pos, ctx)
    }
}

impl Highlighter for ReplHelper {}

impl Validator for ReplHelper {
    fn validate(&self, ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
        self.bracket_validator.validate(ctx)
    }
}

enum ReplInput {
    Command(command::Command),
    Scheme(String),
}

impl Repl {
    pub fn new(vm: VM) -> anyhow::Result<Self> {
        Self::create_directories()?;

        let editor = Editor::<ReplHelper>::with_config(Self::default_config());

        Ok(Self { vm, editor })
    }

    // main read-eval-print loop
    pub fn run_loop(&mut self) -> anyhow::Result<()> {
        let helper = ReplHelper::new();
        self.editor.set_helper(Some(helper));
        self.editor.load_history(&Self::history_path())?;

        loop {
            let result = self.read_input();

            match result {
                Ok(input) => self.handle_input(&input)?,
                Err(err) => match err.downcast_ref() {
                    Some(ReadlineError::Interrupted) => {
                        println!("CTRL-C");
                        break;
                    }
                    Some(ReadlineError::Eof) => {
                        println!("CTRL-D");
                        break;
                    }
                    err => {
                        println!("Error: {:?}", err);
                        break;
                    }
                },
            }
        }

        self.editor.save_history(&Self::history_path())?;
        Ok(())
    }

    fn read_input(&mut self) -> anyhow::Result<ReplInput> {
        let prompt = self.prompt();
        let line = self.editor.readline(&prompt)?;

        if let Some(cmd) = command::Command::parse(&line) {
            Ok(ReplInput::Command(cmd))
        } else {
            Ok(ReplInput::Scheme(line))
        }
    }

    fn handle_input(&mut self, input: &ReplInput) -> anyhow::Result<()> {
        match input {
            ReplInput::Command(cmd) => self.handle_command(cmd),
            ReplInput::Scheme(source) => self.eval(source),
        }
    }

    fn eval(&mut self, source: &String) -> anyhow::Result<()> {
        match self.vm.run_string(source, "repl") {
            Ok(Value::Unspecified) => (),
            Ok(v) => println!("{}", self.vm.write(&v)),
            Err(vm::Error::CompilerError(e)) => e.print_user_friendly_message(),
            Err(vm::Error::RuntimeError(msg, line, stack_trace)) => {
                eprintln!("{}:{}\n{}", msg, line, stack_trace.as_string())
            }
            Err(e @ vm::Error::CompilerBug(_)) => eprintln!("{}", e),
        }
        Ok(())
    }

    fn handle_command(&mut self, cmd: &command::Command) -> anyhow::Result<()> {
        Ok(())
    }

    #[inline]
    fn prompt(&self) -> String {
        String::from(">> ")
    }

    fn default_config() -> rustyline::config::Config {
        let config_builder = rustyline::config::Config::builder();

        config_builder
            .auto_add_history(true)
            .history_ignore_dups(true)
            .history_ignore_space(false)
            .max_history_size(500)
            .completion_prompt_limit(100)
            .build()
    }

    fn history_path() -> std::path::PathBuf {
        Self::config_dir().join("history")
    }

    #[inline]
    fn create_directories() -> anyhow::Result<()> {
        std::fs::create_dir_all(Self::config_dir())?;

        if !Self::history_path().exists() {
            std::fs::File::create(Self::history_path())?;
        }

        Ok(())
    }

    #[inline]
    fn config_dir() -> std::path::PathBuf {
        braces_config_directory().join("repl")
    }
}
