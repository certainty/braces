pub mod command;
pub mod string_completer;
use crate::braces_config_directory;
use crate::compiler::error::UserMessage;
use crate::repl::command::CommandCompleter;
use crate::repl::command::Commands;
use crate::repl::string_completer::StringCompleter;
use crate::vm;
use crate::vm::value::Value;
use crate::vm::VM;
use crate::BRACES_VERSION;
use rustyline::completion::{Completer, Pair};
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::validate::ValidationContext;
use rustyline::validate::ValidationResult;
use rustyline::validate::Validator;
use rustyline::{Editor, Helper};
use std::borrow::Cow;
use std::result::Result::Err;

pub struct Repl {
    vm: VM,
    commands: Commands,
    editor: Editor<ReplHelper>,
}

pub struct ReplHelper {
    bindings_completer: StringCompleter,
    command_completer: CommandCompleter,
    filename_completer: rustyline::completion::FilenameCompleter,
    bracket_validator: rustyline::validate::MatchingBracketValidator,
    bracket_highlighter: rustyline::highlight::MatchingBracketHighlighter,
}

impl ReplHelper {
    pub fn new(vm: &VM) -> Self {
        Self {
            bindings_completer: StringCompleter::from(vm.binding_names()),
            command_completer: CommandCompleter::new(),
            filename_completer: rustyline::completion::FilenameCompleter::default(),
            bracket_validator: rustyline::validate::MatchingBracketValidator::new(),
            bracket_highlighter: rustyline::highlight::MatchingBracketHighlighter::new(),
        }
    }

    pub fn update_bindings(&mut self, vm: &VM) {
        self.bindings_completer = StringCompleter::from(vm.binding_names());
    }
}

impl Helper for ReplHelper {}

impl Hinter for ReplHelper {
    type Hint = String;

    fn hint(&self, _line: &str, _pos: usize, _ctx: &rustyline::Context) -> Option<Self::Hint> {
        None
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
        let (start, matches) = self.command_completer.complete(line, pos, ctx)?;

        if matches.len() > 0 {
            return Ok((start, matches));
        }

        let (start, matches) = self.bindings_completer.complete(line, pos, ctx)?;
        if matches.len() > 0 {
            return Ok((start, matches));
        }

        self.filename_completer.complete(line, pos, ctx)
    }
}

impl Highlighter for ReplHelper {
    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.bracket_highlighter.highlight(line, pos)
    }
}

impl Validator for ReplHelper {
    fn validate(&self, ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
        self.bracket_validator.validate(ctx)
    }
}

impl Repl {
    pub fn new(vm: VM) -> anyhow::Result<Self> {
        Self::create_directories()?;

        let editor = Editor::<ReplHelper>::with_config(Self::default_config());
        let commands = Commands::new();

        Ok(Self {
            vm,
            editor,
            commands,
        })
    }

    // main read-eval-print loop
    pub fn run_loop(&mut self) -> anyhow::Result<()> {
        self.editor.load_history(&Self::history_path())?;
        self.banner()?;

        loop {
            let helper = ReplHelper::new(&self.vm);
            self.editor.set_helper(Some(helper));

            let line = self.read_line();

            match line {
                Ok(input) => match self.handle_input(&input) {
                    Ok(_) => (),
                    Err(e) => {
                        eprintln!("{}", e);
                    }
                },
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

    fn banner(&self) -> anyhow::Result<()> {
        println!("BRACES - the tiny scheme");
        println!("Version: {}\n", BRACES_VERSION);
        println!("Type :help for help.");
        Ok(())
    }

    fn read_line(&mut self) -> anyhow::Result<String> {
        let prompt = self.prompt();
        let line = self.editor.readline(&prompt)?;
        Ok(line)
    }

    fn handle_input(&mut self, input: &String) -> anyhow::Result<()> {
        if !self.commands.dispatch(&input, &mut self.vm)? {
            self.eval(input)?
        }
        Ok(())
    }

    fn eval(&mut self, source: &String) -> anyhow::Result<()> {
        match self.vm.run_string(source, "repl") {
            Ok(Value::Unspecified) => (),
            Ok(v) => println!("{}", self.vm.write(&v)),
            Err(vm::Error::CompilerError(e)) => e.print_user_friendly_message(),
            Err(vm::Error::RuntimeError(msg, line, stack_trace, Some(ctx))) => {
                eprintln!(
                    "{} in line {} [in {}]\n{}",
                    msg,
                    line,
                    ctx,
                    stack_trace.as_string()
                )
            }

            Err(vm::Error::RuntimeError(msg, line, stack_trace, _)) => {
                eprintln!("{} in line {}\n{}", msg, line, stack_trace.as_string())
            }

            Err(e @ vm::Error::CompilerBug(_)) => eprintln!("{}", e),
            Err(e) => eprintln!("{}", e),
        }
        Ok(())
    }

    #[inline]
    fn prompt(&self) -> String {
        String::from("λ ")
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
