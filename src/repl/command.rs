use crate::repl::string_completer::StringCompleter;
use crate::vm::value::procedure::Procedure;
use crate::vm::value::Value;
use crate::vm::Setting;
use crate::vm::VM;
use rustyline::completion::Completer;
use rustyline::completion::Pair;
use rustyline::Context;

pub struct Commands;

impl Commands {
    pub fn new() -> Self {
        Self {}
    }

    pub fn dispatch(&self, input: &str, vm: &mut VM) -> anyhow::Result<bool> {
        let parts: Vec<&str> = input.trim().split_whitespace().collect();

        if let Some(true) = parts.first().map(|e| e.starts_with(':')) {
            match &parts[..] {
                [":help"] => self.handle_help()?,
                [":set", argument] => self.handle_set(argument, vm)?,
                [":settings"] => self.handle_settings(vm)?,
                [":disass", binding] => self.handle_disass(binding, vm)?,
                _ => return Err(anyhow!("Invalid command")),
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn handle_help(&self) -> anyhow::Result<()> {
        println!("Available commands: ");
        self.display_help(":help", "Show help on the available commands");
        self.display_help(":set (+|-)flag | key=value", "Update settings");
        self.display_help(":settings", "Show the values of all settings");
        self.display_help(
            ":disass binding",
            "Disassemble the procedure bound to `binding`",
        );

        Ok(())
    }

    fn handle_set(&self, setting: &str, vm: &mut VM) -> anyhow::Result<()> {
        if setting.starts_with('+') {
            let setting = self.parse_setting(&setting[1..])?;
            vm.settings.enable(setting);
            Ok(())
        } else if setting.starts_with('-') {
            let setting = self.parse_setting(&setting[1..])?;
            vm.settings.disable(setting);
            Ok(())
        } else {
            Err(anyhow!(
                "Setting must be a known setting and prefixed with either + or -"
            ))
        }
    }

    fn parse_setting(&self, input: &str) -> anyhow::Result<Setting> {
        match input.to_lowercase().as_str() {
            "debug" => Ok(Setting::Debug),
            other => Err(anyhow!("Unknown setting {}", other)),
        }
    }

    fn handle_settings(&self, vm: &VM) -> anyhow::Result<()> {
        let info = vm
            .settings
            .as_vec()
            .iter()
            .map(|(s, f)| format!("{}: {}", s, if *f { "enabled" } else { "disabled" }))
            .collect::<Vec<_>>()
            .join(" ");

        println!("Settings+> {}", info);
        Ok(())
    }

    fn handle_disass(&self, ident: &str, vm: &mut VM) -> anyhow::Result<()> {
        match vm.top_level.get(&vm.values.sym(ident)) {
            Some(v) => match &*v.get_inner_ref() {
                Value::Closure(closure) => match vm.disassemble(closure.procedure()) {
                    Err(e) => Err(anyhow!("{}", e)),
                    _ => Ok(()),
                },
                Value::Procedure(Procedure::Native(proc)) => match vm.disassemble(&(*proc)) {
                    Err(e) => Err(anyhow!("{}", e)),
                    _ => Ok(()),
                },
                _ => Err(anyhow!("Can't disassemble non-procedure")),
            },
            _ => Err(anyhow!("Can't disassemble non-procedure")),
        }
    }

    #[inline]
    fn display_help(&self, usage: &str, description: &str) {
        println!("{:<25} {}", usage, description);
    }
}

pub struct CommandCompleter {
    implementation: StringCompleter,
}

impl CommandCompleter {
    pub fn new() -> Self {
        Self {
            implementation: StringCompleter::from(vec![
                String::from(":set"),
                String::from(":settings"),
                String::from(":help"),
                String::from(":disass"),
            ]),
        }
    }
}

impl Completer for CommandCompleter {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        ctx: &Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Pair>)> {
        let trimmed = &line[..pos].trim();
        if line.starts_with(':') {
            let parts: Vec<&str> = trimmed.split_whitespace().collect();
            match &parts[..] {
                [cmd, ..] => self.implementation.complete(cmd, pos, ctx),
                [] => Ok((pos, vec![])),
            }
        } else {
            // not a command
            return Ok((pos, vec![]));
        }
    }
}
