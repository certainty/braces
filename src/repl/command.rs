use crate::vm::value::procedure::Procedure;
use crate::vm::value::Value;
use crate::vm::VM;
use rustc_hash::FxHashMap;

pub type CommandHandler = dyn Fn(&mut VM, Vec<String>) -> anyhow::Result<()>;

pub struct CommandRegistry {
    commands: FxHashMap<String, Command>,
}

pub struct Command {
    name: String,
    help: String,
    argc: usize,
    handler: Box<CommandHandler>,
}

impl Command {
    pub fn new<
        N: Into<String>,
        H: Into<String>,
        F: 'static + Fn(&mut VM, Vec<String>) -> anyhow::Result<()>,
    >(
        name: N,
        help: H,
        argc: usize,
        handler: F,
    ) -> Self {
        Self {
            name: name.into(),
            help: help.into(),
            argc,
            handler: Box::new(handler),
        }
    }
}

impl CommandRegistry {
    pub fn new() -> Self {
        Self {
            commands: FxHashMap::default(),
        }
    }

    pub fn register(&mut self, cmd: Command) {
        self.commands.insert(cmd.name.clone(), cmd);
    }

    pub fn dispatch(&self, input: &str, vm: &mut VM) -> anyhow::Result<bool> {
        if let Some((cmd, args)) = Self::parse_command(input) {
            if let Some(command) = self.commands.get(&cmd) {
                if command.argc == args.len() {
                    (*command.handler)(vm, args)?;
                    return Ok(true);
                } else {
                    println!(
                        "Command {} requires exactly {} arguments.",
                        command.name, command.argc
                    );
                    return Ok(true);
                }
            }
        }
        Ok(false)
    }

    fn parse_command(input: &str) -> Option<(String, Vec<String>)> {
        let parts: Vec<&str> = input.trim().split_whitespace().collect();

        match &parts[..] {
            [command, arguments @ ..] if command.starts_with(':') => Some((
                command[1..].to_string(),
                arguments
                    .iter()
                    .map(|e| String::from(*e))
                    .collect::<Vec<_>>(),
            )),
            _ => None,
        }
    }
}

impl Default for CommandRegistry {
    fn default() -> CommandRegistry {
        let mut registry = CommandRegistry::new();

        registry.register(Command::new("help", "Print help", 0, handle_help));
        registry.register(Command::new(
            "disass",
            "Disassemble a procedure",
            1,
            handle_disass,
        ));

        registry
    }
}

fn handle_help(_vm: &mut VM, _args: Vec<String>) -> anyhow::Result<()> {
    println!("You called for help");
    Ok(())
}

fn handle_disass(vm: &mut VM, args: Vec<String>) -> anyhow::Result<()> {
    match &args[..] {
        [ident] => match vm.toplevel.get(&vm.values.sym(ident)) {
            Some(Value::Closure(closure)) => match vm.disassemble(closure.procedure()) {
                Err(e) => Err(anyhow!("{}", e)),
                _ => Ok(()),
            },
            Some(Value::Procedure(Procedure::Native(proc))) => match vm.disassemble(&(*proc)) {
                Err(e) => Err(anyhow!("{}", e)),
                _ => Ok(()),
            },
            _ => {
                println!("Can't disassemble non-procedure");
                Ok(())
            }
        },
        _ => Err(anyhow!("Invalid arguments")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::VM;

    #[test]
    fn parse_disass_command() {
        let mut vm = vm::VM::default();
        let cmd = Command::new("test", "test", 0, test_handler);
        let mut commands = CommandRegistry::new();

        commands.register(cmd);

        assert_eq!(commands.dispatch("  :test", &mut vm).unwrap(), true);
    }

    fn test_handler(vm: &mut VM, args: Vec<String>) -> anyhow::Result<()> {
        Ok(())
    }
}
