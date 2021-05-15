use crate::vm::value::procedure::Procedure;
use crate::vm::value::Value;
use crate::vm::Setting;
use crate::vm::VM;
use rustc_hash::FxHashMap;

pub type CommandHandler = dyn Fn(&CommandRegistry, &mut VM, Vec<String>) -> anyhow::Result<()>;

pub struct CommandRegistry {
    commands: FxHashMap<String, Command>,
}

pub struct Command {
    pub name: String,
    pub usage: String,
    pub description: String,
    pub argc: usize,
    handler: Box<CommandHandler>,
}

impl Command {
    pub fn new<
        N: Into<String>,
        H: Into<String>,
        D: Into<String>,
        F: 'static + Fn(&CommandRegistry, &mut VM, Vec<String>) -> anyhow::Result<()>,
    >(
        name: N,
        usage: H,
        description: D,
        argc: usize,
        handler: F,
    ) -> Self {
        Self {
            name: name.into(),
            usage: usage.into(),
            description: description.into(),
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
                    (*command.handler)(self, vm, args)?;
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

        registry.register(Command::new("help", ":help", "Show help", 0, handle_help));
        registry.register(Command::new(
            "set",
            ":set (+|-)setting",
            "Enable or disable a setting",
            1,
            handle_set,
        ));
        registry.register(Command::new(
            "settings",
            ":settings",
            "Display the value of current settings",
            0,
            handle_list_settings,
        ));
        registry.register(Command::new(
            "disass",
            ":disass name",
            "Disassemble the procedure bound to `name`",
            1,
            handle_disass,
        ));

        registry
    }
}

fn handle_help(registry: &CommandRegistry, _vm: &mut VM, _args: Vec<String>) -> anyhow::Result<()> {
    println!("Available commands: ");
    for cmd in registry.commands.values() {
        println!("{:<25} {}", cmd.usage, cmd.description);
    }
    Ok(())
}

fn handle_disass(
    _registry: &CommandRegistry,
    vm: &mut VM,
    args: Vec<String>,
) -> anyhow::Result<()> {
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

fn handle_set(_registry: &CommandRegistry, vm: &mut VM, args: Vec<String>) -> anyhow::Result<()> {
    match &args[..] {
        [setting] if setting.starts_with('+') => {
            let setting = parse_setting(&setting[1..])?;
            vm.settings.enable(setting);
            Ok(())
        }
        [setting] if setting.starts_with('-') => {
            let setting = parse_setting(&setting[1..])?;
            vm.settings.disable(setting);
            Ok(())
        }
        _ => Err(anyhow!("Invalid arguments")),
    }
}

fn handle_list_settings(
    _registry: &CommandRegistry,
    vm: &mut VM,
    _args: Vec<String>,
) -> anyhow::Result<()> {
    let info = vm
        .settings
        .as_vec()
        .iter()
        .map(|(s, f)| format!("{}: {}", s, if *f { "enabled" } else { "disabled" }))
        .collect::<Vec<_>>()
        .join(" ");

    println!("Settings +> {}", info);
    Ok(())
}

fn parse_setting(input: &str) -> anyhow::Result<Setting> {
    match input.to_lowercase().as_str() {
        "debug" => Ok(Setting::Debug),
        other => Err(anyhow!("Unknown setting {}", other)),
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
