use crate::vm::value::procedure::Procedure;
use crate::vm::value::Value;
use crate::vm::Setting;
use crate::vm::VM;

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
        self.display_help(":set (+|-)setting", "Enable or disable a setting");
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
        match vm.toplevel.get(&vm.values.sym(ident)) {
            Some(Value::Closure(closure)) => match vm.disassemble(closure.procedure()) {
                Err(e) => Err(anyhow!("{}", e)),
                _ => Ok(()),
            },
            Some(Value::Procedure(Procedure::Native(proc))) => match vm.disassemble(&(*proc)) {
                Err(e) => Err(anyhow!("{}", e)),
                _ => Ok(()),
            },
            _ => Err(anyhow!("Can't disassemble non-procedure")),
        }
    }

    #[inline]
    fn display_help(&self, usage: &str, description: &str) {
        println!("{:<25} {}", usage, description);
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
