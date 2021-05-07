use crate::vm::scheme::value::Value;
use crate::vm::stack::Stack;

// Utilities to get insights into how the stacks are arranged

pub fn pretty_print(stack: &Stack<Value>) -> String {
    let mut out = String::new();
    let mut longest_value = 0;
    let mut values: Vec<String> = vec![];

    for value in stack.as_vec().iter() {
        let v = stack_print(value);
        longest_value = std::cmp::max(longest_value, v.len());
        values.push(v);
    }

    for current in values.iter() {
        out.push_str(&"-".repeat(longest_value + 4));
        out.push_str(&format!(
            "\n| {:width$} |\n",
            current,
            width = longest_value
        ));
    }
    out.push_str(&"-".repeat(longest_value + 4));
    out.push('\n');
    out
}

fn stack_print(v: &Value) -> String {
    match v {
        Value::Procedure(proc) => format!("#<procedure {}>", proc.name()),
        Value::Closure(closure) => {
            let up_values: Vec<String> = closure
                .up_values
                .iter()
                .map(|e| format!("{} @ {:p}", stack_print(&e.borrow()), e))
                .collect();
            format!(
                "#<closure {}  up-values: [{}]>",
                closure.proc.name(),
                up_values.join(", ")
            )
        }
        Value::UpValue(inner) => stack_print(&inner.borrow()),
        v => format!("{:?}", v),
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn pretty_stack_justifies_values() {}
}
