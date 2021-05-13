use crate::vm::stack::Stack;
use crate::vm::value::Value;

// Utilities to get insights into how the stacks are arranged

pub fn pretty_print(stack: &Stack<Value>, frame_base: usize) -> String {
    let mut out = String::new();
    let mut longest_value = 0;
    let mut values: Vec<String> = vec![];

    for value in stack.as_vec().iter() {
        let v = stack_print(value);
        longest_value = std::cmp::max(longest_value, v.len());
        values.push(v);
    }

    for (addr, current) in values.iter().enumerate() {
        let vertical_delim = if addr == frame_base { "=" } else { "-" };

        out.push_str("    ");
        out.push_str(&vertical_delim.repeat(longest_value + 4));
        out.push_str(&format!(
            "\n{:03} | {:width$} |",
            addr,
            current,
            width = longest_value
        ));

        if addr == frame_base {
            out.push_str(&format!(" 000 <--- stack base ({:03})", frame_base));
        }
        if addr > frame_base {
            let frame_relative = addr - frame_base;
            out.push_str(&format!(" {:03} ", frame_relative));
        }

        out.push_str("\n");
    }
    out.push_str("    ");
    out.push_str(&"-".repeat(longest_value + 4));
    out.push('\n');
    out
}

fn stack_print(v: &Value) -> String {
    match v {
        Value::ProperList(inner) => {
            format!(
                "({})",
                inner
                    .iter()
                    .map(stack_print)
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        }
        Value::Procedure(proc) => format!(
            "#<procedure {}>",
            proc.name().unwrap_or(String::from("lambda"))
        ),
        Value::Closure(closure) => {
            let up_values: Vec<String> = closure
                .up_values
                .iter()
                .map(|e| format!("{} @ {:p}", stack_print(&e.as_ref()), e))
                .collect();
            let label = closure.name().clone();

            format!(
                "#<closure {}  up-values: [{}]>",
                label.unwrap_or(String::from("lambda")),
                up_values.join(", ")
            )
        }
        v => format!("{:?}", v),
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn pretty_stack_justifies_values() {}
}
