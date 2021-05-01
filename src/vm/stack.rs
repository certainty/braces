// Implementations of the value and call stack used by the VM
// Most of the functionality here is unchecked and/or unsafe to be more efficient.

use super::scheme::value::Value;

const DEFAULT_FRAMES_MAX: usize = 64;
const DEFAULT_STACK_MAX: usize = DEFAULT_FRAMES_MAX * 256;

// the stack implementation for the VM
pub struct Stack<V> {
    cap: usize,
    repr: Vec<V>,
}

impl<V> Stack<V> {
    pub fn new(cap: usize) -> Self {
        Self {
            cap,
            repr: Vec::with_capacity(cap),
        }
    }
    // Push a value to the stack.
    // If it exceeds the stack's capacity it will panic
    pub fn push(&mut self, v: V) {
        if self.repr.len() < self.cap {
            self.repr.push(v)
        } else {
            panic!("Value stack overflow")
        }
    }

    // Pop from the top of the stack.
    // The caller has to make sure that the stack is not empty.
    pub fn pop(&mut self) -> V {
        self.repr.pop().unwrap()
    }

    // Peek into the stack at the position that is `distance` elements away from the stack top.
    //
    // | 10 | <- top
    // |----|
    // | 12 |
    // |----|
    // | 13 |
    // |----|
    // | 14 |
    // |----|
    //
    // Given the above stack `peek(2)` would return 12.
    // If distance is greater than the size of the stack the function will panic.

    pub fn peek(&self, distance: usize) -> &V {
        &self.repr[self.repr.len() - distance - 1]
    }

    #[inline]
    pub fn top(&self) -> &V {
        self.peek(0)
    }

    fn set(&mut self, index: usize, v: V) {
        self.repr[index] = v
    }

    fn get(&self, index: usize) -> &V {
        &self.repr[index]
    }

    // returns a vector of the stack elements in the order they appear on the stack from left to right
    pub fn as_vec<'a>(&'a self) -> &Vec<V> {
        &self.repr
    }
}

// A stack frame is a windowed view into a stack
// You can get and set values in that frame
struct Frame<'a, V> {
    base: usize,
    stack: &'a mut Stack<V>,
}

impl<'a, V> Frame<'a, V> {
    pub fn from<'b>(stack: &'b mut Stack<V>, base: usize) -> Frame<'b, V> {
        Frame { base, stack }
    }

    pub fn get(&'a self, index: usize) -> &'a V {
        self.stack.get(self.base + index)
    }

    pub fn set(&mut self, index: usize, v: V) {
        self.stack.set(self.base + index, v)
    }
}

impl<V> Default for Stack<V> {
    fn default() -> Stack<V> {
        Stack::new(DEFAULT_STACK_MAX)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stack_push() {
        let mut stack: Stack<Value> = Stack::default();

        stack.push(Value::Bool(true));
        stack.push(Value::Bool(false));

        assert_eq!(stack.as_vec(), &vec![Value::Bool(true), Value::Bool(false)])
    }

    #[test]
    fn test_stack_pop() {
        let mut stack: Stack<Value> = Stack::default();

        stack.push(Value::Bool(true));
        stack.push(Value::Bool(false));

        assert_eq!(stack.as_vec(), &vec![Value::Bool(true), Value::Bool(false)]);
        assert_eq!(stack.pop(), Value::Bool(false));
        assert_eq!(stack.as_vec(), &vec![Value::Bool(true)]);
    }

    #[test]
    fn test_stack_peek() {
        let mut stack: Stack<Value> = Stack::default();

        stack.push(Value::Bool(true));
        stack.push(Value::Bool(false));
        stack.push(Value::Char('c'));
        stack.push(Value::Char('d'));

        assert_eq!(stack.peek(0), &Value::Char('d'));
        assert_eq!(stack.peek(3), &Value::Bool(true));
    }

    #[test]
    fn test_frame_get() {
        let mut stack: Stack<Value> = Stack::default();

        stack.push(Value::Bool(true));
        stack.push(Value::Bool(false));
        stack.push(Value::Char('c'));
        stack.push(Value::Char('d'));

        let frame = Frame::from(&mut stack, 1);

        assert_eq!(frame.get(0), &Value::Bool(false));
        assert_eq!(frame.get(1), &Value::Char('c'));
    }

    #[test]
    fn test_frame_set() {
        let mut stack: Stack<Value> = Stack::default();

        stack.push(Value::Bool(true));
        stack.push(Value::Bool(false));
        stack.push(Value::Char('c'));
        stack.push(Value::Char('d'));

        let mut frame = Frame::from(&mut stack, 1);
        frame.set(0, Value::Bool(true));
        frame.set(2, Value::Char('y'));

        assert_eq!(
            stack.as_vec(),
            &vec![
                Value::Bool(true),
                Value::Bool(true),
                Value::Char('c'),
                Value::Char('y')
            ]
        );
    }
}
