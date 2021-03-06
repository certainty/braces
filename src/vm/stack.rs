// Implementations of the value and call stack used by the VM
// Most of the functionality here is unchecked and/or unsafe to be more efficient.
const DEFAULT_FRAMES_MAX: usize = 64;
const DEFAULT_STACK_MAX: usize = DEFAULT_FRAMES_MAX * 256;

// the stack implementation for the VM
#[derive(Debug)]
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
    // discards everything up to the given base
    pub fn truncate(&mut self, base: usize) {
        self.repr.truncate(base)
    }

    // Push a value to the stack.
    // If it exceeds the stack's capacity it will panic
    pub fn push(&mut self, v: V) {
        if self.repr.len() < self.cap {
            self.repr.push(v)
        } else {
            panic!("Stack overflow")
        }
    }

    pub fn at(&self, idx: usize) -> &V {
        &self.repr[idx]
    }

    // Pop from the top of the stack.
    // The caller has to make sure that the stack is not empty.
    pub fn pop(&mut self) -> V {
        self.repr.pop().unwrap()
    }

    pub fn pop_n(&mut self, n: usize) -> Vec<V> {
        let start = self.repr.len() - n;
        self.repr.drain(start..).collect()
    }

    pub fn top_n_mut(&mut self, n: usize) -> &mut [V] {
        let start = self.len() - n;
        &mut self.repr.as_mut_slice()[start..]
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

    pub fn top_mut_ptr(&mut self) -> *mut V {
        unsafe {
            self.repr
                .as_mut_ptr()
                .add(std::cmp::max(self.repr.len() - 1, 0))
        }
    }

    pub fn as_mut_ptr(&mut self) -> *mut V {
        self.repr.as_mut_ptr()
    }

    pub fn len(&self) -> usize {
        self.repr.len()
    }

    pub fn set(&mut self, index: usize, v: V) {
        self.repr[index] = v
    }

    // returns a vector of the stack elements in the order they appear on the stack from left to right
    pub fn as_vec(&self) -> &Vec<V> {
        &self.repr
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
    use crate::vm::Value;

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
    fn test_stack_pop_n() {
        let mut stack: Stack<Value> = Stack::default();

        stack.push(Value::Bool(true));
        stack.push(Value::Bool(false));
        stack.push(Value::Bool(false));
        stack.push(Value::Bool(true));

        assert_eq!(stack.pop_n(2), vec![Value::Bool(false), Value::Bool(true)]);

        // just make sure pushing still works
        stack.push(Value::Bool(false));
        assert_eq!(
            stack.as_vec(),
            &vec![Value::Bool(true), Value::Bool(false), Value::Bool(false)]
        )
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
    fn test_top_n_mut() {
        let mut stack: Stack<i32> = Stack::default();

        stack.push(1);
        stack.push(2);
        stack.push(3);
        stack.push(4);

        assert_eq!(stack.top_n_mut(3), &[2, 3, 4])
    }
}
