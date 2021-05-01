// Implementations of the value and call stack used by the VM
// Most of the functionality here is unchecked and/or unsafe to be more efficient.
use std::rc::Rc;

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

    pub fn top_mut_ptr<'a>(&'a mut self) -> *mut V {
        unsafe { self.repr.as_mut_ptr().add(self.repr.len() - 1) }
    }

    pub fn as_mut_ptr(&mut self) -> *mut V {
        self.repr.as_mut_ptr()
    }

    pub fn len(&self) -> usize {
        self.repr.len()
    }

    fn set(&mut self, index: usize, v: V) {
        self.repr[index] = v
    }

    fn get<'a>(&'a self, index: usize) -> &'a V {
        &self.repr[index]
    }

    // returns a vector of the stack elements in the order they appear on the stack from left to right
    pub fn as_vec<'a>(&'a self) -> &Vec<V> {
        &self.repr
    }
}

impl<V> Default for Stack<V> {
    fn default() -> Stack<V> {
        Stack::new(DEFAULT_STACK_MAX)
    }
}

// A stack frame is a windowed view into a stack
// You can get and set values in that frame
// Note that the Frame owns the stack for the time it lives and it may give it back
#[derive(Debug)]
pub struct Frame<V> {
    base: usize,
    slots: *mut V,
}

impl<V> Frame<V> {
    pub fn from(slots: *mut V, base: usize) -> Frame<V> {
        Self { base, slots }
    }

    pub fn get<'a>(&'a self, index: usize) -> &'a V {
        unsafe { &*self.slots.add(self.base + index) }
    }

    pub fn set(&mut self, index: usize, v: V) {
        unsafe { (*self.slots.add(self.base + index)) = v }
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

        let frame = Frame::from(stack, 1);

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

        let mut frame = Frame::from(stack, 1);
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
