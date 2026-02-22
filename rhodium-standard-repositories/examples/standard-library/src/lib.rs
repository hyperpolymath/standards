//! # Standard Library
//!
//! A collection of useful data structures demonstrating Silver-level Rhodium compliance.
//!
//! This library provides:
//! - A stack data structure with standard operations
//! - A queue implementation
//! - Optional serialization support via the `serialization` feature
//!
//! ## Examples
//!
//! ```
//! use standard_library::Stack;
//!
//! let mut stack = Stack::new();
//! stack.push(1);
//! stack.push(2);
//! assert_eq!(stack.pop(), Some(2));
//! ```

#![warn(missing_docs)]
#![cfg_attr(docsrs, feature(doc_cfg))]

#[cfg(feature = "serialization")]
use serde::{Deserialize, Serialize};

/// A generic stack data structure implementing LIFO (Last-In-First-Out) behavior.
///
/// # Examples
///
/// ```
/// use standard_library::Stack;
///
/// let mut stack = Stack::new();
/// stack.push(42);
/// stack.push(100);
///
/// assert_eq!(stack.len(), 2);
/// assert_eq!(stack.pop(), Some(100));
/// assert_eq!(stack.pop(), Some(42));
/// assert_eq!(stack.pop(), None);
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialization", derive(Serialize, Deserialize))]
pub struct Stack<T> {
    items: Vec<T>,
}

impl<T> Stack<T> {
    /// Creates a new empty stack.
    ///
    /// # Examples
    ///
    /// ```
    /// use standard_library::Stack;
    ///
    /// let stack: Stack<i32> = Stack::new();
    /// assert!(stack.is_empty());
    /// ```
    pub fn new() -> Self {
        Stack { items: Vec::new() }
    }

    /// Creates a new stack with the specified capacity.
    ///
    /// # Examples
    ///
    /// ```
    /// use standard_library::Stack;
    ///
    /// let stack: Stack<i32> = Stack::with_capacity(10);
    /// assert!(stack.is_empty());
    /// ```
    pub fn with_capacity(capacity: usize) -> Self {
        Stack {
            items: Vec::with_capacity(capacity),
        }
    }

    /// Pushes an item onto the stack.
    ///
    /// # Examples
    ///
    /// ```
    /// use standard_library::Stack;
    ///
    /// let mut stack = Stack::new();
    /// stack.push(1);
    /// assert_eq!(stack.len(), 1);
    /// ```
    pub fn push(&mut self, item: T) {
        self.items.push(item);
    }

    /// Removes and returns the top item from the stack.
    ///
    /// Returns `None` if the stack is empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use standard_library::Stack;
    ///
    /// let mut stack = Stack::new();
    /// stack.push(1);
    /// assert_eq!(stack.pop(), Some(1));
    /// assert_eq!(stack.pop(), None);
    /// ```
    pub fn pop(&mut self) -> Option<T> {
        self.items.pop()
    }

    /// Returns a reference to the top item without removing it.
    ///
    /// Returns `None` if the stack is empty.
    pub fn peek(&self) -> Option<&T> {
        self.items.last()
    }

    /// Returns the number of items in the stack.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Returns `true` if the stack is empty.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Clears all items from the stack.
    pub fn clear(&mut self) {
        self.items.clear();
    }
}

impl<T> Default for Stack<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// A generic queue data structure implementing FIFO (First-In-First-Out) behavior.
///
/// # Examples
///
/// ```
/// use standard_library::Queue;
///
/// let mut queue = Queue::new();
/// queue.enqueue(1);
/// queue.enqueue(2);
///
/// assert_eq!(queue.dequeue(), Some(1));
/// assert_eq!(queue.dequeue(), Some(2));
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialization", derive(Serialize, Deserialize))]
pub struct Queue<T> {
    items: Vec<T>,
}

impl<T> Queue<T> {
    /// Creates a new empty queue.
    pub fn new() -> Self {
        Queue { items: Vec::new() }
    }

    /// Adds an item to the back of the queue.
    pub fn enqueue(&mut self, item: T) {
        self.items.push(item);
    }

    /// Removes and returns the front item from the queue.
    ///
    /// Returns `None` if the queue is empty.
    pub fn dequeue(&mut self) -> Option<T> {
        if self.items.is_empty() {
            None
        } else {
            Some(self.items.remove(0))
        }
    }

    /// Returns a reference to the front item without removing it.
    pub fn front(&self) -> Option<&T> {
        self.items.first()
    }

    /// Returns the number of items in the queue.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Returns `true` if the queue is empty.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
}

impl<T> Default for Queue<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod stack_tests {
        use super::*;

        #[test]
        fn test_new_stack() {
            let stack: Stack<i32> = Stack::new();
            assert!(stack.is_empty());
            assert_eq!(stack.len(), 0);
        }

        #[test]
        fn test_push_pop() {
            let mut stack = Stack::new();
            stack.push(1);
            stack.push(2);
            stack.push(3);

            assert_eq!(stack.pop(), Some(3));
            assert_eq!(stack.pop(), Some(2));
            assert_eq!(stack.pop(), Some(1));
            assert_eq!(stack.pop(), None);
        }

        #[test]
        fn test_peek() {
            let mut stack = Stack::new();
            assert_eq!(stack.peek(), None);

            stack.push(42);
            assert_eq!(stack.peek(), Some(&42));
            assert_eq!(stack.len(), 1); // peek doesn't remove
        }

        #[test]
        fn test_clear() {
            let mut stack = Stack::new();
            stack.push(1);
            stack.push(2);
            stack.clear();
            assert!(stack.is_empty());
        }
    }

    mod queue_tests {
        use super::*;

        #[test]
        fn test_new_queue() {
            let queue: Queue<i32> = Queue::new();
            assert!(queue.is_empty());
            assert_eq!(queue.len(), 0);
        }

        #[test]
        fn test_enqueue_dequeue() {
            let mut queue = Queue::new();
            queue.enqueue(1);
            queue.enqueue(2);
            queue.enqueue(3);

            assert_eq!(queue.dequeue(), Some(1));
            assert_eq!(queue.dequeue(), Some(2));
            assert_eq!(queue.dequeue(), Some(3));
            assert_eq!(queue.dequeue(), None);
        }

        #[test]
        fn test_front() {
            let mut queue = Queue::new();
            assert_eq!(queue.front(), None);

            queue.enqueue(42);
            assert_eq!(queue.front(), Some(&42));
            assert_eq!(queue.len(), 1);
        }
    }

    #[cfg(feature = "serialization")]
    mod serialization_tests {
        use super::*;

        #[test]
        fn test_stack_serialization() {
            let mut stack = Stack::new();
            stack.push(1);
            stack.push(2);
            stack.push(3);

            let json = serde_json::to_string(&stack).unwrap();
            let deserialized: Stack<i32> = serde_json::from_str(&json).unwrap();

            assert_eq!(stack, deserialized);
        }
    }
}
