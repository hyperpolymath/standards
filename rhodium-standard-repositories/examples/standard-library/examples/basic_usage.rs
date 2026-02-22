//! Basic usage example for the standard-library crate

use standard_library::{Queue, Stack};

fn main() {
    println!("Standard Library Examples\n");

    // Stack example
    println!("=== Stack (LIFO) ===");
    let mut stack = Stack::new();

    stack.push("First");
    stack.push("Second");
    stack.push("Third");

    println!("Pushed: First, Second, Third");
    println!("Popping items:");

    while let Some(item) = stack.pop() {
        println!("  - {}", item);
    }

    // Queue example
    println!("\n=== Queue (FIFO) ===");
    let mut queue = Queue::new();

    queue.enqueue("First");
    queue.enqueue("Second");
    queue.enqueue("Third");

    println!("Enqueued: First, Second, Third");
    println!("Dequeuing items:");

    while let Some(item) = queue.dequeue() {
        println!("  - {}", item);
    }
}
