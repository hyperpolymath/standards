//! # Minimal Rust Project
//!
//! This is a simple library demonstrating Bronze-level Rhodium compliance.
//! It provides basic mathematical operations as an example.

/// Adds two numbers together.
///
/// # Examples
///
/// ```
/// use minimal_rust_project::add;
///
/// let result = add(2, 3);
/// assert_eq!(result, 5);
/// ```
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

/// Multiplies two numbers together.
///
/// # Examples
///
/// ```
/// use minimal_rust_project::multiply;
///
/// let result = multiply(4, 5);
/// assert_eq!(result, 20);
/// ```
pub fn multiply(a: i32, b: i32) -> i32 {
    a * b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(add(2, 2), 4);
        assert_eq!(add(-1, 1), 0);
        assert_eq!(add(0, 0), 0);
    }

    #[test]
    fn test_multiply() {
        assert_eq!(multiply(2, 3), 6);
        assert_eq!(multiply(-2, 3), -6);
        assert_eq!(multiply(0, 5), 0);
    }
}
