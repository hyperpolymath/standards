//! Application state management

use crate::models::User;
use std::collections::HashMap;
use std::sync::RwLock;

/// Shared application state
pub struct AppState {
    /// In-memory user storage (in production, this would be a database)
    pub users: RwLock<HashMap<String, User>>,
}

impl AppState {
    /// Create new application state with some initial data
    pub fn new() -> Self {
        let mut users = HashMap::new();

        // Add some sample users
        users.insert(
            "1".to_string(),
            User {
                id: "1".to_string(),
                name: "Alice Johnson".to_string(),
                email: "alice@example.com".to_string(),
            },
        );
        users.insert(
            "2".to_string(),
            User {
                id: "2".to_string(),
                name: "Bob Smith".to_string(),
                email: "bob@example.com".to_string(),
            },
        );

        Self {
            users: RwLock::new(users),
        }
    }
}

impl Default for AppState {
    fn default() -> Self {
        Self::new()
    }
}
