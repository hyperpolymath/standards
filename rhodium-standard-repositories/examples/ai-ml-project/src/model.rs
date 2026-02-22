//! Model management

use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct ModelMetadata {
    pub name: String,
    pub version: String,
    pub created_at: String,
    pub accuracy: f64,
}

impl ModelMetadata {
    pub fn new(name: String, version: String, accuracy: f64) -> Self {
        Self {
            name,
            version,
            created_at: "2025-11-22T00:00:00Z".to_string(),
            accuracy,
        }
    }
}
