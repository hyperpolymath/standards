//! Data models

use serde::{Deserialize, Serialize};

/// User model
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct User {
    pub id: String,
    pub name: String,
    pub email: String,
}

/// Request to create a new user
#[derive(Debug, Deserialize)]
pub struct CreateUserRequest {
    pub name: String,
    pub email: String,
}

/// Health check response
#[derive(Debug, Serialize)]
pub struct HealthResponse {
    pub status: String,
    pub version: String,
}

/// Build information for provenance tracking
#[derive(Debug, Serialize)]
pub struct BuildInfo {
    pub version: String,
    pub git_commit: Option<String>,
    pub build_timestamp: String,
    pub rust_version: String,
    pub target: String,
}

impl BuildInfo {
    pub fn new() -> Self {
        Self {
            version: env!("CARGO_PKG_VERSION").to_string(),
            git_commit: option_env!("GIT_COMMIT").map(String::from),
            build_timestamp: chrono::Utc::now().to_rfc3339(),
            rust_version: "1.70+".to_string(),
            target: std::env::consts::ARCH.to_string(),
        }
    }
}

impl Default for BuildInfo {
    fn default() -> Self {
        Self::new()
    }
}
