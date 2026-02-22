//! Request handlers

use crate::{
    error::AppError,
    models::{CreateUserRequest, User},
    state::AppState,
};
use axum::{
    extract::{Path, State},
    http::StatusCode,
    Json,
};
use std::sync::Arc;
use tracing::{info, warn};

/// List all users
pub async fn list_users(State(state): State<Arc<AppState>>) -> Json<Vec<User>> {
    let users = state.users.read().unwrap();
    let user_list: Vec<User> = users.values().cloned().collect();
    info!("Listed {} users", user_list.len());
    Json(user_list)
}

/// Get a specific user by ID
pub async fn get_user(
    State(state): State<Arc<AppState>>,
    Path(id): Path<String>,
) -> Result<Json<User>, AppError> {
    let users = state.users.read().unwrap();

    users
        .get(&id)
        .cloned()
        .map(Json)
        .ok_or_else(|| {
            warn!("User not found: {}", id);
            AppError::UserNotFound(id)
        })
}

/// Create a new user
pub async fn create_user(
    State(state): State<Arc<AppState>>,
    Json(payload): Json<CreateUserRequest>,
) -> Result<(StatusCode, Json<User>), AppError> {
    // Validate input
    if payload.name.is_empty() {
        return Err(AppError::InvalidInput("Name cannot be empty".to_string()));
    }
    if payload.email.is_empty() || !payload.email.contains('@') {
        return Err(AppError::InvalidInput("Invalid email address".to_string()));
    }

    let mut users = state.users.write().unwrap();
    let id = (users.len() + 1).to_string();

    let user = User {
        id: id.clone(),
        name: payload.name,
        email: payload.email,
    };

    users.insert(id, user.clone());
    info!("Created user: {:?}", user);

    Ok((StatusCode::CREATED, Json(user)))
}

/// Delete a user
pub async fn delete_user(
    State(state): State<Arc<AppState>>,
    Path(id): Path<String>,
) -> Result<StatusCode, AppError> {
    let mut users = state.users.write().unwrap();

    users
        .remove(&id)
        .map(|_| {
            info!("Deleted user: {}", id);
            StatusCode::NO_CONTENT
        })
        .ok_or_else(|| {
            warn!("Attempted to delete non-existent user: {}", id);
            AppError::UserNotFound(id)
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_list_users() {
        let state = Arc::new(AppState::new());
        let result = list_users(State(state)).await;
        assert_eq!(result.0.len(), 2);
    }

    #[tokio::test]
    async fn test_get_user() {
        let state = Arc::new(AppState::new());
        let result = get_user(State(state), Path("1".to_string())).await;
        assert!(result.is_ok());
        assert_eq!(result.unwrap().0.id, "1");
    }

    #[tokio::test]
    async fn test_get_nonexistent_user() {
        let state = Arc::new(AppState::new());
        let result = get_user(State(state), Path("999".to_string())).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_create_user() {
        let state = Arc::new(AppState::new());
        let request = CreateUserRequest {
            name: "Test User".to_string(),
            email: "test@example.com".to_string(),
        };

        let result = create_user(State(state.clone()), Json(request)).await;
        assert!(result.is_ok());

        let (status, user) = result.unwrap();
        assert_eq!(status, StatusCode::CREATED);
        assert_eq!(user.0.name, "Test User");
    }

    #[tokio::test]
    async fn test_create_user_invalid_email() {
        let state = Arc::new(AppState::new());
        let request = CreateUserRequest {
            name: "Test User".to_string(),
            email: "invalid-email".to_string(),
        };

        let result = create_user(State(state), Json(request)).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_delete_user() {
        let state = Arc::new(AppState::new());
        let result = delete_user(State(state), Path("1".to_string())).await;
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), StatusCode::NO_CONTENT);
    }
}
