//! Error types and handling

use axum::{
    http::StatusCode,
    response::{IntoResponse, Response},
    Json,
};
use serde::Serialize;
use thiserror::Error;

/// Application-wide error type
#[derive(Error, Debug)]
pub enum AppError {
    #[error("User not found: {0}")]
    UserNotFound(String),

    #[error("Invalid input: {0}")]
    InvalidInput(String),

    #[error("Internal server error")]
    Internal,
}

#[derive(Serialize)]
struct ErrorResponse {
    error: String,
    message: String,
}

impl IntoResponse for AppError {
    fn into_response(self) -> Response {
        let (status, error_type, message) = match self {
            AppError::UserNotFound(id) => (
                StatusCode::NOT_FOUND,
                "UserNotFound",
                format!("User {} not found", id),
            ),
            AppError::InvalidInput(msg) => (StatusCode::BAD_REQUEST, "InvalidInput", msg),
            AppError::Internal => (
                StatusCode::INTERNAL_SERVER_ERROR,
                "InternalError",
                "An internal error occurred".to_string(),
            ),
        };

        let body = Json(ErrorResponse {
            error: error_type.to_string(),
            message,
        });

        (status, body).into_response()
    }
}
