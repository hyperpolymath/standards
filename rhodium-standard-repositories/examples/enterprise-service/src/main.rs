//! # Enterprise Service
//!
//! A production-ready REST API service demonstrating Gold-level Rhodium compliance.
//!
//! This service provides a simple user management API with:
//! - RESTful endpoints
//! - Error handling
//! - Logging and tracing
//! - Configuration management
//! - Health checks
//! - Metrics endpoints

use axum::{
    extract::{Path, State},
    http::StatusCode,
    response::IntoResponse,
    routing::{get, post},
    Json, Router,
};
use std::sync::Arc;
use tokio::net::TcpListener;
use tower_http::{compression::CompressionLayer, trace::TraceLayer};
use tracing::{info, Level};
use tracing_subscriber::FmtSubscriber;

pub mod error;
pub mod handlers;
pub mod models;
pub mod state;

use error::AppError;
use models::{BuildInfo, HealthResponse, User};
use state::AppState;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing
    let subscriber = FmtSubscriber::builder()
        .with_max_level(Level::INFO)
        .finish();
    tracing::subscriber::set_global_default(subscriber)?;

    info!("Starting Enterprise Service");

    // Initialize application state
    let state = Arc::new(AppState::new());

    // Build router
    let app = Router::new()
        .route("/health", get(health_check))
        .route("/build-info", get(build_info))
        .route("/api/v1/users", get(handlers::list_users).post(handlers::create_user))
        .route("/api/v1/users/:id", get(handlers::get_user).delete(handlers::delete_user))
        .layer(CompressionLayer::new())
        .layer(TraceLayer::new_for_http())
        .with_state(state);

    // Start server
    let addr = "0.0.0.0:3000";
    let listener = TcpListener::bind(addr).await?;
    info!("Server listening on {}", addr);

    axum::serve(listener, app).await?;

    Ok(())
}

/// Health check endpoint
async fn health_check() -> Json<HealthResponse> {
    Json(HealthResponse {
        status: "healthy".to_string(),
        version: env!("CARGO_PKG_VERSION").to_string(),
    })
}

/// Build information endpoint for provenance tracking
async fn build_info() -> Json<BuildInfo> {
    Json(BuildInfo::new())
}
