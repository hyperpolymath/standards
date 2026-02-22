//! Inference server

use axum::{
    extract::Json,
    routing::post,
    Router,
};
use serde::{Deserialize, Serialize};
use ai_ml_project::inference;

#[derive(Deserialize)]
struct PredictionRequest {
    features: Vec<f64>,
}

#[derive(Serialize)]
struct PredictionResponse {
    prediction: f64,
}

async fn predict(Json(req): Json<PredictionRequest>) -> Json<PredictionResponse> {
    let prediction = inference::predict(&req.features);
    Json(PredictionResponse { prediction })
}

#[tokio::main]
async fn main() {
    let app = Router::new().route("/predict", post(predict));

    println!("Inference server running on http://localhost:8080");
    let listener = tokio::net::TcpListener::bind("0.0.0.0:8080").await.unwrap();
    axum::serve(listener, app).await.unwrap();
}
