//! Performance benchmarks for the API

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use enterprise_service::{handlers, models::CreateUserRequest, state::AppState};
use std::sync::Arc;
use axum::extract::State;
use axum::Json;

fn bench_list_users(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let state = Arc::new(AppState::new());

    c.bench_function("list_users", |b| {
        b.to_async(&rt).iter(|| async {
            black_box(handlers::list_users(State(state.clone())).await)
        })
    });
}

fn bench_create_user(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let state = Arc::new(AppState::new());

    c.bench_function("create_user", |b| {
        b.to_async(&rt).iter(|| async {
            let request = CreateUserRequest {
                name: "Benchmark User".to_string(),
                email: "bench@example.com".to_string(),
            };
            black_box(handlers::create_user(State(state.clone()), Json(request)).await)
        })
    });
}

criterion_group!(benches, bench_list_users, bench_create_user);
criterion_main!(benches);
