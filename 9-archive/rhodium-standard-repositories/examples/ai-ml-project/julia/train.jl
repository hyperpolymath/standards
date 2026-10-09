#!/usr/bin/env julia
#=
Model training script for AI/ML project.
Fully ported to Julia.
=#

using LinearAlgebra
using JSON
using Dates
using Random

mutable struct SimpleModel
    weights::Union{Vector{Float64}, Nothing}
    bias::Union{Float64, Nothing}
end

SimpleModel() = SimpleModel(nothing, nothing)

function train!(model::SimpleModel, X::Matrix{Float64}, y::Vector{Float64})
    # Add bias term
    X_with_bias = [X ones(size(X, 1))]
    
    # Normal equation: theta = (X^T X)^-1 X^T y
    # In Julia, \ is the efficient least squares solver
    theta = X_with_bias \ y
    
    model.weights = theta[1:end-1]
    model.bias = theta[end]
end

function predict(model::SimpleModel, X::Matrix{Float64})
    return X * model.weights .+ model.bias
end

function evaluate(model::SimpleModel, X::Matrix{Float64}, y::Vector{Float64})
    predictions = predict(model, X)
    mse = sum((predictions .- y).^2) / length(y)
    return Dict("mse" => mse)
end

function save_model(model::SimpleModel, path::String)
    mkpath(dirname(path))
    open(path, "w") do f
        data = Dict(
            "weights" => model.weights,
            "bias" => model.bias,
            "trained_at" => string(now(UTC))
        )
        JSON.print(f, data, 2)
    end
end

function generate_synthetic_data(n_samples=1000)
    Random.seed!(42)
    X = randn(n_samples, 3)
    y = 0.5 .* X[:, 1] .+ 0.3 .* X[:, 2] .+ 0.2 .* X[:, 3] .+ 0.1 .+ randn(n_samples) .* 0.1
    return X, y
end

function main()
    println("Generating synthetic data...")
    X, y = generate_synthetic_data()

    # Split data
    split_idx = floor(Int, 0.8 * size(X, 1))
    X_train, X_test = X[1:split_idx, :], X[split_idx+1:end, :]
    y_train, y_test = y[1:split_idx], y[split_idx+1:end]

    println("Training model...")
    model = SimpleModel()
    train!(model, X_train, y_train)

    println("Evaluating model...")
    metrics = evaluate(model, X_test, y_test)
    @printf "Test MSE: %.4f\n" metrics["mse"]

    println("Saving model...")
    save_model(model, "models/linear_model.json")

    println("Training complete!")
end

# Add printf functionality
using Printf

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
