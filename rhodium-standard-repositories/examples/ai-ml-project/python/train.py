#!/usr/bin/env python3
"""
Model training script for AI/ML project.
"""

import json
import numpy as np
from datetime import datetime
from pathlib import Path


class SimpleModel:
    """Simple linear model for demonstration."""

    def __init__(self):
        self.weights = None
        self.bias = None

    def train(self, X, y):
        """Train the model using linear regression."""
        # Add bias term
        X_with_bias = np.c_[X, np.ones(X.shape[0])]

        # Normal equation: theta = (X^T X)^-1 X^T y
        theta = np.linalg.lstsq(X_with_bias, y, rcond=None)[0]

        self.weights = theta[:-1]
        self.bias = theta[-1]

    def predict(self, X):
        """Make predictions."""
        return X @ self.weights + self.bias

    def evaluate(self, X, y):
        """Evaluate model accuracy."""
        predictions = self.predict(X)
        mse = np.mean((predictions - y) ** 2)
        return {"mse": float(mse)}

    def save(self, path):
        """Save model parameters."""
        Path(path).parent.mkdir(parents=True, exist_ok=True)
        with open(path, 'w') as f:
            json.dump({
                "weights": self.weights.tolist(),
                "bias": float(self.bias),
                "trained_at": datetime.utcnow().isoformat()
            }, f, indent=2)


def generate_synthetic_data(n_samples=1000):
    """Generate synthetic dataset for training."""
    np.random.seed(42)
    X = np.random.randn(n_samples, 3)
    y = 0.5 * X[:, 0] + 0.3 * X[:, 1] + 0.2 * X[:, 2] + 0.1 + np.random.randn(n_samples) * 0.1
    return X, y


def main():
    """Main training pipeline."""
    print("Generating synthetic data...")
    X, y = generate_synthetic_data()

    # Split data
    split_idx = int(0.8 * len(X))
    X_train, X_test = X[:split_idx], X[split_idx:]
    y_train, y_test = y[:split_idx], y[split_idx:]

    print("Training model...")
    model = SimpleModel()
    model.train(X_train, y_train)

    print("Evaluating model...")
    metrics = model.evaluate(X_test, y_test)
    print(f"Test MSE: {metrics['mse']:.4f}")

    print("Saving model...")
    model.save("models/linear_model.json")

    print("Training complete!")


if __name__ == "__main__":
    main()
