//! Inference engine

use ndarray::Array1;

/// Performs inference on input features
pub fn predict(features: &[f64]) -> f64 {
    // Simple linear model for demonstration
    let weights = vec![0.5, 0.3, 0.2];
    let bias = 0.1;

    features.iter()
        .zip(weights.iter())
        .map(|(x, w)| x * w)
        .sum::<f64>() + bias
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_predict() {
        let features = vec![1.0, 2.0, 3.0];
        let result = predict(&features);
        assert!((result - 1.7).abs() < 0.001);
    }
}
