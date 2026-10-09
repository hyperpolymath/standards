//! Data preprocessing

/// Normalize features to [0, 1] range
pub fn normalize(data: &[f64]) -> Vec<f64> {
    let min = data.iter().cloned().fold(f64::INFINITY, f64::min);
    let max = data.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
    let range = max - min;

    if range == 0.0 {
        return data.to_vec();
    }

    data.iter().map(|x| (x - min) / range).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_normalize() {
        let data = vec![1.0, 2.0, 3.0, 4.0, 5.0];
        let normalized = normalize(&data);
        assert_eq!(normalized[0], 0.0);
        assert_eq!(normalized[4], 1.0);
    }
}
