# Model Card: Simple Linear Model

## Model Details

- **Model Name**: Simple Linear Regression
- **Version**: 1.0.0
- **Date**: 2025-11-22
- **Model Type**: Linear Regression
- **Framework**: Scikit-learn (training), Rust (inference)

## Intended Use

### Primary Use Cases

- Demonstration of ML model deployment
- Educational example of Rhodium compliance
- Template for production ML projects

### Out-of-Scope Uses

- Production predictions (this is a demonstration model)
- High-stakes decision making
- Safety-critical applications

## Training Data

- **Source**: Synthetically generated data
- **Size**: 1000 samples, 3 features
- **Distribution**: Normal distribution with known coefficients
- **Preprocessing**: Standardization applied

## Model Architecture

Simple linear regression model:
- Input: 3 features
- Output: Single continuous value
- Parameters: 3 weights + 1 bias

## Performance

### Metrics

- **MSE (Test)**: ~0.01
- **Training Time**: < 1 second
- **Inference Latency**: < 1ms

### Limitations

- Assumes linear relationships
- No handling of missing values
- No categorical feature support

## Ethical Considerations

- Model trained on synthetic data (no privacy concerns)
- No known biases (synthetic distribution)
- No fairness issues (demonstration only)

## Maintenance

- **Owner**: Rhodium Framework Contributors
- **Update Frequency**: As needed
- **Monitoring**: MSE tracking recommended
