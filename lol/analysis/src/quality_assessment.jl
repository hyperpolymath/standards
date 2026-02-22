# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

"""
    QualityAssessment

Corpus quality scoring and VeriSimDB-compatible weak point generation.
Combines multiple quality dimensions into a composite score.
Adapted from Axiology.jl multi-criteria scoring patterns.
"""
module QualityAssessment

using StatsBase

include("corpus_stats.jl")
using .CorpusStats

export quality_score, detect_issues, QualityReport

struct QualityReport
    completeness::Float64
    encoding_quality::Float64
    statistical_normality::Float64
    vocabulary_richness::Float64
    composite_score::Float64
end

"""
    quality_score(text::String) -> Float64

Compute a composite quality score from 0.0 (worst) to 1.0 (best).
"""
function quality_score(text::String)
    isempty(text) && return 0.0

    report = full_assessment(text)
    return report.composite_score
end

"""
    full_assessment(text::String) -> QualityReport

Compute all quality dimensions for a text.
"""
function full_assessment(text::String)
    isempty(text) && return QualityReport(0.0, 0.0, 0.0, 0.0, 0.0)

    completeness = assess_completeness(text)
    encoding = assess_encoding(text)
    normality = assess_statistical_normality(text)
    richness = assess_vocabulary_richness(text)

    # Weighted composite (encoding issues are most critical)
    composite = 0.3 * completeness + 0.3 * encoding + 0.2 * normality + 0.2 * richness

    return QualityReport(completeness, encoding, normality, richness, composite)
end

"""
    assess_completeness(text::String) -> Float64

Score based on text length and content density.
Very short texts score low, diminishing returns above threshold.
"""
function assess_completeness(text::String)
    len = length(text)
    len == 0 && return 0.0

    # Sigmoid-like curve: reaches ~0.9 at 1000 chars
    return 1.0 - exp(-len / 500.0)
end

"""
    assess_encoding(text::String) -> Float64

Score based on absence of encoding errors.
Checks for replacement characters, null bytes, and mojibake.
"""
function assess_encoding(text::String)
    total_chars = length(text)
    total_chars == 0 && return 1.0

    bad_chars = 0

    for c in text
        # Unicode replacement character
        if c == '\ufffd'
            bad_chars += 1
        # Null byte
        elseif c == '\0'
            bad_chars += 1
        # Control characters (except common whitespace)
        elseif iscontrol(c) && c ∉ ['\n', '\r', '\t']
            bad_chars += 1
        end
    end

    bad_ratio = bad_chars / total_chars
    return max(0.0, 1.0 - bad_ratio * 100)  # 1% bad chars = score 0
end

"""
    assess_statistical_normality(text::String) -> Float64

Score based on how "normal" the text looks statistically.
Uses Zipf's law conformance as a proxy.
"""
function assess_statistical_normality(text::String)
    words = split(lowercase(text))
    length(words) < 10 && return 0.5  # Not enough data

    # Check Zipf's law coefficient (should be near 1.0 for natural text)
    counts = countmap(words)
    freqs = sort(collect(values(counts)), rev=true)
    n = length(freqs)
    n < 2 && return 0.5

    # Estimate alpha via linear regression on log-log
    log_ranks = [log(i) for i in 1:n]
    log_freqs = [log(f) for f in freqs]

    mean_lr = mean(log_ranks)
    mean_lf = mean(log_freqs)

    cov_sum = sum((log_ranks[i] - mean_lr) * (log_freqs[i] - mean_lf) for i in 1:n)
    var_sum = sum((log_ranks[i] - mean_lr)^2 for i in 1:n)

    var_sum == 0 && return 0.5
    alpha = -cov_sum / var_sum

    # Score: 1.0 when alpha ≈ 1.0, decreasing as it deviates
    deviation = abs(alpha - 1.0)
    return max(0.0, 1.0 - deviation)
end

"""
    assess_vocabulary_richness(text::String) -> Float64

Score based on type-token ratio and hapax legomena.
"""
function assess_vocabulary_richness(text::String)
    words = split(lowercase(text))
    isempty(words) && return 0.0

    ttr = length(Set(words)) / length(words)

    # TTR between 0.3-0.8 is typical for natural text
    if ttr < 0.1
        return ttr * 3  # Very repetitive
    elseif ttr > 0.95
        return 0.8  # Suspiciously all-unique (very short text)
    else
        return min(1.0, ttr * 1.5)  # Normal range
    end
end

"""
    detect_issues(text::String, language::String) -> Vector{Dict{String,Any}}

Detect quality issues and return VeriSimDB-compatible weak points.
"""
function detect_issues(text::String, language::String)
    weak_points = Dict{String,Any}[]
    report = full_assessment(text)

    # Low completeness
    if report.completeness < 0.3
        push!(weak_points, Dict(
            "category" => "truncated-content",
            "severity" => report.completeness < 0.1 ? "high" : "medium",
            "location" => language,
            "description" => "Very short text (completeness score: $(round(report.completeness, digits=2)))",
            "language" => language,
            "source" => nothing
        ))
    end

    # Encoding issues
    if report.encoding_quality < 0.9
        push!(weak_points, Dict(
            "category" => "encoding-error",
            "severity" => report.encoding_quality < 0.5 ? "high" : "medium",
            "location" => language,
            "description" => "Encoding quality issues detected (score: $(round(report.encoding_quality, digits=2)))",
            "language" => language,
            "source" => nothing
        ))
    end

    # Statistical anomaly
    if report.statistical_normality < 0.3
        push!(weak_points, Dict(
            "category" => "statistical-outlier",
            "severity" => "medium",
            "location" => language,
            "description" => "Text deviates from expected statistical properties (normality: $(round(report.statistical_normality, digits=2)))",
            "language" => language,
            "source" => nothing
        ))
    end

    # Very low vocabulary richness (possibly machine-generated or corrupted)
    if report.vocabulary_richness < 0.2
        push!(weak_points, Dict(
            "category" => "duplicate-content",
            "severity" => "medium",
            "location" => language,
            "description" => "Extremely low vocabulary diversity (richness: $(round(report.vocabulary_richness, digits=2)))",
            "language" => language,
            "source" => nothing
        ))
    end

    return weak_points
end

end # module
