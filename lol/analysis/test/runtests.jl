# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

using Test

# Include modules directly for testing
include("../src/corpus_stats.jl")
include("../src/frequency_analysis.jl")
include("../src/zero_frequency.jl")
include("../src/quality_assessment.jl")

using .CorpusStats
using .FrequencyAnalysis
using .ZeroFrequency
using .QualityAssessment

@testset "LolAnalysis" begin

    @testset "CorpusStats" begin
        text = "In the beginning God created the heavens and the earth"

        @test CorpusStats.character_entropy(text) > 0.0
        @test CorpusStats.character_entropy("") == 0.0
        @test CorpusStats.character_entropy("aaaa") == 0.0

        @test CorpusStats.word_entropy(text) > 0.0
        @test CorpusStats.word_entropy("") == 0.0

        @test 0.0 < CorpusStats.type_token_ratio(text) <= 1.0
        @test CorpusStats.type_token_ratio("") == 0.0

        @test 0.0 <= CorpusStats.hapax_legomena_ratio(text) <= 1.0

        stats = CorpusStats.compute_all(text)
        @test stats.total_chars > 0
        @test stats.total_words > 0
        @test stats.unique_words > 0
        @test stats.mean_word_length > 0.0
    end

    @testset "FrequencyAnalysis" begin
        text = "the quick brown fox jumps over the lazy dog the fox"

        profile = FrequencyAnalysis.ngram_profile(text, 3)
        @test !isempty(profile)
        @test sum(values(profile)) ≈ 1.0 atol=0.01

        spectrum = FrequencyAnalysis.frequency_spectrum(text)
        @test !isempty(spectrum)
        @test all(t -> t[1] > 0 && t[2] > 0, spectrum)

        alpha = FrequencyAnalysis.zipf_coefficient(text)
        @test alpha > 0.0

        growth = FrequencyAnalysis.vocabulary_growth(text; step=2)
        @test !isempty(growth)
        @test growth[end][2] <= growth[end][1]  # Types <= tokens

        freqs = FrequencyAnalysis.ngram_frequencies(text, 2)
        @test !isempty(freqs)
        @test all(v -> v > 0, values(freqs))
    end

    @testset "ZeroFrequency" begin
        counts = Dict("the" => 5, "fox" => 3, "dog" => 1, "cat" => 1, "bird" => 2)

        smoothed = ZeroFrequency.good_turing_smooth(counts)
        @test !isempty(smoothed)
        @test all(v -> v > 0.0, values(smoothed))

        add_k = ZeroFrequency.add_k_smooth(counts, 100)
        @test !isempty(add_k)
        @test all(v -> v > 0.0, values(add_k))

        kn = ZeroFrequency.kneser_ney_smooth(counts)
        @test !isempty(kn)
        @test all(v -> v > 0.0, values(kn))

        unseen = ZeroFrequency.estimate_unseen(counts)
        @test unseen.observed_types == 5
        @test unseen.hapax_count == 2
        @test 0.0 < unseen.coverage <= 1.0
        @test unseen.unseen_estimate >= 0
    end

    @testset "QualityAssessment" begin
        good_text = "In the beginning God created the heavens and the earth. " *
                    "And the earth was without form, and void; and darkness was upon the face of the deep. " *
                    "And the Spirit of God moved upon the face of the waters."

        score = QualityAssessment.quality_score(good_text)
        @test 0.0 <= score <= 1.0
        @test score > 0.3  # Decent quality text should score reasonably

        @test QualityAssessment.quality_score("") == 0.0

        issues = QualityAssessment.detect_issues(good_text, "eng")
        @test issues isa Vector

        # Short text should trigger truncation warning
        short_issues = QualityAssessment.detect_issues("hello", "eng")
        @test any(wp -> wp["category"] == "truncated-content", short_issues)

        # Text with encoding errors
        bad_text = "Hello\ufffdworld\0test"
        bad_issues = QualityAssessment.detect_issues(bad_text * " " ^ 200, "eng")
        @test any(wp -> wp["category"] == "encoding-error", bad_issues)
    end
end

println("All tests passed!")
