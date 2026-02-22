// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

/**
 * Statistics Utilities Tests
 */

open Vitest

let approximately = (a, b, ~tolerance=0.0001, ()) => {
  Math.abs(a -. b) < tolerance
}

describe("Statistics.Basic", () => {
  describe("sum", () => {
    test("sums array of floats", () => {
      expect(Statistics.Basic.sum([1.0, 2.0, 3.0, 4.0]))->toBe(10.0)
    })

    test("returns 0 for empty array", () => {
      expect(Statistics.Basic.sum([]))->toBe(0.0)
    })
  })

  describe("mean", () => {
    test("calculates mean correctly", () => {
      expect(Statistics.Basic.mean([1.0, 2.0, 3.0, 4.0, 5.0]))->toBe(3.0)
    })

    test("returns 0 for empty array", () => {
      expect(Statistics.Basic.mean([]))->toBe(0.0)
    })
  })

  describe("variance", () => {
    test("calculates variance correctly", () => {
      let result = Statistics.Basic.variance([2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0])
      expect(approximately(result, 4.0, ()))->toBe(true)
    })

    test("returns 0 for single element", () => {
      expect(Statistics.Basic.variance([5.0]))->toBe(0.0)
    })

    test("returns 0 for empty array", () => {
      expect(Statistics.Basic.variance([]))->toBe(0.0)
    })
  })

  describe("standardDeviation", () => {
    test("calculates standard deviation correctly", () => {
      let result = Statistics.Basic.standardDeviation([2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0])
      expect(approximately(result, 2.0, ()))->toBe(true)
    })
  })

  describe("min and max", () => {
    test("min returns minimum value", () => {
      expect(Statistics.Basic.min([3.0, 1.0, 4.0, 1.0, 5.0]))->toEqual(Some(1.0))
    })

    test("max returns maximum value", () => {
      expect(Statistics.Basic.max([3.0, 1.0, 4.0, 1.0, 5.0]))->toEqual(Some(5.0))
    })

    test("min returns None for empty array", () => {
      expect(Statistics.Basic.min([]))->toEqual(None)
    })

    test("max returns None for empty array", () => {
      expect(Statistics.Basic.max([]))->toEqual(None)
    })
  })
})

describe("Statistics.Information", () => {
  describe("entropy", () => {
    test("calculates entropy for uniform distribution", () => {
      // Uniform distribution over 4 outcomes: H = log2(4) = 2
      let dist = [0.25, 0.25, 0.25, 0.25]
      let result = Statistics.Information.entropy(dist)
      expect(approximately(result, 2.0, ()))->toBe(true)
    })

    test("returns 0 for deterministic distribution", () => {
      let dist = [1.0, 0.0, 0.0, 0.0]
      let result = Statistics.Information.entropy(dist)
      expect(approximately(result, 0.0, ()))->toBe(true)
    })

    test("handles binary distribution correctly", () => {
      // Fair coin: H = 1 bit
      let dist = [0.5, 0.5]
      let result = Statistics.Information.entropy(dist)
      expect(approximately(result, 1.0, ()))->toBe(true)
    })
  })

  describe("klDivergence", () => {
    test("returns 0 for identical distributions", () => {
      let p = [0.25, 0.25, 0.25, 0.25]
      let result = Statistics.Information.klDivergence(p, p)
      expect(approximately(result, 0.0, ()))->toBe(true)
    })

    test("is asymmetric", () => {
      let p = [0.5, 0.5]
      let q = [0.9, 0.1]
      let kl_pq = Statistics.Information.klDivergence(p, q)
      let kl_qp = Statistics.Information.klDivergence(q, p)
      expect(kl_pq != kl_qp)->toBe(true)
    })

    test("returns NaN for mismatched lengths", () => {
      let p = [0.5, 0.5]
      let q = [0.33, 0.33, 0.34]
      let result = Statistics.Information.klDivergence(p, q)
      expect(Float.isNaN(result))->toBe(true)
    })
  })

  describe("symmetricKL", () => {
    test("is symmetric", () => {
      let p = [0.5, 0.5]
      let q = [0.9, 0.1]
      let skl_pq = Statistics.Information.symmetricKL(p, q)
      let skl_qp = Statistics.Information.symmetricKL(q, p)
      expect(approximately(skl_pq, skl_qp, ()))->toBe(true)
    })
  })

  describe("jensenShannon", () => {
    test("is symmetric", () => {
      let p = [0.5, 0.5]
      let q = [0.9, 0.1]
      let js_pq = Statistics.Information.jensenShannon(p, q)
      let js_qp = Statistics.Information.jensenShannon(q, p)
      expect(approximately(js_pq, js_qp, ()))->toBe(true)
    })

    test("returns 0 for identical distributions", () => {
      let p = [0.25, 0.25, 0.25, 0.25]
      let result = Statistics.Information.jensenShannon(p, p)
      expect(approximately(result, 0.0, ()))->toBe(true)
    })

    test("is bounded between 0 and 1", () => {
      let p = [1.0, 0.0]
      let q = [0.0, 1.0]
      let result = Statistics.Information.jensenShannon(p, q)
      expect(result >= 0.0 && result <= 1.0)->toBe(true)
    })
  })
})

describe("Statistics.Distance", () => {
  describe("euclidean", () => {
    test("calculates euclidean distance correctly", () => {
      let a = [0.0, 0.0]
      let b = [3.0, 4.0]
      expect(Statistics.Distance.euclidean(a, b))->toBe(5.0)
    })

    test("returns 0 for identical vectors", () => {
      let a = [1.0, 2.0, 3.0]
      expect(Statistics.Distance.euclidean(a, a))->toBe(0.0)
    })
  })

  describe("cosine", () => {
    test("returns 0 for identical vectors", () => {
      let a = [1.0, 2.0, 3.0]
      let result = Statistics.Distance.cosine(a, a)
      expect(approximately(result, 0.0, ()))->toBe(true)
    })

    test("returns 1 for orthogonal vectors", () => {
      let a = [1.0, 0.0]
      let b = [0.0, 1.0]
      let result = Statistics.Distance.cosine(a, b)
      expect(approximately(result, 1.0, ()))->toBe(true)
    })
  })

  describe("jaccard", () => {
    test("returns 0 for identical sets", () => {
      let a = [1.0, 1.0, 0.0]
      expect(Statistics.Distance.jaccard(a, a))->toBe(0.0)
    })

    test("returns 1 for disjoint sets", () => {
      let a = [1.0, 0.0, 0.0]
      let b = [0.0, 1.0, 1.0]
      expect(Statistics.Distance.jaccard(a, b))->toBe(1.0)
    })
  })
})

describe("Statistics.Normalization", () => {
  describe("normalize", () => {
    test("normalizes to sum to 1", () => {
      let result = Statistics.Normalization.normalize([1.0, 2.0, 3.0, 4.0])
      let sum = Statistics.Basic.sum(result)
      expect(approximately(sum, 1.0, ()))->toBe(true)
    })

    test("preserves proportions", () => {
      let result = Statistics.Normalization.normalize([1.0, 3.0])
      expect(approximately(Array.getUnsafe(result, 0), 0.25, ()))->toBe(true)
      expect(approximately(Array.getUnsafe(result, 1), 0.75, ()))->toBe(true)
    })
  })

  describe("minMaxNormalize", () => {
    test("scales to [0, 1] range", () => {
      let result = Statistics.Normalization.minMaxNormalize([10.0, 20.0, 30.0])
      expect(Array.getUnsafe(result, 0))->toBe(0.0)
      expect(Array.getUnsafe(result, 1))->toBe(0.5)
      expect(Array.getUnsafe(result, 2))->toBe(1.0)
    })
  })

  describe("zScoreNormalize", () => {
    test("results in mean of 0", () => {
      let result = Statistics.Normalization.zScoreNormalize([1.0, 2.0, 3.0, 4.0, 5.0])
      let mean = Statistics.Basic.mean(result)
      expect(approximately(mean, 0.0, ()))->toBe(true)
    })

    test("results in standard deviation of 1", () => {
      let result = Statistics.Normalization.zScoreNormalize([1.0, 2.0, 3.0, 4.0, 5.0])
      let sd = Statistics.Basic.standardDeviation(result)
      expect(approximately(sd, 1.0, ()))->toBe(true)
    })
  })
})
