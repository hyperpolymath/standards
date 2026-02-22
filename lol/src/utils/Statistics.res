// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

/**
 * Statistical Utilities
 *
 * Mathematical and statistical functions for corpus analysis,
 * including KL-divergence, entropy, and clustering metrics.
 */

module Types = {
  type distribution = array<float>
  type matrix = array<array<float>>

  type distanceMetric =
    | Euclidean
    | Cosine
    | KLDivergence
    | JensenShannon
    | Jaccard
}

module Basic = {
  let sum = (arr: array<float>): float => {
    arr->Array.reduce(0.0, (acc, x) => acc +. x)
  }

  let mean = (arr: array<float>): float => {
    let len = Array.length(arr)
    if len == 0 {
      0.0
    } else {
      sum(arr) /. Float.fromInt(len)
    }
  }

  let variance = (arr: array<float>): float => {
    let len = Array.length(arr)
    if len == 0 {
      0.0
    } else {
      let m = mean(arr)
      let squaredDiffs = arr->Array.map(x => {
        let diff = x -. m
        diff *. diff
      })
      sum(squaredDiffs) /. Float.fromInt(len)
    }
  }

  let standardDeviation = (arr: array<float>): float => {
    Math.sqrt(variance(arr))
  }

  let min = (arr: array<float>): option<float> => {
    arr->Array.reduce(None, (acc, x) => {
      switch acc {
        | None => Some(x)
        | Some(m) => Some(x < m ? x : m)
      }
    })
  }

  let max = (arr: array<float>): option<float> => {
    arr->Array.reduce(None, (acc, x) => {
      switch acc {
        | None => Some(x)
        | Some(m) => Some(x > m ? x : m)
      }
    })
  }
}

module Information = {
  let epsilon = 1e-10

  // Shannon entropy: H(X) = -Σ p(x) log p(x)
  let entropy = (dist: array<float>): float => {
    dist->Array.reduce(0.0, (acc, p) => {
      if p > epsilon {
        acc -. p *. Math.log2(p)
      } else {
        acc
      }
    })
  }

  // KL-Divergence: D_KL(P||Q) = Σ P(i) log(P(i)/Q(i))
  let klDivergence = (p: array<float>, q: array<float>): float => {
    if Array.length(p) != Array.length(q) {
      Float.Constants.nan
    } else {
      let result = ref(0.0)
      for i in 0 to Array.length(p) - 1 {
        let pi = Array.getUnsafe(p, i)
        let qi = Array.getUnsafe(q, i)
        if pi > epsilon && qi > epsilon {
          result := result.contents +. pi *. Math.log2(pi /. qi)
        }
      }
      result.contents
    }
  }

  // Symmetric KL-Divergence: (D_KL(P||Q) + D_KL(Q||P)) / 2
  let symmetricKL = (p: array<float>, q: array<float>): float => {
    (klDivergence(p, q) +. klDivergence(q, p)) /. 2.0
  }

  // Jensen-Shannon Divergence: JSD(P||Q) = (D_KL(P||M) + D_KL(Q||M)) / 2
  // where M = (P + Q) / 2
  let jensenShannon = (p: array<float>, q: array<float>): float => {
    if Array.length(p) != Array.length(q) {
      Float.Constants.nan
    } else {
      let m = p->Array.mapWithIndex((pi, i) => {
        let qi = Array.getUnsafe(q, i)
        (pi +. qi) /. 2.0
      })
      (klDivergence(p, m) +. klDivergence(q, m)) /. 2.0
    }
  }
}

module Distance = {
  open Types

  let euclidean = (a: array<float>, b: array<float>): float => {
    if Array.length(a) != Array.length(b) {
      Float.Constants.nan
    } else {
      let sumSq = ref(0.0)
      for i in 0 to Array.length(a) - 1 {
        let diff = Array.getUnsafe(a, i) -. Array.getUnsafe(b, i)
        sumSq := sumSq.contents +. diff *. diff
      }
      Math.sqrt(sumSq.contents)
    }
  }

  let cosine = (a: array<float>, b: array<float>): float => {
    if Array.length(a) != Array.length(b) {
      Float.Constants.nan
    } else {
      let dot = ref(0.0)
      let normA = ref(0.0)
      let normB = ref(0.0)
      for i in 0 to Array.length(a) - 1 {
        let ai = Array.getUnsafe(a, i)
        let bi = Array.getUnsafe(b, i)
        dot := dot.contents +. ai *. bi
        normA := normA.contents +. ai *. ai
        normB := normB.contents +. bi *. bi
      }
      let denom = Math.sqrt(normA.contents) *. Math.sqrt(normB.contents)
      if denom > 0.0 {
        1.0 -. dot.contents /. denom  // Convert similarity to distance
      } else {
        0.0
      }
    }
  }

  let jaccard = (a: array<float>, b: array<float>): float => {
    // Treating as binary vectors (presence/absence)
    let intersection = ref(0)
    let union = ref(0)
    for i in 0 to Array.length(a) - 1 {
      let ai = Array.getUnsafe(a, i) > 0.0
      let bi = Array.getUnsafe(b, i) > 0.0
      if ai && bi { intersection := intersection.contents + 1 }
      if ai || bi { union := union.contents + 1 }
    }
    if union.contents == 0 {
      0.0
    } else {
      1.0 -. Float.fromInt(intersection.contents) /. Float.fromInt(union.contents)
    }
  }

  let compute = (metric: distanceMetric, a: array<float>, b: array<float>): float => {
    switch metric {
      | Euclidean => euclidean(a, b)
      | Cosine => cosine(a, b)
      | KLDivergence => Information.symmetricKL(a, b)
      | JensenShannon => Information.jensenShannon(a, b)
      | Jaccard => jaccard(a, b)
    }
  }
}

module Matrix = {
  open Types

  let distanceMatrix = (vectors: array<array<float>>, metric: distanceMetric): matrix => {
    let n = Array.length(vectors)
    let result = Array.make(~length=n, [])
    for i in 0 to n - 1 {
      let row = Array.make(~length=n, 0.0)
      for j in 0 to n - 1 {
        if i == j {
          Array.setUnsafe(row, j, 0.0)
        } else if j < i {
          // Use symmetry
          Array.setUnsafe(row, j, Array.getUnsafe(Array.getUnsafe(result, j), i))
        } else {
          let vi = Array.getUnsafe(vectors, i)
          let vj = Array.getUnsafe(vectors, j)
          Array.setUnsafe(row, j, Distance.compute(metric, vi, vj))
        }
      }
      Array.setUnsafe(result, i, row)
    }
    result
  }
}

module Normalization = {
  let normalize = (arr: array<float>): array<float> => {
    let total = Basic.sum(arr)
    if total > 0.0 {
      arr->Array.map(x => x /. total)
    } else {
      arr
    }
  }

  let minMaxNormalize = (arr: array<float>): array<float> => {
    switch (Basic.min(arr), Basic.max(arr)) {
      | (Some(minVal), Some(maxVal)) when maxVal > minVal =>
        let range = maxVal -. minVal
        arr->Array.map(x => (x -. minVal) /. range)
      | _ => arr
    }
  }

  let zScoreNormalize = (arr: array<float>): array<float> => {
    let m = Basic.mean(arr)
    let sd = Basic.standardDeviation(arr)
    if sd > 0.0 {
      arr->Array.map(x => (x -. m) /. sd)
    } else {
      arr
    }
  }
}
