// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

/**
 * Crawler Module Tests
 */

open Vitest

describe("Crawler.Config", () => {
  test("has sensible default timeout", () => {
    expect(Crawler.Config.defaultTimeout)->toBe(30000)
  })

  test("has sensible default retries", () => {
    expect(Crawler.Config.defaultRetries)->toBe(3)
  })

  test("has sensible default rate limit", () => {
    expect(Crawler.Config.defaultRateLimitMs)->toBe(1000)
  })

  test("makeDefaultHeaders creates valid headers", () => {
    let headers = Crawler.Config.makeDefaultHeaders()
    let userAgent = Dict.get(headers, "User-Agent")
    expect(Option.isSome(userAgent))->toBe(true)
  })
})

describe("Crawler.Request", () => {
  test("make creates request with defaults", () => {
    let req = Crawler.Request.make(~url="https://example.com", ())
    expect(req.url)->toBe("https://example.com")
    expect(req.method)->toEqual(Crawler.Types.GET)
    expect(req.timeout)->toBe(Crawler.Config.defaultTimeout)
    expect(req.retries)->toBe(Crawler.Config.defaultRetries)
  })

  test("make accepts custom options", () => {
    let req = Crawler.Request.make(
      ~url="https://example.com",
      ~method=Crawler.Types.POST,
      ~timeout=5000,
      ~retries=5,
      (),
    )
    expect(req.method)->toEqual(Crawler.Types.POST)
    expect(req.timeout)->toBe(5000)
    expect(req.retries)->toBe(5)
  })

  test("withHeader adds header", () => {
    let req = Crawler.Request.make(~url="https://example.com", ())
    let _ = Crawler.Request.withHeader(req, "X-Custom", "value")
    expect(Dict.get(req.headers, "X-Custom"))->toEqual(Some("value"))
  })

  test("methodToString converts correctly", () => {
    expect(Crawler.Request.methodToString(Crawler.Types.GET))->toBe("GET")
    expect(Crawler.Request.methodToString(Crawler.Types.POST))->toBe("POST")
    expect(Crawler.Request.methodToString(Crawler.Types.HEAD))->toBe("HEAD")
  })
})

describe("Crawler.RateLimiter", () => {
  test("make creates limiter with default delay", () => {
    let limiter = Crawler.RateLimiter.make()
    expect(limiter.delayMs)->toBe(Crawler.Config.defaultRateLimitMs)
  })

  test("make accepts custom delay", () => {
    let limiter = Crawler.RateLimiter.make(~delayMs=2000, ())
    expect(limiter.delayMs)->toBe(2000)
  })

  test("canProceed returns true for new limiter", () => {
    let limiter = Crawler.RateLimiter.make()
    expect(Crawler.RateLimiter.canProceed(limiter))->toBe(true)
  })

  test("recordRequest updates lastRequest", () => {
    let limiter = Crawler.RateLimiter.make()
    let before = limiter.lastRequest
    Crawler.RateLimiter.recordRequest(limiter)
    expect(limiter.lastRequest > before)->toBe(true)
  })
})

describe("Crawler.RetryPolicy", () => {
  describe("calculateDelay", () => {
    test("Constant returns same delay", () => {
      let strategy = Crawler.RetryPolicy.Constant(1000)
      expect(Crawler.RetryPolicy.calculateDelay(strategy, 1))->toBe(1000)
      expect(Crawler.RetryPolicy.calculateDelay(strategy, 3))->toBe(1000)
    })

    test("Linear increases linearly", () => {
      let strategy = Crawler.RetryPolicy.Linear(1000)
      expect(Crawler.RetryPolicy.calculateDelay(strategy, 1))->toBe(1000)
      expect(Crawler.RetryPolicy.calculateDelay(strategy, 2))->toBe(2000)
      expect(Crawler.RetryPolicy.calculateDelay(strategy, 3))->toBe(3000)
    })

    test("Exponential increases exponentially", () => {
      let strategy = Crawler.RetryPolicy.Exponential(1000, 2.0)
      expect(Crawler.RetryPolicy.calculateDelay(strategy, 1))->toBe(1000)
      expect(Crawler.RetryPolicy.calculateDelay(strategy, 2))->toBe(2000)
      expect(Crawler.RetryPolicy.calculateDelay(strategy, 3))->toBe(4000)
    })
  })

  describe("shouldRetry", () => {
    test("returns true when under max", () => {
      expect(Crawler.RetryPolicy.shouldRetry(1, 3))->toBe(true)
      expect(Crawler.RetryPolicy.shouldRetry(2, 3))->toBe(true)
    })

    test("returns false when at or over max", () => {
      expect(Crawler.RetryPolicy.shouldRetry(3, 3))->toBe(false)
      expect(Crawler.RetryPolicy.shouldRetry(4, 3))->toBe(false)
    })
  })
})

describe("Crawler.Parser", () => {
  test("selectorToString formats correctly", () => {
    expect(Crawler.Parser.selectorToString(Crawler.Parser.Css(".verse")))->toBe("css:.verse")
    expect(Crawler.Parser.selectorToString(Crawler.Parser.XPath("//div")))->toBe("xpath://div")
    expect(Crawler.Parser.selectorToString(Crawler.Parser.Regex("\\d+")))->toBe("regex:\\d+")
  })
})
