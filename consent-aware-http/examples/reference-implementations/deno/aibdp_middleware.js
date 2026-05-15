// SPDX-License-Identifier: MIT OR GPL-3.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
//
// AIBDP + HTTP 430 middleware for Deno (port of the former Python/Flask
// reference implementation; Python is banned estate-wide).
//
// Implements AI Boundary Declaration Protocol (AIBDP) enforcement with
// HTTP 430 (Consent Required) responses for `Deno.serve` handlers.

// AI User-Agent patterns for detection
const AI_USER_AGENTS = [
  /GPTBot/i,
  /ChatGPT-User/i,
  /Claude-Web/i,
  /anthropic-ai/i,
  /Google-Extended/i,
  /CCBot/i,
  /Googlebot/i,
  /Bingbot/i,
  /Slurp/i,
  /DuckDuckBot/i,
  /Baiduspider/i,
  /YandexBot/i,
  /PerplexityBot/i,
  /Diffbot/i,
];

// AIBDP manifest loader with TTL cache.
export class AIBDPManifest {
  constructor(manifestPath, cacheDurationSeconds = 3600) {
    this.manifestPath = manifestPath;
    this.cacheDuration = cacheDurationSeconds;
    this._manifest = null;
    this._loadTime = null;
  }

  load() {
    const now = Date.now();
    if (this._manifest && this._loadTime !== null) {
      if ((now - this._loadTime) / 1000 < this.cacheDuration) {
        return this._manifest;
      }
    }
    try {
      const text = Deno.readTextFileSync(this.manifestPath);
      this._manifest = JSON.parse(text);
      this._loadTime = now;
      return this._manifest;
    } catch (e) {
      if (e instanceof Deno.errors.NotFound) return null;
      console.log(`Warning: Failed to load AIBDP manifest: ${e}`);
      return null;
    }
  }

  get manifest() {
    return this.load();
  }
}

export function isAiUserAgent(userAgent) {
  if (!userAgent) return false;
  return AI_USER_AGENTS.some((p) => p.test(userAgent));
}

// `headers` may be a Headers instance or a plain object.
function headerGet(headers, name) {
  if (headers instanceof Headers) return headers.get(name);
  if (name in headers) return headers[name];
  // case-insensitive fallback for plain objects
  const lower = name.toLowerCase();
  for (const k of Object.keys(headers)) {
    if (k.toLowerCase() === lower) return headers[k];
  }
  return null;
}

function headerHas(headers, name) {
  return headerGet(headers, name) !== null && headerGet(headers, name) !== undefined;
}

export function extractAiPurpose(headers) {
  const explicit = headerGet(headers, "AI-Purpose");
  if (explicit) return String(explicit).toLowerCase();

  const ua = headerGet(headers, "User-Agent") || "";
  if (/GPTBot/i.test(ua)) return "training";
  if (/Claude-Web/i.test(ua)) return "indexing";
  if (/Google-Extended/i.test(ua)) return "training";
  if (/Googlebot/i.test(ua)) return "indexing";
  return "unknown";
}

export function pathMatches(requestPath, pattern) {
  if (pattern === "all") return true;
  // Match the Python sequential-replace glob translation exactly.
  const regexPattern = pattern
    .replaceAll(".", "\\.")
    .replaceAll("**", ".*")
    .replaceAll("*", "[^/]*")
    .replaceAll("?", ".");
  return new RegExp(`^${regexPattern}$`).test(requestPath);
}

export function getApplicablePolicy(manifest, purpose, requestPath) {
  if (!manifest || !("policies" in manifest)) return null;
  const policy = manifest.policies[purpose];
  if (!policy) return null;

  const scope = policy.scope;
  if (scope) {
    if (Array.isArray(scope)) {
      if (!scope.some((pat) => pathMatches(requestPath, pat))) return null;
    }
    // scope: "all" always matches
  }

  for (const exception of policy.exceptions ?? []) {
    if (pathMatches(requestPath, exception.path)) return exception;
  }

  return policy;
}

export function checkPolicyConditions(policy, headers) {
  if (policy.status !== "conditional") return [true, []];
  const conditions = policy.conditions ?? [];
  if (conditions.length === 0) return [true, []];

  const missing = [];
  if (!headerHas(headers, "AI-Consent-Reviewed")) {
    missing.push("AI-Consent-Reviewed header required");
  }
  if (!headerHas(headers, "AI-Consent-Conditions")) {
    missing.push("AI-Consent-Conditions header required");
  }
  return [missing.length === 0, missing];
}

export function create430Response(manifest, policy, purpose, extra = {}) {
  const manifestUri = manifest.canonical_uri ?? "/.well-known/aibdp.json";
  const body = {
    error: "AI usage boundaries declared in AIBDP manifest not satisfied",
    manifest: manifestUri,
    violated_policy: purpose,
    policy_status: policy.status,
    required_conditions: policy.conditions ?? [],
    rationale: policy.rationale ?? "No additional information provided",
    contact: manifest.contact ?? null,
    ...extra,
  };
  return new Response(JSON.stringify(body), {
    status: 430,
    headers: {
      "Content-Type": "application/json",
      "Link": `<${manifestUri}>; rel="blocked-by-consent"`,
      "Retry-After": "86400",
    },
  });
}

// Flask-style middleware: wrap a `Deno.serve` handler.
export class AIBDPMiddleware {
  constructor({
    manifestPath = ".well-known/aibdp.json",
    enforceForAll = false,
    onViolation = null,
  } = {}) {
    this.manifestLoader = new AIBDPManifest(manifestPath);
    this.enforceForAll = enforceForAll;
    this.onViolation = onViolation;
  }

  // Returns a Response (HTTP 430) when a violation is detected, else null.
  beforeRequest(req) {
    try {
      const manifest = this.manifestLoader.manifest;
      if (!manifest) return null;

      const userAgent = req.headers.get("User-Agent") || "";
      const isAi = this.enforceForAll || isAiUserAgent(userAgent);
      if (!isAi) return null;

      const purpose = extractAiPurpose(req.headers);
      const path = new URL(req.url).pathname;
      const policy = getApplicablePolicy(manifest, purpose, path);
      if (!policy) return null;

      if (policy.status === "refused") {
        this.onViolation?.(req, policy, purpose);
        return create430Response(manifest, policy, purpose);
      }

      if (policy.status === "conditional") {
        const [satisfied, missing] = checkPolicyConditions(policy, req.headers);
        if (!satisfied) {
          this.onViolation?.(req, policy, purpose);
          return create430Response(manifest, policy, purpose, {
            missing_conditions: missing,
          });
        }
      }
      return null;
    } catch (e) {
      console.log(`AIBDP middleware error: ${e}`);
      return null; // fail open — don't break the site
    }
  }

  wrap(handler) {
    return (req, info) => {
      const blocked = this.beforeRequest(req);
      if (blocked) return blocked;
      return handler(req, info);
    };
  }
}

export function serveManifest(manifestPath = ".well-known/aibdp.json") {
  const loader = new AIBDPManifest(manifestPath);
  return () => {
    const manifest = loader.manifest;
    if (!manifest) {
      return new Response(JSON.stringify({ error: "Manifest not found" }), {
        status: 404,
        headers: { "Content-Type": "application/json" },
      });
    }
    return new Response(JSON.stringify(manifest), {
      headers: {
        "Content-Type": "application/aibdp+json",
        "Cache-Control": "public, max-age=3600",
        "Access-Control-Allow-Origin": "*",
      },
    });
  };
}

export function aibdpRequired(
  handler,
  { manifestPath = ".well-known/aibdp.json", purpose = "unknown" } = {},
) {
  const loader = new AIBDPManifest(manifestPath);
  return (req, info) => {
    const manifest = loader.manifest;
    if (!manifest) return handler(req, info);

    const userAgent = req.headers.get("User-Agent") || "";
    if (!isAiUserAgent(userAgent)) return handler(req, info);

    const path = new URL(req.url).pathname;
    const policy = getApplicablePolicy(manifest, purpose, path);
    if (!policy) return handler(req, info);

    if (policy.status === "refused") {
      return create430Response(manifest, policy, purpose);
    }
    if (policy.status === "conditional") {
      const [satisfied] = checkPolicyConditions(policy, req.headers);
      if (!satisfied) return create430Response(manifest, policy, purpose);
    }
    return handler(req, info);
  };
}
