// SPDX-License-Identifier: PMPL-1.0-or-later
// AXEL Protocol - Policy Schema Validation Tests

import { assertEquals, assertThrows } from "jsr:@std/assert@1";
import Ajv from "npm:ajv@8.17.1";
import addFormats from "npm:ajv-formats@3.0.1";
import { join, dirname, fromFileUrl } from "jsr:@std/path@1";

const ROOT = join(dirname(fromFileUrl(import.meta.url)), "..");
const schemaPath = join(ROOT, "schemas", "axel-policy.schema.json");

const schema = JSON.parse(await Deno.readTextFile(schemaPath));
const ajv = new Ajv({ allErrors: true, strict: false, validateSchema: false });
addFormats(ajv);
const validate = ajv.compile(schema);

function isValid(policy: unknown): boolean {
  return validate(policy) as boolean;
}

// --- Valid policies ---

Deno.test("valid minimal policy", () => {
  const policy = {
    version: "AXEL1",
    id: "20260212T1200Z",
    scope: { hostnames: ["example.com"] },
    content: { category: "adult", min_age: 18 },
    enforcement: { profiles: ["AXEL-O"], proof_required: true },
    cache: { max_age_seconds: 86400, stale_if_error_seconds: 604800 },
  };
  assertEquals(isValid(policy), true);
});

Deno.test("valid full policy with all optional fields", () => {
  const policy = {
    version: "AXEL1",
    id: "20260212T1430Z",
    scope: { hostnames: ["explicit.example.com", "cdn-explicit.example.com"] },
    content: { category: "adult", min_age: 18 },
    enforcement: {
      profiles: ["AXEL-O", "AXEL-N"],
      proof_required: true,
      browser_flow: { gate_url: "https://explicit.example.com/verify" },
      api_flow: {
        problem_type: "https://axel-protocol.org/problems/proof-required",
      },
    },
    isolation: {
      level: "L1-AUDITED",
      prefixes: {
        ipv6: ["2001:db8:abcd::/48"],
        ipv4: ["198.51.100.0/24"],
      },
      cdn_pool: "explicit-only-pool-us-east",
    },
    verifiers: [
      {
        name: "ExampleVerify",
        url: "https://verify.example.com",
        methods: ["zkp", "gov_id"],
      },
    ],
    auditing: {
      statement_url:
        "https://auditor.example.org/statements/example-com-2026.json",
      policy_hash_sha256:
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
    },
    cache: { max_age_seconds: 86400, stale_if_error_seconds: 604800 },
    extensions: {},
  };
  assertEquals(isValid(policy), true);
});

Deno.test("valid policy with proof_required false", () => {
  const policy = {
    version: "AXEL1",
    id: "test-1",
    scope: { hostnames: ["safe.example.com"] },
    content: { category: "mature", min_age: 0 },
    enforcement: { profiles: ["AXEL-O"], proof_required: false },
    cache: { max_age_seconds: 3600, stale_if_error_seconds: 7200 },
  };
  assertEquals(isValid(policy), true);
});

// --- Invalid policies ---

Deno.test("reject: missing version", () => {
  const policy = {
    id: "test-1",
    scope: { hostnames: ["example.com"] },
    content: { category: "adult", min_age: 18 },
    enforcement: { profiles: ["AXEL-O"], proof_required: true },
    cache: { max_age_seconds: 86400, stale_if_error_seconds: 604800 },
  };
  assertEquals(isValid(policy), false);
});

Deno.test("reject: wrong version", () => {
  const policy = {
    version: "AXEL2",
    id: "test-1",
    scope: { hostnames: ["example.com"] },
    content: { category: "adult", min_age: 18 },
    enforcement: { profiles: ["AXEL-O"], proof_required: true },
    cache: { max_age_seconds: 86400, stale_if_error_seconds: 604800 },
  };
  assertEquals(isValid(policy), false);
});

Deno.test("reject: empty id", () => {
  const policy = {
    version: "AXEL1",
    id: "",
    scope: { hostnames: ["example.com"] },
    content: { category: "adult", min_age: 18 },
    enforcement: { profiles: ["AXEL-O"], proof_required: true },
    cache: { max_age_seconds: 86400, stale_if_error_seconds: 604800 },
  };
  assertEquals(isValid(policy), false);
});

Deno.test("reject: missing scope", () => {
  const policy = {
    version: "AXEL1",
    id: "test-1",
    content: { category: "adult", min_age: 18 },
    enforcement: { profiles: ["AXEL-O"], proof_required: true },
    cache: { max_age_seconds: 86400, stale_if_error_seconds: 604800 },
  };
  assertEquals(isValid(policy), false);
});

Deno.test("reject: empty hostnames array", () => {
  const policy = {
    version: "AXEL1",
    id: "test-1",
    scope: { hostnames: [] },
    content: { category: "adult", min_age: 18 },
    enforcement: { profiles: ["AXEL-O"], proof_required: true },
    cache: { max_age_seconds: 86400, stale_if_error_seconds: 604800 },
  };
  assertEquals(isValid(policy), false);
});

Deno.test("reject: invalid content category", () => {
  const policy = {
    version: "AXEL1",
    id: "test-1",
    scope: { hostnames: ["example.com"] },
    content: { category: "invalid-category", min_age: 18 },
    enforcement: { profiles: ["AXEL-O"], proof_required: true },
    cache: { max_age_seconds: 86400, stale_if_error_seconds: 604800 },
  };
  assertEquals(isValid(policy), false);
});

Deno.test("reject: negative min_age", () => {
  const policy = {
    version: "AXEL1",
    id: "test-1",
    scope: { hostnames: ["example.com"] },
    content: { category: "adult", min_age: -1 },
    enforcement: { profiles: ["AXEL-O"], proof_required: true },
    cache: { max_age_seconds: 86400, stale_if_error_seconds: 604800 },
  };
  assertEquals(isValid(policy), false);
});

Deno.test("reject: enforcement profiles missing AXEL-O", () => {
  const policy = {
    version: "AXEL1",
    id: "test-1",
    scope: { hostnames: ["example.com"] },
    content: { category: "adult", min_age: 18 },
    enforcement: { profiles: ["AXEL-N"], proof_required: true },
    cache: { max_age_seconds: 86400, stale_if_error_seconds: 604800 },
  };
  assertEquals(isValid(policy), false);
});

Deno.test("reject: missing cache", () => {
  const policy = {
    version: "AXEL1",
    id: "test-1",
    scope: { hostnames: ["example.com"] },
    content: { category: "adult", min_age: 18 },
    enforcement: { profiles: ["AXEL-O"], proof_required: true },
  };
  assertEquals(isValid(policy), false);
});

Deno.test("reject: cache max_age_seconds zero", () => {
  const policy = {
    version: "AXEL1",
    id: "test-1",
    scope: { hostnames: ["example.com"] },
    content: { category: "adult", min_age: 18 },
    enforcement: { profiles: ["AXEL-O"], proof_required: true },
    cache: { max_age_seconds: 0, stale_if_error_seconds: 604800 },
  };
  assertEquals(isValid(policy), false);
});

Deno.test("reject: invalid isolation level", () => {
  const policy = {
    version: "AXEL1",
    id: "test-1",
    scope: { hostnames: ["example.com"] },
    content: { category: "adult", min_age: 18 },
    enforcement: { profiles: ["AXEL-O"], proof_required: true },
    isolation: { level: "L2" },
    cache: { max_age_seconds: 86400, stale_if_error_seconds: 604800 },
  };
  assertEquals(isValid(policy), false);
});

Deno.test("reject: invalid auditing hash (not hex)", () => {
  const policy = {
    version: "AXEL1",
    id: "test-1",
    scope: { hostnames: ["example.com"] },
    content: { category: "adult", min_age: 18 },
    enforcement: { profiles: ["AXEL-O"], proof_required: true },
    auditing: { policy_hash_sha256: "not-a-valid-hash" },
    cache: { max_age_seconds: 86400, stale_if_error_seconds: 604800 },
  };
  assertEquals(isValid(policy), false);
});

Deno.test("reject: additional top-level properties", () => {
  const policy = {
    version: "AXEL1",
    id: "test-1",
    scope: { hostnames: ["example.com"] },
    content: { category: "adult", min_age: 18 },
    enforcement: { profiles: ["AXEL-O"], proof_required: true },
    cache: { max_age_seconds: 86400, stale_if_error_seconds: 604800 },
    unknown_field: "should fail",
  };
  assertEquals(isValid(policy), false);
});

// --- Bundled example files ---

Deno.test("bundled policy: public/.well-known/axel-policy", async () => {
  const text = await Deno.readTextFile(
    join(ROOT, "public", ".well-known", "axel-policy")
  );
  const policy = JSON.parse(text);
  assertEquals(isValid(policy), true);
});

Deno.test(
  "bundled policy: public/.well-known/axel-policy.example-adult.json",
  async () => {
    const text = await Deno.readTextFile(
      join(
        ROOT,
        "public",
        ".well-known",
        "axel-policy.example-adult.json"
      )
    );
    const policy = JSON.parse(text);
    assertEquals(isValid(policy), true);
  }
);
