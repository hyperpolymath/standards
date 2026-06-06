#!/usr/bin/env -S deno run --allow-read --allow-ffi
// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// Deno example using libstamp via Deno FFI (port of the former Python
// ctypes example; Python is banned estate-wide).

import { dirname, fromFileUrl, join } from "jsr:@std/path@^1";

// ============================================================================
// Load Library
// ============================================================================

function loadLibstamp() {
  let libName;
  if (Deno.build.os === "darwin") libName = "libstamp.dylib";
  else if (Deno.build.os === "windows") libName = "stamp.dll";
  else libName = "libstamp.so";

  const here = dirname(fromFileUrl(import.meta.url));
  const searchPaths = [
    join(here, "..", "..", "ffi", "zig", "zig-out", "lib", libName),
    join("/usr/local/lib", libName),
    join("/usr/lib", libName),
  ];

  for (const path of searchPaths) {
    try {
      Deno.statSync(path);
      return Deno.dlopen(path, SYMBOLS);
    } catch {
      // try next
    }
  }
  throw new Error(
    `Could not find ${libName}. Build with: cd ffi/zig && zig build`,
  );
}

const SYMBOLS = {
  stamp_verify_unsubscribe: { parameters: ["pointer"], result: "i32" },
  stamp_verify_consent: { parameters: ["pointer"], result: "i32" },
  stamp_verify_rate_limit: { parameters: ["pointer"], result: "i32" },
  stamp_generate_proof: { parameters: ["pointer"], result: "pointer" },
  stamp_free_proof: { parameters: ["pointer"], result: "void" },
  stamp_version: { parameters: [], result: "pointer" },
};

const libstamp = loadLibstamp();

// ============================================================================
// Result codes
// ============================================================================

const StampResult = {
  SUCCESS: 0,
  ERROR_INVALID_URL: -1,
  ERROR_TIMEOUT: -2,
  ERROR_INVALID_RESPONSE: -3,
  ERROR_INVALID_SIGNATURE: -4,
  ERROR_RATE_LIMIT_EXCEEDED: -5,
  ERROR_CONSENT_INVALID: -6,
  ERROR_NULL_POINTER: -7,
  ERROR_INTERNAL: -99,
  toString(code) {
    return ({
      0: "✓ SUCCESS",
      [-1]: "✗ INVALID_URL",
      [-2]: "✗ TIMEOUT",
      [-3]: "✗ INVALID_RESPONSE",
      [-4]: "✗ INVALID_SIGNATURE",
      [-5]: "✗ RATE_LIMIT_EXCEEDED",
      [-6]: "✗ CONSENT_INVALID",
      [-7]: "✗ NULL_POINTER",
      [-99]: "✗ INTERNAL_ERROR",
    })[code] ?? "✗ UNKNOWN_ERROR";
  },
};

// ============================================================================
// C struct marshalling (x86-64 SysV natural alignment)
// ============================================================================

const enc = new TextEncoder();

function cstring(s) {
  const bytes = enc.encode(s);
  const buf = new Uint8Array(bytes.length + 1);
  buf.set(bytes);
  return buf; // NUL-terminated; keep a reference so it isn't GC'd
}

function ptrBig(buf) {
  return BigInt(Deno.UnsafePointer.value(Deno.UnsafePointer.of(buf)));
}

// StampUnsubscribeParams { char* url; u64 tested_at; u16 response_code;
//   u32 response_time; char* token; char* signature; }  size 40
function unsubscribeParams({ url, testedAt, responseCode, responseTime, token, signature }) {
  const urlB = cstring(url), tokB = cstring(token), sigB = cstring(signature);
  const buf = new ArrayBuffer(40);
  const dv = new DataView(buf);
  dv.setBigUint64(0, ptrBig(urlB), true);
  dv.setBigUint64(8, BigInt(testedAt), true);
  dv.setUint16(16, responseCode, true);
  dv.setUint32(20, responseTime, true);
  dv.setBigUint64(24, ptrBig(tokB), true);
  dv.setBigUint64(32, ptrBig(sigB), true);
  return { buf: new Uint8Array(buf), keep: [urlB, tokB, sigB] };
}

// StampConsentParams { u64 initial_request; u64 confirmation;
//   char* ip_address; char* token; }  size 32
function consentParams({ initialRequest, confirmation, ipAddress, token }) {
  const ipB = cstring(ipAddress), tokB = cstring(token);
  const buf = new ArrayBuffer(32);
  const dv = new DataView(buf);
  dv.setBigUint64(0, BigInt(initialRequest), true);
  dv.setBigUint64(8, BigInt(confirmation), true);
  dv.setBigUint64(16, ptrBig(ipB), true);
  dv.setBigUint64(24, ptrBig(tokB), true);
  return { buf: new Uint8Array(buf), keep: [ipB, tokB] };
}

// StampRateLimitParams { char* sender_id; u64 account_created;
//   u32 messages_today; u32 daily_limit; }  size 24
function rateLimitParams({ senderId, accountCreated, messagesToday, dailyLimit }) {
  const idB = cstring(senderId);
  const buf = new ArrayBuffer(24);
  const dv = new DataView(buf);
  dv.setBigUint64(0, ptrBig(idB), true);
  dv.setBigUint64(8, BigInt(accountCreated), true);
  dv.setUint32(16, messagesToday, true);
  dv.setUint32(20, dailyLimit, true);
  return { buf: new Uint8Array(buf), keep: [idB] };
}

// StampProof { char* data; size_t length; char* signature; }  size 24
function readProof(ptr) {
  if (ptr === null) return null;
  const view = new Deno.UnsafePointerView(ptr);
  const dataPtrVal = view.getBigUint64(0);
  const dataPtr = Deno.UnsafePointer.create(dataPtrVal);
  return new Deno.UnsafePointerView(dataPtr).getCString();
}

// ============================================================================
// Wrappers
// ============================================================================

function getCurrentTimestamp() {
  return Date.now();
}

function verifyUnsubscribe(args) {
  const p = unsubscribeParams(args);
  return libstamp.symbols.stamp_verify_unsubscribe(Deno.UnsafePointer.of(p.buf));
}

function verifyConsent(args) {
  const p = consentParams(args);
  return libstamp.symbols.stamp_verify_consent(Deno.UnsafePointer.of(p.buf));
}

function verifyRateLimit(args) {
  const p = rateLimitParams(args);
  return libstamp.symbols.stamp_verify_rate_limit(Deno.UnsafePointer.of(p.buf));
}

function generateProof(args) {
  const p = unsubscribeParams(args);
  const proofPtr = libstamp.symbols.stamp_generate_proof(Deno.UnsafePointer.of(p.buf));
  if (proofPtr === null) return null;
  const data = readProof(proofPtr);
  libstamp.symbols.stamp_free_proof(proofPtr);
  return data;
}

function getVersion() {
  const ptr = libstamp.symbols.stamp_version();
  return new Deno.UnsafePointerView(ptr).getCString();
}

// ============================================================================
// Examples
// ============================================================================

function assert(cond, msg) {
  if (!cond) throw new Error(`Assertion failed: ${msg}`);
}

function main() {
  console.log("=== STAMP Deno Example ===\n");
  console.log(`Version: ${getVersion()}\n`);

  const now = getCurrentTimestamp();

  console.log("Example 1: Valid Unsubscribe Link");
  console.log("==================================");
  let result = verifyUnsubscribe({
    url: "https://example.com/unsubscribe",
    testedAt: now - 5000,
    responseCode: 200,
    responseTime: 87,
    token: "EXAMPLE-UNSUBSCRIBE-TOKEN",
    signature: "EXAMPLE-SIGNATURE",
  });
  console.log(`Result: ${StampResult.toString(result)}`);
  assert(result === StampResult.SUCCESS, "example 1");
  console.log();

  const proof = generateProof({
    url: "https://example.com/unsubscribe",
    testedAt: now - 5000,
    responseCode: 200,
    responseTime: 87,
    token: "EXAMPLE-UNSUBSCRIBE-TOKEN",
    signature: "EXAMPLE-SIGNATURE",
  });
  if (proof) {
    console.log("Proof generated:");
    console.log(proof);
    console.log();
  }

  console.log("Example 2: Invalid URL");
  console.log("======================");
  result = verifyUnsubscribe({
    url: "not_a_url",
    testedAt: now,
    responseCode: 200,
    responseTime: 87,
    token: "EXAMPLE-TOKEN",
    signature: "EXAMPLE-SIG",
  });
  console.log(`Result: ${StampResult.toString(result)}`);
  assert(result === StampResult.ERROR_INVALID_URL, "example 2");
  console.log();

  console.log("Example 3: Valid Consent Chain");
  console.log("===============================");
  const initial = 1706630400000;
  const confirmation = initial + 100000;
  result = verifyConsent({
    initialRequest: initial,
    confirmation,
    ipAddress: "192.168.1.100",
    token: "EXAMPLE-CONSENT-TOKEN",
  });
  console.log(`Initial: ${initial}`);
  console.log(`Confirmation: ${confirmation} (+${Math.floor((confirmation - initial) / 1000)} seconds)`);
  console.log(`Result: ${StampResult.toString(result)}`);
  assert(result === StampResult.SUCCESS, "example 3");
  console.log();

  console.log("Example 4: Invalid Consent Order");
  console.log("=================================");
  result = verifyConsent({
    initialRequest: confirmation,
    confirmation: initial,
    ipAddress: "192.168.1.100",
    token: "EXAMPLE-CONSENT-TOKEN",
  });
  console.log(`Result: ${StampResult.toString(result)}`);
  assert(result === StampResult.ERROR_CONSENT_INVALID, "example 4");
  console.log();

  console.log("Example 5: Rate Limit Check");
  console.log("============================");
  const accountAge = now - (45 * 24 * 60 * 60 * 1000);
  result = verifyRateLimit({
    senderId: "user_12345",
    accountCreated: accountAge,
    messagesToday: 150,
    dailyLimit: 10000,
  });
  const ageDays = Math.floor((now - accountAge) / (24 * 60 * 60 * 1000));
  console.log(`Account age: ${ageDays} days`);
  console.log("Messages: 150/10000");
  console.log(`Result: ${StampResult.toString(result)}`);
  assert(result === StampResult.SUCCESS, "example 5");
  console.log();

  console.log("Example 6: Rate Limit Exceeded");
  console.log("===============================");
  result = verifyRateLimit({
    senderId: "spammer",
    accountCreated: accountAge,
    messagesToday: 10000,
    dailyLimit: 10000,
  });
  console.log("Messages: 10000/10000");
  console.log(`Result: ${StampResult.toString(result)}`);
  assert(result === StampResult.ERROR_RATE_LIMIT_EXCEEDED, "example 6");
  console.log();

  console.log("All examples passed! ✓");
}

main();
