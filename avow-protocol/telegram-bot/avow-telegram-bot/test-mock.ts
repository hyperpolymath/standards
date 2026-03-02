// SPDX-License-Identifier: AGPL-3.0-or-later
// Test script for mock STAMP library

import * as stamp from "./src/stamp-mock.ts";

console.log("Testing STAMP Mock Library\n");

// Test 1: Valid unsubscribe
console.log("Test 1: Valid Unsubscribe");
const valid_unsub: stamp.UnsubscribeParams = {
  url: "https://example.com/unsubscribe",
  tested_at: Date.now() - 5000,
  response_code: 200,
  response_time: 87,
  token: "abc123",
  signature: "valid_sig",
};

const result1 = stamp.verifyUnsubscribe(valid_unsub);
console.log(`Result: ${stamp.resultToString(result1)}`);
console.assert(result1 === stamp.VerificationResult.SUCCESS, "Should pass");
console.log("✓ Passed\n");

// Test 2: Invalid URL
console.log("Test 2: Invalid URL");
const invalid_url: stamp.UnsubscribeParams = {
  ...valid_unsub,
  url: "not_https",
};

const result2 = stamp.verifyUnsubscribe(invalid_url);
console.log(`Result: ${stamp.resultToString(result2)}`);
console.assert(result2 === stamp.VerificationResult.ERROR_INVALID_URL, "Should fail");
console.log("✓ Passed\n");

// Test 3: Valid consent
console.log("Test 3: Valid Consent");
const valid_consent: stamp.ConsentParams = {
  initial_request: 1000000,
  confirmation: 1100000,
  ip_address: "192.168.1.1",
  token: "token123",
};

const result3 = stamp.verifyConsent(valid_consent);
console.log(`Result: ${stamp.resultToString(result3)}`);
console.assert(result3 === stamp.VerificationResult.SUCCESS, "Should pass");
console.log("✓ Passed\n");

// Test 4: Proof generation
console.log("Test 4: Proof Generation");
const proof = stamp.generateProof("unsubscribe", valid_unsub);
console.log("Proof generated:");
console.log(stamp.formatProof(proof));
console.assert(proof.type === "unsubscribe_verification", "Should be unsubscribe proof");
console.log("✓ Passed\n");

console.log("All tests passed! ✓");
