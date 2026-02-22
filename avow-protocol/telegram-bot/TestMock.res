// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
// Test script for mock STAMP library

Console.log("Testing STAMP Mock Library\n")

// Test 1: Valid unsubscribe
Console.log("Test 1: Valid Unsubscribe")
let validUnsub: StampMock.unsubscribeParams = {
  url: "https://example.com/unsubscribe",
  tested_at: Date.now() -. 5000.0,
  response_code: 200,
  response_time: 87,
  token: "abc123",
  signature: "valid_sig",
}

let result1 = StampMock.verifyUnsubscribe(validUnsub)
Console.log(`Result: ${StampMock.resultToString(result1)}`)
Console.assert_(result1 == Success, ~message="Should pass")
Console.log("Passed\n")

// Test 2: Invalid URL
Console.log("Test 2: Invalid URL")
let invalidUrl: StampMock.unsubscribeParams = {
  ...validUnsub,
  url: "not_https",
}

let result2 = StampMock.verifyUnsubscribe(invalidUrl)
Console.log(`Result: ${StampMock.resultToString(result2)}`)
Console.assert_(result2 == ErrorInvalidUrl, ~message="Should fail")
Console.log("Passed\n")

// Test 3: Valid consent
Console.log("Test 3: Valid Consent")
let validConsent: StampMock.consentParams = {
  initial_request: 1000000.0,
  confirmation: 1100000.0,
  ip_address: "192.168.1.1",
  token: "token123",
}

let result3 = StampMock.verifyConsent(validConsent)
Console.log(`Result: ${StampMock.resultToString(result3)}`)
Console.assert_(result3 == Success, ~message="Should pass")
Console.log("Passed\n")

// Test 4: Proof generation
Console.log("Test 4: Proof Generation")
let proof = StampMock.generateProof(#unsubscribe, StampMock.unsubscribeParamsToJson(validUnsub))
Console.log("Proof generated:")
Console.log(StampMock.formatProof(proof))
Console.assert_(proof.type_ == "unsubscribe_verification", ~message="Should be unsubscribe proof")
Console.log("Passed\n")

Console.log("All tests passed!")
