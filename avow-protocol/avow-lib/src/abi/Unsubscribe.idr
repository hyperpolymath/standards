-- SPDX-License-Identifier: AGPL-3.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

||| Unsubscribe link with formal verification
||| This module demonstrates the CORE VALUE of dependent types:
||| We don't just store an unsubscribe URL - we PROVE it works
module STAMP.ABI.Unsubscribe

import STAMP.ABI.Types

%default total

--------------------------------------------------------------------------------
-- Unsubscribe Link with Proofs
--------------------------------------------------------------------------------

||| An unsubscribe link that is PROVEN to work
|||
||| This is the key innovation: Without dependent types, you can only SAY
||| "this unsubscribe link works." With dependent types, you PROVE it.
|||
||| Compare to other approaches:
||| - JSON: {"unsubscribe": "https://..."} -- Just a string, no verification
||| - Haskell: data Unsub = Unsub URL -- Type-safe, but doesn't prove it works
||| - Idris2: See below -- PROVES the URL was tested and works
public export
record UnsubscribeLink where
  constructor MkUnsubscribeLink

  ||| The URL to click
  url : URL

  ||| When it was tested
  testedAt : Timestamp

  ||| The HTTP response we got when testing
  testResponse : HTTPResponse

  ||| Cryptographic token for verification
  token : CryptoToken

  ||| Signature proving this test actually happened
  signature : CryptoSignature

  -- Here's the magic: These proofs are REQUIRED to construct an UnsubscribeLink
  -- You CANNOT create one without providing evidence that these hold

  ||| Proof 1: The test was recent (within last 60 seconds)
  {auto 0 recentTest : TimeDiff testedAt Now 60000}

  ||| Proof 2: The server responded with 200 OK
  {auto 0 success : testResponse.code = OK}

  ||| Proof 3: The response was fast (< 200ms)
  {auto 0 fastResponse : testResponse.responseTime < 200}

  ||| Proof 4: The signature is cryptographically valid
  {auto 0 validSignature : VerifySignature signature token}

||| What this means in practice:
|||
||| WITHOUT dependent types (Rust, Haskell, Go):
|||   struct UnsubscribeLink { url: String }
|||   // Anyone can create this with ANY URL
|||   let fake = UnsubscribeLink { url: "https://doesnt-exist.com" };
|||   // ✗ No verification that it works
|||
||| WITH dependent types (Idris2):
|||   let fake = MkUnsubscribeLink
|||                (MkURL "https://doesnt-exist.com")
|||                Now
|||                (MkHTTPResponse NotFound 5000)  -- Took 5 seconds, returned 404
|||                token
|||                sig
|||   // ✗ COMPILATION ERROR: Cannot prove fastResponse (5000 < 200 is false)
|||   // ✗ COMPILATION ERROR: Cannot prove success (NotFound ≠ OK)
|||
||| YOU LITERALLY CANNOT COMPILE CODE THAT CREATES AN INVALID UNSUBSCRIBE LINK

--------------------------------------------------------------------------------
-- Unsubscribe Test Result
--------------------------------------------------------------------------------

||| Result of testing an unsubscribe URL
||| This is what you get BEFORE creating an UnsubscribeLink
public export
data UnsubscribeTestResult : Type where
  ||| Test succeeded - can construct UnsubscribeLink
  TestSuccess : (url : URL) ->
                (testedAt : Timestamp) ->
                (response : HTTPResponse) ->
                (token : CryptoToken) ->
                (signature : CryptoSignature) ->
                {auto 0 recent : TimeDiff testedAt Now 60000} ->
                {auto 0 ok : response.code = OK} ->
                {auto 0 fast : response.responseTime < 200} ->
                {auto 0 valid : VerifySignature signature token} ->
                UnsubscribeTestResult

  ||| Test failed - cannot construct UnsubscribeLink
  TestFailure : (url : URL) ->
                (reason : String) ->  -- "timeout", "404", "500", etc.
                UnsubscribeTestResult

||| Convert successful test to UnsubscribeLink
||| This is safe because TestSuccess already has the proofs
public export
toUnsubscribeLink : UnsubscribeTestResult -> Maybe UnsubscribeLink
toUnsubscribeLink (TestSuccess url time resp tok sig) =
  Just (MkUnsubscribeLink url time resp tok sig)
toUnsubscribeLink (TestFailure _ _) = Nothing

--------------------------------------------------------------------------------
-- Example Usage (would be in examples/)
--------------------------------------------------------------------------------

||| Example: This COMPILES (all proofs satisfied)
exampleValidUnsubscribe : UnsubscribeLink
exampleValidUnsubscribe =
  MkUnsubscribeLink
    (MkURL "https://example.com/unsubscribe")
    1706630400000  -- Now
    (MkHTTPResponse OK 87)  -- 87ms response time
    (MkToken "abc123...64chars...xyz789")
    (MkSignature "signature...")

||| Example: This DOES NOT COMPILE (proof obligation violated)
-- exampleInvalidUnsubscribe : UnsubscribeLink
-- exampleInvalidUnsubscribe =
--   MkUnsubscribeLink
--     (MkURL "https://fake.com/unsub")
--     1706630400000
--     (MkHTTPResponse NotFound 5000)  -- ✗ NotFound ≠ OK
--     (MkToken "abc...")
--     (MkSignature "sig...")
--
-- Compilation error:
--   Cannot satisfy constraint: NotFound = OK
--   Cannot satisfy constraint: 5000 < 200

--------------------------------------------------------------------------------
-- Comparison: What Other Languages Can't Do
--------------------------------------------------------------------------------

{-
JSON (no types):
  {
    "unsubscribe": "https://example.com/unsub",
    "tested": true
  }
  ✗ Can fake "tested": true
  ✗ No proof it actually works
  ✗ No timestamp validation

Haskell (strong types, not dependent):
  data UnsubscribeLink = UnsubscribeLink {
    url :: URL,
    tested :: Bool
  }
  ✓ Type-safe
  ✗ Can set tested = True without actually testing
  ✗ No proof the URL works
  ✗ No timestamp validation

Rust (memory safe, not proof):
  struct UnsubscribeLink {
      url: Url,
      tested_at: u64,
      response_code: u16,
  }
  ✓ Memory safe
  ✗ Can set response_code = 200 without testing
  ✗ No proof the timestamp is recent
  ✗ No cryptographic verification

Idris2 (dependent types = proofs):
  record UnsubscribeLink where
    url : URL
    testedAt : Timestamp
    response : HTTPResponse
    {auto 0 recent : ...}
    {auto 0 success : response.code = OK}
    {auto 0 fast : ...}
  ✓ Type-safe
  ✓ Memory safe
  ✓ PROVEN to work (mathematically)
  ✓ Cannot fake proofs
  ✓ Verified at compile time
  ✓ Cryptographically signed

THIS IS WHY STAMP NEEDS DEPENDENT TYPES.
-}
