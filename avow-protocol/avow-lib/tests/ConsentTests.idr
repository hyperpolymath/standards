-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>

||| Tests for STAMP.ABI.Consent
||| Positive tests construct valid ConsentChain values. Negative tests
||| are commented out with the compile-error they produce — each one
||| documents a spammer tactic blocked at the type level.
module STAMP.ABI.ConsentTests

import STAMP.ABI.Types
import STAMP.ABI.Consent

%default total

--------------------------------------------------------------------------------
-- Positive tests — must typecheck
--------------------------------------------------------------------------------

||| A valid double opt-in with proper timing
testValidDoubleOptIn : ConsentChain
testValidDoubleOptIn =
  DoubleOptIn
    1706630400000  -- initial request
    1706630500000  -- confirmation 100s later
    (IPv4 10 0 0 1)
    (MkToken "valid-token-abc-123")

||| A double opt-in at the boundary (just under 24h)
testBoundary24h : ConsentChain
testBoundary24h =
  DoubleOptIn
    1706630400000
    1706716799000       -- 86399 seconds later (< 24h)
    (IPv4 10 0 0 2)
    (MkToken "boundary-token")

||| Single opt-in (lower-trust variant)
testSingleOptIn : ConsentChain
testSingleOptIn =
  SingleOptIn
    1706630400000
    (IPv4 10 0 0 3)
    (MkToken "single-opt-token")

||| Imported example should still be constructible
testReusesExample : ConsentChain
testReusesExample = exampleValidConsent

--------------------------------------------------------------------------------
-- Negative tests — documented by commented-out code
--------------------------------------------------------------------------------

{-
-- Test N1: confirmation BEFORE request — must NOT typecheck.
testBadOrder : ConsentChain
testBadOrder =
  DoubleOptIn
    1706630500000    -- request at T+100
    1706630400000    -- "confirmation" at T+0
    (IPv4 10 0 0 4)
    (MkToken "bad-order")
-- Expected compile error:
--   Cannot satisfy constraint: 1706630400000 > 1706630500000
-}

{-
-- Test N2: confirmation 48h after request — must NOT typecheck.
testTooSlow : ConsentChain
testTooSlow =
  DoubleOptIn
    1706630400000
    1706803200000    -- 48h later, > 86400000ms window
    (IPv4 10 0 0 5)
    (MkToken "too-slow")
-- Expected compile error:
--   Cannot satisfy constraint: TimeDiff 1706630400000 1706803200000 86400000
-}

--------------------------------------------------------------------------------
-- Run marker (for "just test")
--------------------------------------------------------------------------------

||| Anchor function — if this compiles, all positive tests above typecheck.
export
runConsentTests : IO ()
runConsentTests = do
  putStrLn "ConsentTests: 4 positive cases compiled"
  putStrLn "  testValidDoubleOptIn"
  putStrLn "  testBoundary24h"
  putStrLn "  testSingleOptIn"
  putStrLn "  testReusesExample"
  putStrLn "ConsentTests: 2 negative cases documented (see comments)"
