-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>

||| Tests for STAMP.ABI.Unsubscribe — GDPR-critical unsubscribe-link checks
module STAMP.ABI.UnsubscribeTests

import STAMP.ABI.Types
import STAMP.ABI.Unsubscribe

%default total

--------------------------------------------------------------------------------
-- Positive tests
--------------------------------------------------------------------------------

||| Imported library example should construct
testReusesExample : UnsubscribeLink
testReusesExample = exampleValidUnsubscribe

||| After a successful test, the link is extractable
testToLinkSome : Maybe UnsubscribeLink -> Bool
testToLinkSome (Just _) = True
testToLinkSome Nothing = False

--------------------------------------------------------------------------------
-- Negative tests — documented
--------------------------------------------------------------------------------

{-
-- Test N1: link without one-click support — should fail GDPR compliance.
-- (Concrete error depends on Unsubscribe.idr's invariants; fill in once
-- the module's public API stabilises.)
-}

--------------------------------------------------------------------------------
-- Run marker
--------------------------------------------------------------------------------

export
runUnsubscribeTests : IO ()
runUnsubscribeTests = do
  putStrLn "UnsubscribeTests: imported exampleValidUnsubscribe"
  putStrLn "UnsubscribeTests: negative cases pending API finalisation"
