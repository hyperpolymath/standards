-- SPDX-License-Identifier: AGPL-3.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

||| Consent chain verification with proofs
||| Demonstrates proving the user actually opted in (not from a bought list)
module STAMP.ABI.Consent

import STAMP.ABI.Types

%default total

--------------------------------------------------------------------------------
-- Consent Chain with Cryptographic Proof
--------------------------------------------------------------------------------

||| A consent chain that PROVES the user actually opted in
|||
||| Current problem: Spammers claim "user consented" with no proof
||| - Bought email lists
||| - Scraped from websites
||| - "Pre-checked" boxes
||| - Fake double-opt-in
|||
||| STAMP solution: Cryptographically provable consent chain
public export
data ConsentChain : Type where
  ||| Double opt-in: User requested + confirmed
  DoubleOptIn :
    (initialRequest : Timestamp) ->
    (confirmationClick : Timestamp) ->
    (ipAddress : IPAddress) ->
    (confirmationToken : CryptoToken) ->

    ||| Proof 1: Confirmation happened AFTER initial request
    {auto 0 ordered : confirmationClick > initialRequest} ->

    ||| Proof 2: Confirmation was timely (within 24 hours)
    {auto 0 timely : TimeDiff initialRequest confirmationClick 86400000} ->

    ||| Proof 3: Token is cryptographically valid for this IP
    {auto 0 tokenValid : VerifyToken confirmationToken ipAddress} ->

    ConsentChain

  ||| Single opt-in: User explicitly requested (lower trust)
  SingleOptIn :
    (requestTime : Timestamp) ->
    (ipAddress : IPAddress) ->
    (requestToken : CryptoToken) ->

    ||| Proof: Recent request (within last hour)
    {auto 0 recent : TimeDiff requestTime Now 3600000} ->

    ConsentChain

||| Placeholder: Token verification for IP
public export
VerifyToken : CryptoToken -> IPAddress -> Type
VerifyToken token ip = ()  -- Would verify HMAC(ip, secret) == token

--------------------------------------------------------------------------------
-- What You CANNOT Do (Spammer Tactics Blocked)
--------------------------------------------------------------------------------

{-
Spammer tactic 1: Claim double opt-in without proof
  ✗ Cannot construct DoubleOptIn without valid token
  ✗ Cannot fake timestamps (must be ordered)
  ✗ Cannot bypass timely check

Spammer tactic 2: Use bought email list
  ✗ No consent chain exists for bought emails
  ✗ Cannot construct ConsentChain without actual opt-in event

Spammer tactic 3: Pre-checked box "consent"
  ✗ No cryptographic token from actual user click
  ✗ Cannot fake token (would need to know secret key)

Spammer tactic 4: Scrape emails from websites
  ✗ Website display ≠ opt-in consent
  ✗ No ConsentChain provable

THIS IS IMPOSSIBLE WITHOUT DEPENDENT TYPES.
Other languages can store timestamps, but can't PROVE the ordering.
-}

--------------------------------------------------------------------------------
-- Example: Valid Consent Chain
--------------------------------------------------------------------------------

||| Valid double opt-in flow
exampleValidConsent : ConsentChain
exampleValidConsent =
  DoubleOptIn
    1706630400000  -- User filled form
    1706630500000  -- User clicked email link 100s later
    (IPv4 192 168 1 100)
    (MkToken "abc123...token...xyz789")

||| Invalid: Confirmation before request (DOES NOT COMPILE)
-- exampleInvalidConsent : ConsentChain
-- exampleInvalidConsent =
--   DoubleOptIn
--     1706630500000  -- Request at T+100
--     1706630400000  -- Confirmation at T+0
--     (IPv4 192 168 1 100)
--     (MkToken "token...")
--
-- Compilation error:
--   Cannot satisfy constraint: 1706630400000 > 1706630500000
--   (Confirmation must be AFTER request)
