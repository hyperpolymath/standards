-- SPDX-License-Identifier: AGPL-3.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

||| Core types for STAMP protocol verification
||| This module defines the foundational types with dependent type proofs
module STAMP.ABI.Types

import Data.Nat
import Data.String
import Data.List

%default total

--------------------------------------------------------------------------------
-- Primitive Types
--------------------------------------------------------------------------------

||| A URL with proven format validity
public export
data URL : Type where
  MkURL : (urlString : String) ->
          {auto 0 validFormat : IsValidURLFormat urlString} ->
          URL

||| An IP address
public export
data IPAddress : Type where
  IPv4 : (a : Nat) -> (b : Nat) -> (c : Nat) -> (d : Nat) ->
         {auto 0 valid : (a < 256, b < 256, c < 256, d < 256)} ->
         IPAddress
  IPv6 : String -> IPAddress  -- Simplified for now

||| Timestamp in milliseconds since Unix epoch
public export
Timestamp : Type
Timestamp = Nat

||| Cryptographic token (simplified - would use proper crypto in production)
public export
data CryptoToken : Type where
  MkToken : (value : String) ->
            {auto 0 validLength : LengthIs value 64} ->
            CryptoToken

||| Cryptographic signature
public export
data CryptoSignature : Type where
  MkSignature : (value : String) ->
                {auto 0 validFormat : IsValidSignature value} ->
                CryptoSignature

||| HTTP response code
public export
data HTTPResponseCode : Type where
  OK : HTTPResponseCode                    -- 200
  BadRequest : HTTPResponseCode            -- 400
  NotFound : HTTPResponseCode              -- 404
  InternalServerError : HTTPResponseCode   -- 500

||| HTTP response with timing
public export
record HTTPResponse where
  constructor MkHTTPResponse
  code : HTTPResponseCode
  responseTime : Nat  -- in milliseconds

--------------------------------------------------------------------------------
-- Proof Predicates (would be implemented in separate modules)
--------------------------------------------------------------------------------

||| Placeholder: URL format validation
public export
IsValidURLFormat : String -> Type
IsValidURLFormat s = ()  -- Simplified: would check https://, valid domain, etc.

||| Placeholder: String length check
public export
LengthIs : String -> Nat -> Type
LengthIs s n = ()  -- Would verify length(s) == n

||| Placeholder: Signature format validation
public export
IsValidSignature : String -> Type
IsValidSignature s = ()  -- Would verify hex format, length, etc.

||| Placeholder: Cryptographic signature verification
public export
VerifySignature : CryptoSignature -> CryptoToken -> Type
VerifySignature sig token = ()  -- Would perform actual crypto verification

||| Time difference predicate
public export
data TimeDiff : Timestamp -> Timestamp -> Nat -> Type where
  MkTimeDiff : (t1 : Timestamp) -> (t2 : Timestamp) -> (diff : Nat) ->
               {auto 0 correctDiff : (t2 - t1 = diff) || (t1 - t2 = diff)} ->
               TimeDiff t1 t2 diff

--------------------------------------------------------------------------------
-- Sender Identity Types
--------------------------------------------------------------------------------

||| Unique sender identifier
public export
data SenderID : Type where
  MkSenderID : String -> SenderID

||| Trust level based on account age
public export
data TrustLevel : Type where
  Untrusted : TrustLevel                              -- < 30 days
  Apprentice : TrustLevel                             -- 30-90 days
  Trusted : TrustLevel                                -- 90-365 days
  Veteran : TrustLevel                                -- 365+ days

||| Sender identity with time-based reputation
public export
record SenderIdentity where
  constructor MkSender
  id : SenderID
  registeredAt : Timestamp
  trustLevel : TrustLevel
  dailyLimit : Nat
  publicKey : CryptoToken

  ||| Proof that trust level matches account age
  {auto 0 trustLevelValid : TrustLevelMatchesAge trustLevel registeredAt}

||| Placeholder: Trust level validation
public export
TrustLevelMatchesAge : TrustLevel -> Timestamp -> Type
TrustLevelMatchesAge level regTime = ()  -- Would check age constraints

--------------------------------------------------------------------------------
-- Message Types
--------------------------------------------------------------------------------

||| Email address
public export
data EmailAddress : Type where
  MkEmail : String -> EmailAddress

||| Message content (simplified)
public export
record MessageContent where
  constructor MkContent
  subject : String
  body : String
  from : EmailAddress
  to : EmailAddress

||| Current timestamp (would be IO action in real implementation)
public export
Now : Timestamp
Now = 1706630400000  -- Placeholder: 2026-01-30

--------------------------------------------------------------------------------
-- Export all types
--------------------------------------------------------------------------------

public export
%inline
getURLString : URL -> String
getURLString (MkURL s) = s

public export
%inline
getTokenValue : CryptoToken -> String
getTokenValue (MkToken v) = v
