// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
// Mock STAMP verification library (ReScript)
// Temporary implementation for MVP. Will be replaced with real libstamp FFI.

@val external setTimeout: (unit => unit, int) => int = "setTimeout"

type unsubscribeParams = {
  url: string,
  tested_at: float,
  response_code: int,
  response_time: int,
  token: string,
  signature: string,
}

type consentParams = {
  initial_request: float,
  confirmation: float,
  ip_address: string,
  token: string,
}

type rateLimitParams = {
  sender_id: string,
  account_created: float,
  messages_today: int,
  daily_limit: int,
}

type proof = {
  type_: string,
  data: JSON.t,
  timestamp: float,
  signature: string,
}

type verificationResult =
  | Success
  | ErrorInvalidUrl
  | ErrorTimeout
  | ErrorInvalidResponse
  | ErrorInvalidSignature
  | ErrorRateLimitExceeded
  | ErrorConsentInvalid
  | ErrorNullPointer
  | ErrorInternal

let verifyUnsubscribe = (params: unsubscribeParams): verificationResult => {
  if !params.url->String.startsWith("https://") {
    ErrorInvalidUrl
  } else {
    let now = Date.now()
    let ageMs = now -. params.tested_at

    if ageMs > 60000.0 || ageMs < 0.0 {
      ErrorTimeout
    } else if params.response_code !== 200 {
      ErrorInvalidResponse
    } else if params.response_time >= 200 {
      ErrorTimeout
    } else if params.signature == "" {
      ErrorInvalidSignature
    } else {
      Success
    }
  }
}

let verifyConsent = (params: consentParams): verificationResult => {
  if params.confirmation <= params.initial_request {
    ErrorConsentInvalid
  } else {
    let timeDiff = params.confirmation -. params.initial_request

    if timeDiff > 86400000.0 {
      ErrorConsentInvalid
    } else if params.token == "" {
      ErrorInvalidSignature
    } else {
      Success
    }
  }
}

let verifyRateLimit = (params: rateLimitParams): verificationResult => {
  if params.messages_today >= params.daily_limit {
    ErrorRateLimitExceeded
  } else {
    let now = Date.now()
    let ageMs = now -. params.account_created
    let ageDays = ageMs /. (24.0 *. 60.0 *. 60.0 *. 1000.0)

    let maxLimit = if ageDays < 30.0 {
      1000
    } else if ageDays < 90.0 {
      10000
    } else {
      100000
    }

    if params.daily_limit > maxLimit {
      ErrorRateLimitExceeded
    } else {
      Success
    }
  }
}

let generateProof = (type_: [#unsubscribe | #consent | #rateLimit], data: JSON.t): proof => {
  let timestamp = Date.now()
  let random = Math.random()->Float.toString->String.sliceToEnd(~start=2)
  let signature = `mock_sig_${timestamp->Float.toString}_${random}`

  let typeStr = switch type_ {
  | #unsubscribe => "unsubscribe_verification"
  | #consent => "consent_verification"
  | #rateLimit => "rate_limit_verification"
  }

  {type_: typeStr, data, timestamp, signature}
}

let resultToString = (result: verificationResult): string => {
  switch result {
  | Success => "SUCCESS"
  | ErrorInvalidUrl => "INVALID_URL"
  | ErrorTimeout => "TIMEOUT"
  | ErrorInvalidResponse => "INVALID_RESPONSE"
  | ErrorInvalidSignature => "INVALID_SIGNATURE"
  | ErrorRateLimitExceeded => "RATE_LIMIT_EXCEEDED"
  | ErrorConsentInvalid => "CONSENT_INVALID"
  | ErrorNullPointer => "NULL_POINTER"
  | ErrorInternal => "INTERNAL_ERROR"
  }
}

let formatProof = (proof: proof): string => {
  let dict = Dict.make()
  dict->Dict.set("type", JSON.Encode.string(proof.type_))
  dict->Dict.set("data", proof.data)
  dict->Dict.set("timestamp", JSON.Encode.float(proof.timestamp))
  dict->Dict.set("signature", JSON.Encode.string(proof.signature))
  JSON.stringifyAnyWithIndent(dict, 2)->Option.getOr("{}")
}

let generateUnsubscribeUrl = (userId: int, token: string): string => {
  `https://stamp-bot.example.com/unsubscribe?user=${userId->Int.toString}&token=${token}`
}

let testUnsubscribeUrl = async (url: string): (int, int) => {
  let start = Date.now()

  let delay = 50.0 +. Math.random() *. 100.0
  await Promise.make((resolve, _reject) => {
    let _ = setTimeout(() => resolve(), delay->Float.toInt)
  })

  let responseTime = (Date.now() -. start)->Float.toInt
  let responseCode = if url->String.startsWith("https://") {
    200
  } else {
    404
  }

  (responseCode, responseTime)
}

let generateToken = (userId: int): string => {
  let random = Math.random()->Float.toString->String.slice(~start=2, ~end=15)
  let timestamp = Date.now()->Float.toInt->Int.toString
  `${userId->Int.toString}_${timestamp}_${random}`
}

let generateSignature = (data: string): string => {
  let timestamp = Date.now()->Float.toInt->Int.toString
  let length = data->String.length->Int.toString
  let random = Math.random()->Float.toString->String.slice(~start=2, ~end=9)
  `sig_${timestamp}_${length}_${random}`
}

let unsubscribeParamsToJson = (params: unsubscribeParams): JSON.t => {
  let dict = Dict.make()
  dict->Dict.set("url", JSON.Encode.string(params.url))
  dict->Dict.set("tested_at", JSON.Encode.float(params.tested_at))
  dict->Dict.set("response_code", JSON.Encode.int(params.response_code))
  dict->Dict.set("response_time", JSON.Encode.int(params.response_time))
  dict->Dict.set("token", JSON.Encode.string(params.token))
  dict->Dict.set("signature", JSON.Encode.string(params.signature))
  JSON.Encode.object(dict)
}

let consentParamsToJson = (params: consentParams): JSON.t => {
  let dict = Dict.make()
  dict->Dict.set("initial_request", JSON.Encode.float(params.initial_request))
  dict->Dict.set("confirmation", JSON.Encode.float(params.confirmation))
  dict->Dict.set("ip_address", JSON.Encode.string(params.ip_address))
  dict->Dict.set("token", JSON.Encode.string(params.token))
  JSON.Encode.object(dict)
}

let rateLimitParamsToJson = (params: rateLimitParams): JSON.t => {
  let dict = Dict.make()
  dict->Dict.set("sender_id", JSON.Encode.string(params.sender_id))
  dict->Dict.set("account_created", JSON.Encode.float(params.account_created))
  dict->Dict.set("messages_today", JSON.Encode.int(params.messages_today))
  dict->Dict.set("daily_limit", JSON.Encode.int(params.daily_limit))
  JSON.Encode.object(dict)
}
