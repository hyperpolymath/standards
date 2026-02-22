// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
//
// axelSts.res - AXEL-STS DNS Record Validator

module AxelSts = {
  type mode = Testing | Enforce

  type t = {
    version: string,
    mode: mode,
    ipv6Only: bool,
    attestationUrl: string,
  }

  let modeToString = mode =>
    switch mode {
    | Testing => "testing"
    | Enforce => "enforce"
    }

  let modeFromString = str =>
    switch str->Js.String.toLowerCase {
    | "testing" => Some(Testing)
    | "enforce" => Some(Enforce)
    | _ => None
    }

  let parse: string => option<t> = txt => {
    txt
    ->Js.String.split(";")
    ->Array.map(s => s->Js.String.trim)
    ->Array.toList
    ->List.filter(s => s->Js.String.length > 0)
    ->List.map(part => {
      let keyValue = part->Js.String.split("=")
      switch keyValue {
      | [key, value] => Some((key->Js.String.trim, value->Js.String.trim))
      | _ => None
      }
    })
    ->List.filterMap(identity)
    ->List.toArray
    ->Js.Dict.fromArray
    ->dict => {
      let version = dict->Js.Dict.get("v")->Option.getWithDefault("AXEL1")
      let mode = dict->Js.Dict.get("mode")->Option.flatMap(modeFromString)->Option.getWithDefault(Testing)
      let ipv6Only = dict->Js.Dict.get("ipv6-only")->Option.flatMap(v =>
        v->Js.String.toInt->Option.map(i => i == 1)
      )->Option.getWithDefault(true)
      let attestationUrl = dict->Js.Dict.get("attestation")->Option.getWithDefault("")

      // Validate required fields
      if version != "AXEL1" || attestationUrl == "" {
        None
      } else {
        Some({version, mode, ipv6Only, attestationUrl})
      }
    }
  }

  let validate: t => result<unit, string> = record => {
    // Check version
    if record.version != "AXEL1" {
      Error("Invalid version: " ++ record.version)
    } else if record.attestationUrl == "" {
      Error("Missing attestation URL")
    } else if !record.attestationUrl->Js.String.startsWith("https://") {
      Error("Attestation URL must use HTTPS")
    } else {
      Ok()
    }
  }

  let toString: t => string = record => {
    let parts = [
      "v=" ++ record.version,
      "mode=" ++ modeToString(record.mode),
      "ipv6-only=" ++ (record.ipv6Only ? "1" : "0"),
      "attestation=" ++ record.attestationUrl,
    ]
    parts->Array.joinWith("; ")
  }
}

module Apl = {
  type family = IPv6 | IPv4

  type prefix = {
    family: family,
    address: string,
    length: int,
  }

  type t = array<prefix>

  let familyToInt = family =>
    switch family {
    | IPv6 => 1
    | IPv4 => 2
    }

  let familyFromInt = int =>
    switch int {
    | 1 => Some(IPv6)
    | 2 => Some(IPv4)
    | _ => None
    }

  let parsePrefix: string => option<prefix> = str => {
    let parts = str->Js.String.split(":")
    switch parts {
    | [familyStr, cidr] => {
        let family = familyStr->Js.String.toInt->Option.flatMap(familyFromInt)
        let cidrParts = cidr->Js.String.split("/")
        switch (family, cidrParts) {
        | (Some(fam), [address, lengthStr]) => {
            let length = lengthStr->Js.String.toInt->Option.getWithDefault(0)
            Some({family: fam, address, length})
          }
        | _ => None
        }
      }
    | _ => None
    }
  }

  let parse: string => option<t> = txt => {
    txt
    ->Js.String.split(" ")
    ->Array.map(s => s->Js.String.trim)
    ->Array.toList
    ->List.filter(s => s->Js.String.length > 0)
    ->List.map(parsePrefix)
    ->List.filterMap(identity)
    ->List.toArray
    ->Some
  }

  let toString: t => string = prefixes => {
    prefixes
    ->Array.map(p => {
      let familyInt = familyToInt(p.family)
      Js.String.make(familyInt) ++ ":" ++ p.address ++ "/" ++ Js.String.make(p.length)
    })
    ->Array.joinWith(" ")
  }

  let isIpInPrefixes: (string, t) => bool = (ip, prefixes) => {
    // Simplified check - in production, use proper CIDR matching
    prefixes->Array.some(prefix => ip->Js.String.startsWith(prefix.address))
  }
}

module Attestation = {
  type method = ZKP | GovId | CreditCard

  type claims = {
    iss: string,
    sub: string,
    aud: string,
    exp: float,
    iat: float,
    ageVerified: bool,
    method: method,
    minAge: option<int>,
  }

  let methodToString = method =>
    switch method {
    | ZKP => "zkp"
    | GovId => "gov_id"
    | CreditCard => "credit_card"
    }

  let methodFromString = str =>
    switch str {
    | "zkp" => Some(ZKP)
    | "gov_id" => Some(GovId)
    | "credit_card" => Some(CreditCard)
    | _ => None
    }

  let validateToken: string => result<claims, string> = _token => {
    // In production, implement proper JWT validation
    // For now, return a placeholder error
    Error("JWT validation not implemented")
  }

  let isExpired: claims => bool = claims => {
    let now = Js.Date.now() /. 1000.0
    claims.exp < now
  }

  let isValidLifetime: claims => bool = claims => {
    let lifetime = claims.exp -. claims.iat
    lifetime <= 900.0 // 15 minutes max
  }
}

// Example usage:
let exampleSts = "_axel._sts.example.com. IN TXT \"v=AXEL1; mode=enforce; ipv6-only=1; attestation=https://example.com/.well-known/axel/attestation\""

let result = AxelSts.parse(exampleSts)
switch result {
| Some(record) => {
    Js.log("Parsed AXEL-STS record:")
    Js.log("  Version: " ++ record.version)
    Js.log("  Mode: " ++ AxelSts.modeToString(record.mode))
    Js.log("  IPv6 Only: " ++ (record.ipv6Only ? "true" : "false"))
    Js.log("  Attestation URL: " ++ record.attestationUrl)

    switch AxelSts.validate(record) {
    | Ok() => Js.log("✓ Record is valid")
    | Error(msg) => Js.log("✗ Validation error: " ++ msg)
    }
  }
| None => Js.log("✗ Failed to parse AXEL-STS record")
}
