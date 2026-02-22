(* SPDX-License-Identifier: MIT *)
(* SPDX-FileCopyrightText: 2024-2025 Palimpsest Stewardship Council *)

(** Core type definitions for Palimpsest License metadata and operations *)

(** License version *)
type license_version =
  | V03
  | V04
  | Custom of string
[@@deriving yojson]

(** Language codes (ISO 639-1) *)
type language =
  | En  (** English *)
  | Nl  (** Dutch *)
  | Other of string
[@@deriving yojson]

(** Usage types as defined in the license *)
type usage_type =
  | Interpretive      (** AI systems that interpret/understand content *)
  | NonInterpretive   (** Simple storage/display *)
  | Derivative        (** Creating derivative works *)
  | Training          (** AI training/learning *)
  | Commercial        (** Commercial use *)
  | Personal          (** Personal/non-commercial use *)
[@@deriving yojson]

(** Consent status for different usage types *)
type consent_status =
  | Granted
  | Denied
  | ConditionallyGranted of string  (** with conditions *)
  | NotSpecified
[@@deriving yojson]

(** Severity levels for validation and compliance *)
type severity =
  | Critical
  | Major
  | Minor
  | Error
  | Warning
[@@deriving yojson]

(** Emotional lineage information *)
type emotional_lineage = {
  origin: string option;
  cultural_context: string option;
  trauma_marker: bool;
  symbolic_weight: string option;
  narrative_intent: string option;
} [@@deriving yojson]

(** Creator information *)
type creator = {
  name: string;
  identifier: string option;  (** URI, ORCID, etc. *)
  role: string option;
  contact: string option;
} [@@deriving yojson]

(** Attribution requirements *)
type attribution = {
  creators: creator list;
  source: string option;
  original_title: string option;
  date_created: string option;
  must_preserve_lineage: bool;
} [@@deriving yojson]

(** Consent record for specific usage *)
type consent = {
  usage_type: usage_type;
  status: consent_status;
  granted_date: string option;
  expiry_date: string option;
  conditions: string list option;
  revocable: bool;
} [@@deriving yojson]

(** Traceability hash for quantum-proof verification *)
type traceability_hash = {
  algorithm: string;
  value: string;
  timestamp: string;
  blockchain_ref: string option;
} [@@deriving yojson]

(** Complete license metadata *)
type metadata = {
  version: license_version;
  language: language;
  work_title: string;
  work_identifier: string option;
  license_uri: string;
  attribution: attribution;
  consents: consent list;
  emotional_lineage: emotional_lineage option;
  traceability: traceability_hash option;
} [@@deriving yojson]

(** Validation error *)
type validation_error = {
  field: string;
  message: string;
  severity: severity;
} [@@deriving yojson]

(** Validation result *)
type validation_result =
  | Valid
  | Invalid of validation_error list

(** Compliance issue *)
type compliance_issue = {
  clause: string;
  description: string;
  issue_severity: severity;
  remediation: string option;
} [@@deriving yojson]

(** Compliance check result *)
type compliance_result = {
  is_compliant: bool;
  issues: compliance_issue list;
  checked_at: string;
} [@@deriving yojson]

(** Helper functions for version handling *)

let version_to_string = function
  | V03 -> "0.3"
  | V04 -> "0.4"
  | Custom v -> v

let version_of_string = function
  | "0.3" | "v0.3" -> V03
  | "0.4" | "v0.4" -> V04
  | v -> Custom v

(** Helper functions for language handling *)

let language_to_string = function
  | En -> "en"
  | Nl -> "nl"
  | Other code -> code

let language_of_string = function
  | "en" | "eng" -> En
  | "nl" | "nld" | "dut" -> Nl
  | code -> Other code

(** Helper functions for usage type handling *)

let usage_type_to_string = function
  | Interpretive -> "interpretive"
  | NonInterpretive -> "non-interpretive"
  | Derivative -> "derivative"
  | Training -> "training"
  | Commercial -> "commercial"
  | Personal -> "personal"

let usage_type_of_string = function
  | "interpretive" -> Some Interpretive
  | "non-interpretive" | "noninterpretive" -> Some NonInterpretive
  | "derivative" -> Some Derivative
  | "training" -> Some Training
  | "commercial" -> Some Commercial
  | "personal" -> Some Personal
  | _ -> None

(** Check if consent is effectively granted *)
let is_consent_granted consent =
  match consent.status with
  | Granted | ConditionallyGranted _ -> true
  | Denied | NotSpecified -> false

(** Check if consent is still valid (not expired) *)
let is_consent_valid ?current_date consent =
  match consent.expiry_date with
  | None -> true
  | Some expiry ->
    match current_date with
    | None -> true
    | Some current -> current <= expiry  (* Simple string comparison *)

(** Create default empty emotional lineage *)
let empty_emotional_lineage = {
  origin = None;
  cultural_context = None;
  trauma_marker = false;
  symbolic_weight = None;
  narrative_intent = None;
}

(** Create default empty attribution *)
let empty_attribution = {
  creators = [];
  source = None;
  original_title = None;
  date_created = None;
  must_preserve_lineage = false;
}

(** Create default metadata with minimal required fields *)
let make_metadata ~work_title ~license_uri =
  {
    version = V04;
    language = En;
    work_title;
    work_identifier = None;
    license_uri;
    attribution = empty_attribution;
    consents = [];
    emotional_lineage = None;
    traceability = None;
  }
