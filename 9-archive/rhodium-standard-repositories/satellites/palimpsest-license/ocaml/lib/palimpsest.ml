(* SPDX-License-Identifier: MIT *)
(* SPDX-FileCopyrightText: 2024-2025 Palimpsest Stewardship Council *)

(** Palimpsest License - Main module

    This library provides tools for working with Palimpsest License metadata:
    - Type definitions for license metadata
    - JSON parsing and serialisation
    - Compliance checking
    - Validation utilities
*)

(** Core types for license metadata *)
module Types = Types

(** JSON parser for metadata *)
module Parser = Parser

(** Compliance checker *)
module Compliance = Compliance

(** Re-export commonly used types *)
type metadata = Types.metadata
type compliance_result = Types.compliance_result
type usage_type = Types.usage_type
type consent_status = Types.consent_status
type license_version = Types.license_version

(** Create new metadata with required fields *)
let make_metadata = Types.make_metadata

(** Parse metadata from JSON string *)
let parse = Parser.parse_string

(** Serialise metadata to JSON string *)
let serialise = Parser.serialise_string

(** Serialise metadata to pretty JSON string *)
let serialise_pretty = Parser.serialise_string_pretty

(** Check compliance for requested usages *)
let check_compliance = Compliance.check

(** Check compliance for single usage *)
let check_usage = Compliance.check_usage

(** Generate compliance report *)
let generate_report = Compliance.generate_report

(** Check metadata integrity between original and derivative *)
let check_integrity = Compliance.check_metadata_integrity
