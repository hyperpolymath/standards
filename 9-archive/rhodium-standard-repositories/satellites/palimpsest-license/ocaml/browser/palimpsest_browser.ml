(* SPDX-License-Identifier: MIT *)
(* SPDX-FileCopyrightText: 2024-2025 Palimpsest Stewardship Council *)

(** Browser entry point for Palimpsest License tools.
    Compiled to JavaScript via Melange. *)

open Palimpsest
open Palimpsest.Types

(** Parse metadata from JSON string - exposed to JS *)
let parse_metadata json_string =
  match Parser.parse_string json_string with
  | Ok metadata -> `Ok metadata
  | Error msg -> `Error msg

(** Check compliance for metadata - exposed to JS *)
let check_compliance metadata usages =
  Compliance.check metadata usages

(** Generate compliance report as string *)
let generate_report result =
  Compliance.generate_report result

(** Serialise metadata to JSON string *)
let to_json metadata =
  Parser.serialise_string_pretty metadata

(** Create minimal metadata *)
let create_metadata ~title ~license_uri =
  make_metadata ~work_title:title ~license_uri

(** Version info *)
let version = "0.4.0"

(** Check if training use is permitted *)
let is_training_permitted metadata =
  let result = Compliance.check_usage metadata Training in
  result.is_compliant

(** Check if interpretive AI use is permitted *)
let is_interpretive_permitted metadata =
  let result = Compliance.check_usage metadata Interpretive in
  result.is_compliant

(** Quick compliance check for common AI use cases *)
let check_ai_compliance metadata =
  let result = Compliance.check metadata [Training; Interpretive] in
  {|
{
  "compliant": |} ^ string_of_bool result.is_compliant ^ {|,
  "issues": |} ^ string_of_int (List.length result.issues) ^ {|,
  "checkedAt": "|} ^ result.checked_at ^ {|"
}|}
