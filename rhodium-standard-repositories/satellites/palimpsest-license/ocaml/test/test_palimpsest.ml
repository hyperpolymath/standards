(* SPDX-License-Identifier: MIT *)
(* SPDX-FileCopyrightText: 2024-2025 Palimpsest Stewardship Council *)

(** Tests for Palimpsest License library *)

open Palimpsest
open Palimpsest.Types

let test_version_conversion () =
  Alcotest.(check string) "v0.3" "0.3" (version_to_string V03);
  Alcotest.(check string) "v0.4" "0.4" (version_to_string V04);
  Alcotest.(check string) "custom" "1.0" (version_to_string (Custom "1.0"))

let test_version_parsing () =
  let check_version input expected =
    let result = version_of_string input in
    match result, expected with
    | V03, V03 -> ()
    | V04, V04 -> ()
    | Custom a, Custom b when a = b -> ()
    | _ -> Alcotest.fail "Version mismatch"
  in
  check_version "0.3" V03;
  check_version "v0.3" V03;
  check_version "0.4" V04;
  check_version "v0.4" V04;
  check_version "1.0" (Custom "1.0")

let test_language_conversion () =
  Alcotest.(check string) "en" "en" (language_to_string En);
  Alcotest.(check string) "nl" "nl" (language_to_string Nl);
  Alcotest.(check string) "other" "de" (language_to_string (Other "de"))

let test_usage_type_conversion () =
  Alcotest.(check string) "interpretive" "interpretive"
    (usage_type_to_string Interpretive);
  Alcotest.(check string) "training" "training"
    (usage_type_to_string Training);
  Alcotest.(check string) "commercial" "commercial"
    (usage_type_to_string Commercial)

let test_consent_granted () =
  let granted = {
    usage_type = Personal;
    status = Granted;
    granted_date = None;
    expiry_date = None;
    conditions = None;
    revocable = false;
  } in
  let denied = { granted with status = Denied } in
  Alcotest.(check bool) "granted is granted" true (is_consent_granted granted);
  Alcotest.(check bool) "denied is not granted" false (is_consent_granted denied)

let test_parse_minimal () =
  let json = {|{"workTitle": "Test Work", "licenseUri": "https://palimpsestlicense.org/v0.4"}|} in
  match parse json with
  | Ok metadata ->
    Alcotest.(check string) "work title" "Test Work" metadata.work_title;
    Alcotest.(check string) "license uri" "https://palimpsestlicense.org/v0.4" metadata.license_uri
  | Error msg -> Alcotest.fail msg

let test_parse_missing_required () =
  let json = {|{"workTitle": "Test Work"}|} in
  match parse json with
  | Ok _ -> Alcotest.fail "Should have failed"
  | Error msg ->
    Alcotest.(check bool) "contains licenseUri error" true
      (String.length msg > 0)

let test_compliance_no_issues () =
  let metadata = make_metadata
    ~work_title:"Test"
    ~license_uri:"https://palimpsestlicense.org/v0.4" in
  let result = check_usage metadata Personal in
  Alcotest.(check bool) "is compliant" true result.is_compliant

let test_compliance_training_requires_consent () =
  let metadata = make_metadata
    ~work_title:"Test"
    ~license_uri:"https://palimpsestlicense.org/v0.4" in
  let result = check_usage metadata Training in
  Alcotest.(check bool) "not compliant without consent" false result.is_compliant;
  Alcotest.(check bool) "has issues" true (List.length result.issues > 0)

let test_serialise_roundtrip () =
  let original = make_metadata
    ~work_title:"Roundtrip Test"
    ~license_uri:"https://palimpsestlicense.org/v0.4" in
  let json = serialise original in
  match parse json with
  | Ok parsed ->
    Alcotest.(check string) "work title preserved"
      original.work_title parsed.work_title;
    Alcotest.(check string) "license uri preserved"
      original.license_uri parsed.license_uri
  | Error msg -> Alcotest.fail msg

let () =
  let open Alcotest in
  run "Palimpsest" [
    "types", [
      test_case "version conversion" `Quick test_version_conversion;
      test_case "version parsing" `Quick test_version_parsing;
      test_case "language conversion" `Quick test_language_conversion;
      test_case "usage type conversion" `Quick test_usage_type_conversion;
      test_case "consent granted check" `Quick test_consent_granted;
    ];
    "parser", [
      test_case "parse minimal metadata" `Quick test_parse_minimal;
      test_case "parse missing required field" `Quick test_parse_missing_required;
      test_case "serialise roundtrip" `Quick test_serialise_roundtrip;
    ];
    "compliance", [
      test_case "no issues for personal use" `Quick test_compliance_no_issues;
      test_case "training requires consent" `Quick test_compliance_training_requires_consent;
    ];
  ]
