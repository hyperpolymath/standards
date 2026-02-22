(* SPDX-License-Identifier: MIT *)
(* SPDX-FileCopyrightText: 2024-2025 Palimpsest Stewardship Council *)

(** Compliance checker for Palimpsest License.
    Validates usage against license terms and consent grants. *)

open Types

(** Get current ISO timestamp *)
let get_current_timestamp () =
  let open Unix in
  let tm = gmtime (time ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

(** Check if metadata has required attribution information *)
let check_attribution metadata =
  let issues = ref [] in

  (* Check if creators are specified *)
  if List.length metadata.attribution.creators = 0 then
    issues := {
      clause = "2.3 - Attribution";
      description = "No creators specified in attribution metadata";
      issue_severity = Major;
      remediation = Some "Add creator information to metadata";
    } :: !issues;

  (* Check if lineage preservation is required but emotional lineage is missing *)
  if metadata.attribution.must_preserve_lineage &&
     Option.is_none metadata.emotional_lineage then
    issues := {
      clause = "2.3 - Emotional Lineage";
      description = "Lineage preservation required but emotional lineage data is missing";
      issue_severity = Critical;
      remediation = Some "Add emotional lineage metadata including origin, cultural context, and narrative intent";
    } :: !issues;

  List.rev !issues

(** Check if traceability requirements are met *)
let check_traceability metadata =
  let issues = ref [] in

  (* For v0.4+, traceability hash should be present *)
  (match metadata.version with
   | V04 | Custom _ ->
     if Option.is_none metadata.traceability then
       issues := {
         clause = "Quantum-Proof Traceability";
         description = "No traceability hash found for v0.4+ license";
         issue_severity = Major;
         remediation = Some "Generate and include quantum-proof traceability hash";
       } :: !issues
   | V03 -> ());  (* Not required for v0.3 *)

  List.rev !issues

(** Find consent for specific usage type *)
let find_consent metadata usage_type =
  List.find_opt (fun c -> c.usage_type = usage_type) metadata.consents

(** Check if usage is permitted based on consents *)
let check_usage_permission ?current_date metadata requested_usage =
  let issues = ref [] in

  (match find_consent metadata requested_usage with
   | None ->
     (* No consent record found *)
     (match requested_usage with
      | Interpretive | Training ->
        (* Clause 1.2: Non-Interpretive systems require explicit consent *)
        issues := {
          clause = "1.2 - Non-Interpretive Use";
          description = "Interpretive or training use requires explicit consent";
          issue_severity = Critical;
          remediation = Some "Obtain explicit consent for AI interpretive use or training";
        } :: !issues
      | NonInterpretive | Personal -> ()  (* Generally allowed *)
      | Derivative ->
        issues := {
          clause = "Derivative Works";
          description = "No consent found for derivative works";
          issue_severity = Major;
          remediation = Some "Verify if derivative use is permitted and obtain consent if needed";
        } :: !issues
      | Commercial ->
        issues := {
          clause = "Commercial Use";
          description = "No consent found for commercial use";
          issue_severity = Major;
          remediation = Some "Obtain consent for commercial use";
        } :: !issues)
   | Some consent ->
     (* Consent found, check status *)
     (match consent.status with
      | Denied ->
        issues := {
          clause = "Usage Consent";
          description = "Consent explicitly denied for " ^ usage_type_to_string requested_usage;
          issue_severity = Critical;
          remediation = Some "This usage is not permitted under the license";
        } :: !issues
      | NotSpecified ->
        issues := {
          clause = "Usage Consent";
          description = "Consent status not specified for " ^ usage_type_to_string requested_usage;
          issue_severity = Major;
          remediation = Some "Clarify consent status with rights holder";
        } :: !issues
      | Granted | ConditionallyGranted _ ->
        (* Check if consent is still valid *)
        if not (is_consent_valid ?current_date consent) then
          issues := {
            clause = "Usage Consent";
            description = "Consent has expired for " ^ usage_type_to_string requested_usage;
            issue_severity = Critical;
            remediation = Some "Renew consent with rights holder";
          } :: !issues;

        (* If conditionally granted, note the conditions *)
        (match consent.status with
         | ConditionallyGranted conditions ->
           issues := {
             clause = "Usage Consent";
             description = "Usage permitted with conditions: " ^ conditions;
             issue_severity = Minor;
             remediation = Some "Ensure all specified conditions are met";
           } :: !issues
         | _ -> ())));

  List.rev !issues

(** Comprehensive compliance check *)
let check ?current_date metadata requested_usages =
  let all_issues = ref [] in

  (* Check attribution *)
  all_issues := check_attribution metadata @ !all_issues;

  (* Check traceability *)
  all_issues := check_traceability metadata @ !all_issues;

  (* Check each requested usage *)
  List.iter (fun usage ->
    all_issues := check_usage_permission ?current_date metadata usage @ !all_issues
  ) requested_usages;

  (* Determine overall compliance *)
  let has_critical = List.exists (fun issue ->
    issue.issue_severity = Critical
  ) !all_issues in

  {
    is_compliant = not has_critical;
    issues = List.rev !all_issues;
    checked_at = get_current_timestamp ();
  }

(** Quick check for single usage type *)
let check_usage ?current_date metadata usage =
  check ?current_date metadata [usage]

(** Check if metadata stripping has occurred (Clause 2.3 violation) *)
let check_metadata_integrity ~original ~derivative =
  let issues = ref [] in

  (* Check if attribution is preserved *)
  if List.length derivative.attribution.creators <
     List.length original.attribution.creators then
    issues := {
      clause = "2.3 - Metadata Preservation";
      description = "Creator attribution has been stripped or reduced";
      issue_severity = Critical;
      remediation = Some "Restore complete creator attribution from original";
    } :: !issues;

  (* Check if emotional lineage is preserved when required *)
  if original.attribution.must_preserve_lineage then
    (match original.emotional_lineage, derivative.emotional_lineage with
     | Some _, None ->
       issues := {
         clause = "2.3 - Emotional Lineage";
         description = "Emotional lineage metadata has been stripped";
         issue_severity = Critical;
         remediation = Some "Restore emotional lineage metadata from original";
       } :: !issues
     | _ -> ());

  (* Check if traceability is preserved *)
  (match original.traceability, derivative.traceability with
   | Some _, None ->
     issues := {
       clause = "Traceability";
       description = "Traceability hash has been stripped";
       issue_severity = Critical;
       remediation = Some "Restore traceability hash or create new hash linking to original";
     } :: !issues
   | _ -> ());

  List.rev !issues

(** Generate compliance report as string *)
let generate_report result =
  let status = if result.is_compliant then
    "COMPLIANT" else "NON-COMPLIANT" in

  let buf = Buffer.create 1024 in
  Buffer.add_string buf "Palimpsest License Compliance Report\n";
  Buffer.add_string buf ("Status: " ^ status ^ "\n");
  Buffer.add_string buf ("Checked at: " ^ result.checked_at ^ "\n\n");

  if List.length result.issues = 0 then
    Buffer.add_string buf "No compliance issues found.\n"
  else begin
    Buffer.add_string buf (Printf.sprintf "Issues found: %d\n\n"
      (List.length result.issues));

    List.iteri (fun idx issue ->
      let severity_symbol = match issue.issue_severity with
        | Critical -> "[CRITICAL]"
        | Major -> "[MAJOR]"
        | Minor -> "[MINOR]"
        | Error -> "[ERROR]"
        | Warning -> "[WARNING]"
      in
      let remediation = match issue.remediation with
        | Some r -> "\n  Remediation: " ^ r
        | None -> ""
      in
      Buffer.add_string buf (Printf.sprintf "%d. %s %s\n  %s%s\n\n"
        (idx + 1) severity_symbol issue.clause issue.description remediation)
    ) result.issues
  end;

  Buffer.contents buf
