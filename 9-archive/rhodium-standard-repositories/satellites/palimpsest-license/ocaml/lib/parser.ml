(* SPDX-License-Identifier: MIT *)
(* SPDX-FileCopyrightText: 2024-2025 Palimpsest Stewardship Council *)

(** Parser for Palimpsest License metadata (JSON-LD and plain JSON) *)

open Types

(** Helper to safely extract string from JSON *)
let get_string json =
  match json with
  | `String s -> Some s
  | _ -> None

(** Helper to safely extract bool from JSON *)
let get_bool json =
  match json with
  | `Bool b -> Some b
  | _ -> None

(** Helper to get field from assoc list *)
let get_field key assoc =
  List.assoc_opt key assoc

(** Helper to get string field with default *)
let get_string_field key assoc =
  match get_field key assoc with
  | Some json -> get_string json
  | None -> None

(** Helper to get bool field with default *)
let get_bool_field ?(default=false) key assoc =
  match get_field key assoc with
  | Some json -> Option.value ~default (get_bool json)
  | None -> default

(** Parse creator from JSON *)
let parse_creator json =
  match json with
  | `Assoc assoc ->
    Some {
      name = Option.value ~default:"Unknown" (get_string_field "name" assoc);
      identifier = get_string_field "identifier" assoc;
      role = get_string_field "role" assoc;
      contact = get_string_field "contact" assoc;
    }
  | _ -> None

(** Parse list of creators *)
let parse_creators json =
  match json with
  | `List items -> List.filter_map parse_creator items
  | _ -> []

(** Parse attribution from JSON *)
let parse_attribution json =
  match json with
  | `Assoc assoc ->
    let creators = match get_field "creators" assoc with
      | Some c -> parse_creators c
      | None -> []
    in
    Some {
      creators;
      source = get_string_field "source" assoc;
      original_title = get_string_field "originalTitle" assoc;
      date_created = get_string_field "dateCreated" assoc;
      must_preserve_lineage = get_bool_field "mustPreserveLineage" assoc;
    }
  | _ -> None

(** Parse emotional lineage from JSON *)
let parse_emotional_lineage json =
  match json with
  | `Assoc assoc ->
    Some {
      origin = get_string_field "origin" assoc;
      cultural_context = get_string_field "culturalContext" assoc;
      trauma_marker = get_bool_field "traumaMarker" assoc;
      symbolic_weight = get_string_field "symbolicWeight" assoc;
      narrative_intent = get_string_field "narrativeIntent" assoc;
    }
  | _ -> None

(** Parse consent status from string *)
let parse_consent_status str =
  match str with
  | "granted" -> Granted
  | "denied" -> Denied
  | "not-specified" | "notSpecified" -> NotSpecified
  | s when String.length s > 12 && String.sub s 0 12 = "conditional:" ->
    ConditionallyGranted (String.sub s 12 (String.length s - 12))
  | _ -> NotSpecified

(** Parse single consent from JSON *)
let parse_consent json =
  match json with
  | `Assoc assoc ->
    (match get_string_field "usageType" assoc with
     | None -> None
     | Some usage_str ->
       match usage_type_of_string usage_str with
       | None -> None
       | Some usage_type ->
         let status_str = Option.value ~default:"not-specified"
           (get_string_field "status" assoc) in
         let conditions = match get_field "conditions" assoc with
           | Some (`List items) ->
             Some (List.filter_map get_string items)
           | _ -> None
         in
         Some {
           usage_type;
           status = parse_consent_status status_str;
           granted_date = get_string_field "grantedDate" assoc;
           expiry_date = get_string_field "expiryDate" assoc;
           conditions;
           revocable = get_bool_field "revocable" assoc;
         })
  | _ -> None

(** Parse list of consents *)
let parse_consents json =
  match json with
  | `List items -> List.filter_map parse_consent items
  | _ -> []

(** Parse traceability hash from JSON *)
let parse_traceability json =
  match json with
  | `Assoc assoc ->
    Some {
      algorithm = Option.value ~default:"SHA-256"
        (get_string_field "algorithm" assoc);
      value = Option.value ~default:""
        (get_string_field "value" assoc);
      timestamp = Option.value ~default:""
        (get_string_field "timestamp" assoc);
      blockchain_ref = get_string_field "blockchainRef" assoc;
    }
  | _ -> None

(** Main parser: converts JSON to metadata structure *)
let parse json =
  match json with
  | `Assoc assoc ->
    let version = match get_string_field "version" assoc with
      | Some v -> version_of_string v
      | None -> V04
    in
    let language = match get_string_field "language" assoc with
      | Some l -> language_of_string l
      | None -> En
    in
    let work_title = get_string_field "workTitle" assoc in
    let license_uri = get_string_field "licenseUri" assoc in

    (match work_title, license_uri with
     | None, _ -> Error "Missing required field: workTitle"
     | _, None -> Error "Missing required field: licenseUri"
     | Some title, Some uri ->
       let attribution = match get_field "attribution" assoc with
         | Some a -> Option.value ~default:empty_attribution (parse_attribution a)
         | None -> empty_attribution
       in
       let consents = match get_field "consents" assoc with
         | Some c -> parse_consents c
         | None -> []
       in
       let emotional_lineage = match get_field "emotionalLineage" assoc with
         | Some e -> parse_emotional_lineage e
         | None -> None
       in
       let traceability = match get_field "traceability" assoc with
         | Some t -> parse_traceability t
         | None -> None
       in
       Ok {
         version;
         language;
         work_title = title;
         work_identifier = get_string_field "workIdentifier" assoc;
         license_uri = uri;
         attribution;
         consents;
         emotional_lineage;
         traceability;
       })
  | _ -> Error "Invalid JSON: expected object"

(** Parse from JSON string *)
let parse_string json_string =
  try
    let json = Yojson.Safe.from_string json_string in
    parse json
  with
  | Yojson.Json_error msg -> Error msg

(** Serialise metadata to JSON *)
let serialise metadata =
  let creators_json = List.map (fun c ->
    let fields = [("name", `String c.name)] in
    let fields = match c.identifier with
      | Some id -> ("identifier", `String id) :: fields
      | None -> fields
    in
    let fields = match c.role with
      | Some r -> ("role", `String r) :: fields
      | None -> fields
    in
    let fields = match c.contact with
      | Some ct -> ("contact", `String ct) :: fields
      | None -> fields
    in
    `Assoc (List.rev fields)
  ) metadata.attribution.creators in

  let consents_json = List.map (fun c ->
    let status_str = match c.status with
      | Granted -> "granted"
      | Denied -> "denied"
      | NotSpecified -> "not-specified"
      | ConditionallyGranted cond -> "conditional:" ^ cond
    in
    `Assoc [
      ("usageType", `String (usage_type_to_string c.usage_type));
      ("status", `String status_str);
      ("revocable", `Bool c.revocable);
    ]
  ) metadata.consents in

  let fields = [
    ("version", `String (version_to_string metadata.version));
    ("language", `String (language_to_string metadata.language));
    ("workTitle", `String metadata.work_title);
    ("licenseUri", `String metadata.license_uri);
    ("attribution", `Assoc [
      ("creators", `List creators_json);
      ("mustPreserveLineage", `Bool metadata.attribution.must_preserve_lineage);
    ]);
    ("consents", `List consents_json);
  ] in

  let fields = match metadata.work_identifier with
    | Some id -> ("workIdentifier", `String id) :: fields
    | None -> fields
  in

  `Assoc fields

(** Serialise to JSON string *)
let serialise_string metadata =
  Yojson.Safe.to_string (serialise metadata)

(** Pretty-print serialise to JSON string *)
let serialise_string_pretty metadata =
  Yojson.Safe.pretty_to_string (serialise metadata)
