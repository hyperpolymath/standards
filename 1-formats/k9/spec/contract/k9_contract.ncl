# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# k9_contract.ncl — THE NORMATIVE K9 COMPONENT CONTRACT (v1.0.0).
#
# Ruling D173 (standards#1058, 2026-09-30): "K9 needs a Nickel contract, not an
# ABNF." This file is that contract. It is normative; the prose that governs it
# is ../K9-CONTRACT-SPEC.adoc, and where the two disagree the prose wins and
# this file is a bug.
#
# WHAT THIS FILE IS
#   The closed, versioned, machine-checkable shape of a K9 component's
#   `pedigree` block, plus the decision functions a host must consult before
#   it lets a component do anything:
#
#     leash_levels / is_leash      §7  the leash taxonomy as a CLOSED set
#     required_capabilities        §8  what a security flag costs you
#     granted_ok / denied          §8  default-deny capability arithmetic
#     authorize_hunt               §9  all five preconditions, always
#     signature_verdict            §10 presence is NOT verification
#
# WHAT THIS FILE IS NOT
#   * It is not a grammar. K9 has no syntax of its own: the body of a
#     `.k9.ncl` file is a Nickel term and Nickel's grammar is normative by
#     reference (K9-CONTRACT-SPEC §3.5). Re-specifying it here, in ABNF or
#     otherwise, would fork a language we do not own.
#   * It is not a validator. A contract states what holds; the canonical
#     validator (../../tools/k9-validate.sh) decides whether a check could be
#     run at all, and reports a check it could not run as SKIPPED rather than
#     as a pass.
#   * It does not verify signatures. Nickel has no cryptographic primitive,
#     so nothing in this file can produce `signature_check = 'Passed`. See
#     §10: an implementation that has not run a verifier MUST report 'Not_Run,
#     and 'Not_Run can never authorise Hunt.
#
# NAMING HAZARD — carried from SPEC.adoc, unchanged:
#   K9's execution triad is  must / just / nickel.
#   The contractiles family is  must / trust / dust / intend.
#   `must` appears in both and means different things. Never conflate them.
#   Nothing in this file names a contractile verb; a contractile component is
#   an ordinary K9 component whose `component_type` begins "contractile:".
#
# API SURFACE DISCIPLINE
#   Every std function used below is one already exercised by a `.ncl` file
#   this repository typechecks in CI (capabilities.ncl, leash.ncl,
#   pedigree.ncl): std.string.length, std.string.substring, std.string.split,
#   std.string.join, std.array.length, std.array.any, std.array.filter,
#   std.record.has_field, std.contract.from_predicate. A normative artefact
#   must not be the first thing in the estate to touch an unexercised corner
#   of a dependency's standard library.
#
# ─────────────────────────────────────────────────────────────────────────
# Type aliases (top-level `let` — the idiom pedigree.ncl and leash.ncl use)
# ─────────────────────────────────────────────────────────────────────────

# §7 — the leash taxonomy. CLOSED: three members, and adding a fourth is a
# MAJOR change to this contract, not an extension.
let SecurityLevel = [| 'Kennel, 'Yard, 'Hunt |] in

# §10 — the result of an attempted cryptographic check. 'Not_Run is the honest
# default and it is load-bearing: it is the value a validator reports when no
# verifier was available, and it must never be silently upgraded.
let SignatureCheck = [| 'Not_Run, 'Passed, 'Failed |] in

# §10 — what a host may infer from "does the file carry a signature block"
# (lexical) combined with "did a verifier accept it" (cryptographic).
let SignatureVerdict = [| 'Absent, 'Present_Unverified, 'Verified, 'Rejected |] in

# §8.1 — the reserved extension namespace prefix.
let ExtensionPrefix = "x-" in

# ─────────────────────────────────────────────────────────────────────────
# §5.2 / §8.1 — name shapes, using nothing beyond length and substring
# ─────────────────────────────────────────────────────────────────────────
let is_digit = fun c =>
  c == "0"
  || c == "1"
  || c == "2"
  || c == "3"
  || c == "4"
  || c == "5"
  || c == "6"
  || c == "7"
  || c == "8"
  || c == "9"
in

# A numeric dot-triple whose MAJOR segment equals `major`. Walks the string
# once. Rejects: non-digits, a leading or trailing dot, an empty segment, a
# leading zero in any segment ("1.00.0"), and the wrong number of segments.
#
# `prev_dot` starts TRUE so a leading "." is refused by the same branch that
# refuses ".."; `seg_len` must end > 0 so a trailing "." is refused too.
let is_semver_of = fun major v =>
  let want = major ++ "." in
  let rec go = fun i dots seg_len first_zero s =>
    if i >= std.string.length s then
      dots == 2 && seg_len > 0
    else
      # `substring start end str`, NOT (start, length, str): 1.18.0's
      # std.ncl documents `substring start end str` as the slice from `start`
      # (included) to `end` (excluded), so the single character at `i` is
      # `substring i (i + 1) s`. Written as `substring i 1 s` it returned "1"
      # at i=0 by coincidence and "" at every later index, so the walk fell
      # out at the second character and `is_semver_of` rejected "1.0.0" —
      # every positive control with it. Verified against core/stdlib/std.ncl.
      let c = std.string.substring i (i + 1) s in
      if c == "." then
        seg_len > 0 && go (i + 1) (dots + 1) 0 false s
      else if is_digit c then
        (first_zero == false || seg_len == 0)
        && go (i + 1) dots (seg_len + 1) (c == "0") s
      else
        false
  in
  # A dot-triple is at least `want` plus "N.N": three more characters.
  std.string.length v >= (std.string.length want + 3)
  && std.string.substring 0 (std.string.length want) v == want
  && go 0 0 0 false v
in

# ─────────────────────────────────────────────────────────────────────────
# The contract record. Nickel records are recursive, so fields below refer to
# their siblings by name; the type aliases above stay top-level so no field
# ever shadows the binding it is defined in terms of.
# ─────────────────────────────────────────────────────────────────────────
{
  # ── §5 Identity and versioning ───────────────────────────────────────
  #
  # `contract_version` is the version of THIS file. `schema_major` is the
  # major segment a conforming component's `pedigree.schema_version` must
  # carry to be readable by this contract. They are separate numbers on
  # purpose: this contract can be revised (1.0.0 -> 1.0.1) without changing
  # what a component must look like.
  contract_name = "k9-svc-component",
  contract_version = "1.0.0",
  schema_major = "1",

  # §4 — the dialects a K9 envelope may carry. A file whose body is not one of
  # these is not a K9 file, whatever its suffix says.
  dialect = {
    # `K9!` envelope + Nickel body declaring a `pedigree`. In scope here.
    component = "k9-svc-component",
    # No `K9!` envelope, Nickel body, no top-level `pedigree`. Imported, never
    # executed. In scope for §11 only.
    library = "k9-svc-library",
    # `K9!` envelope + YAML body. Governed by
    # 2-protocols/k9-coordination/spec/abnf/coordination-k9-grammar_v1.1.abnf,
    # NOT by this contract. Named here so a validator can route it instead of
    # misreporting it as a broken component.
    coordination = "k9-coordination",
  },

  # ── §7 Leash taxonomy — CLOSED SET ───────────────────────────────────
  #
  # Machine-checkable enumeration of the three levels in ascending order of
  # what they permit. This list IS the closed set the prose promises, and it
  # lives in the same file as the `SecurityLevel` enum it mirrors so the two
  # cannot drift silently; the conformance suite asserts they agree.
  leash_levels = ['Kennel, 'Yard, 'Hunt],

  # Total order over the closed set. Used for the monotonicity rule (§7.4): a
  # host may enforce a level STRICTER than the component declares, never laxer.
  level_rank = fun l =>
    if l == 'Kennel then
      0
    else if l == 'Yard then
      1
    else
      2,

  is_leash = fun v =>
    std.array.length (std.array.filter (fun l => l == v) leash_levels) == 1,

  # The leash a component DECLARES. Fail-closed: a pedigree with no readable
  # `security.leash` ranks as 'Kennel here, and is rejected outright by the
  # Pedigree contract below. This helper exists so a host handed a partial
  # pedigree cannot crash on it.
  leash_of = fun pedigree =>
    if std.record.has_field "security" pedigree
    && std.record.has_field "leash" pedigree.security then
      pedigree.security.leash
    else
      'Kennel,

  # ── §8 Capability model — DEFAULT-DENY ───────────────────────────────
  #
  # §8.1 CLOSED CORE SET. Exhaustive for the core namespace. Adding or
  # removing a name here is a MAJOR change to this contract.
  core_capabilities = [
    "fs.read",
    "fs.write",
    "net.fetch",
    "process.spawn",
    "container.run",
    "secret.read",
    "deploy.apply",
    "rollback.apply",
  ],

  # §8.1 RESERVED EXTENSION NAMESPACE. Non-core names MUST be "x-<vendor>.<p>"
  # so they can never collide with a future core name.
  extension_prefix = ExtensionPrefix,

  is_core = fun name => std.array.any (fun c => c == name) core_capabilities,

  # A well-formed extension: starts with the prefix and has at least one "."
  # after the vendor segment, i.e. "x-vendor.something", never bare "x-vendor".
  is_extension = fun name =>
    std.string.length name > std.string.length ExtensionPrefix
    && std.string.substring 0 (std.string.length ExtensionPrefix) name == ExtensionPrefix
    && std.array.length (std.string.split "." name) >= 2,

  Capability =
    std.contract.from_predicate (fun name =>
      is_core name || is_extension name
    ),

  # An explicit allow-list. DEFAULT-DENY is expressed by the empty grant: it
  # permits nothing, and "nothing" is the correct answer for an absent grant.
  CapabilityGrant = Array Capability,

  default_grant = [],

  permits = fun grant name => std.array.any (fun g => g == name) grant,

  denied = fun grant requested =>
    std.array.filter (fun r => !(permits grant r)) requested,

  granted_ok = fun grant requested =>
    std.array.length (denied grant requested) == 0,

  # §8.4 DERIVED REQUIREMENTS — the rule that makes the grant load-bearing.
  #
  # A security flag is a REQUEST for a capability, and a request the grant
  # does not cover is a contradiction inside the pedigree, not a preference.
  # `allow_network = true` with an empty grant is therefore INVALID, not
  # merely unwise: the component asks for the network while granting itself
  # nothing. This STRENGTHENS the pre-#1058 state, where the flags and the
  # grant were two unrelated opinions in the same block.
  required_capabilities = fun security =>
    (
      if std.record.has_field "allow_network" security
      && security.allow_network then
        ["net.fetch"]
      else
        []
    )
    @ (
      if std.record.has_field "allow_filesystem_write" security
      && security.allow_filesystem_write then
        ["fs.write"]
      else
        []
    )
    @ (
      if std.record.has_field "allow_subprocess" security
      && security.allow_subprocess then
        ["process.spawn"]
      else
        []
    ),

  # What the grant fails to cover. Empty iff the pedigree is self-consistent.
  grant_deficit = fun grant security =>
    denied grant (required_capabilities security),

  # ── §9 Hunt preconditions — ALL FIVE, ALWAYS ─────────────────────────
  #
  # Owner directive 2026-06-03, restated and NOT RELAXED by this contract:
  # Hunt requires all five, always. There is no configurable subset, and a
  # valid signature alone is not sufficient. Reproduced from leash.ncl
  # verbatim so the two cannot drift; the conformance suite asserts equality.
  hunt_preconditions = [
    "signature",
    "policy",
    "sandbox",
    "dry_run",
    "capability_grant",
  ],

  # The host supplies one Bool of EVIDENCE per precondition. Evidence is
  # runtime fact, not a static component field — which is why `check_level`
  # in leash.ncl can never authorise Hunt on its own.
  HuntEvidence = {
    signature | Bool,
    policy | Bool,
    sandbox | Bool,
    dry_run | Bool,
    capability_grant | Bool,
  },

  # §9.2 The gate. Identical semantics to leash.ncl `authorize_hunt`, and
  # deliberately so: a host implementing this file and a host implementing
  # leash.ncl must reach the same verdict on the same evidence.
  authorize_hunt = fun evidence =>
    let unmet =
      (if evidence.signature then [] else ["signature"])
      @ (if evidence.policy then [] else ["policy"])
      @ (if evidence.sandbox then [] else ["sandbox"])
      @ (if evidence.dry_run then [] else ["dry_run"])
      @ (if evidence.capability_grant then [] else ["capability_grant"])
    in
    if std.array.length unmet == 0 then
      {
        permitted = true,
        enforced_level = 'Hunt,
        unmet_preconditions = [],
      }
    else
      {
        permitted = false,
        enforced_level = 'Yard,
        reason =
          "Hunt requires all five preconditions; unmet: "
          ++ std.string.join ", " unmet,
        unmet_preconditions = unmet,
      },

  # ── §10 Signature semantics — PRESENCE IS NOT VERIFICATION ───────────
  #
  # A `signature` block in a pedigree is a CLAIM. Reading it proves the file
  # says it was signed. It does not prove the file was signed, and it does not
  # prove the signature covers the bytes about to be executed.
  signature_checks = ['Not_Run, 'Passed, 'Failed],
  signature_verdicts = ['Absent, 'Present_Unverified, 'Verified, 'Rejected],

  # §10.2 — the only mapping a conforming implementation may make.
  #   no block                 -> 'Absent             (nothing was claimed)
  #   block, verifier passed   -> 'Verified           (cryptographic fact)
  #   block, verifier rejected -> 'Rejected           (cryptographic fact)
  #   block, no verifier ran   -> 'Present_Unverified (lexical fact ONLY)
  #
  # Note what is absent from that table: there is no input that turns
  # 'Not_Run into 'Verified. An implementation cannot infer verification from
  # presence, and a validator that reports 'Verified without having run a
  # verifier is non-conforming (§10.3).
  signature_verdict = fun has_block check =>
    if has_block == false then
      'Absent
    else if check == 'Passed then
      'Verified
    else if check == 'Failed then
      'Rejected
    else
      'Present_Unverified,

  # §10.4 — the Hunt `signature` precondition is satisfiable by 'Verified and
  # by nothing else. Presence yields 'Present_Unverified, which is false here.
  # This is the line that stops a lexical check from becoming an execution
  # licence, and it is the reason this contract cannot be replaced by a grep.
  hunt_signature_evidence = fun verdict => verdict == 'Verified,

  # The signature block a component MAY carry. Every field but `algorithm` is
  # optional at the CONTRACT level, because §10.1 makes presence a per-leash
  # rule rather than a universal one: 'Kennel and 'Yard components are
  # ordinarily unsigned, and that is correct rather than a defect.
  Signature = {
    algorithm | String | default = "Ed25519",
    public_key | String | optional,
    signature | String | optional,
    payload_hash | String | optional,
    signed_at | String | optional,
    key_id | String | default = "primary",
  },

  # ── §6 The component pedigree, v1 ────────────────────────────────────
  #
  # Field-for-field this is the shape the estate's `.k9.ncl` components
  # actually carry (`.machine_readable/svc/k9/template-*.k9.ncl.in`, the
  # trust-tier templates — `.in` because a template is not a component and is
  # never loaded; MIGRATION-1058 M3), with two normative tightenings:
  # `component_type` is REQUIRED (§6.2), and the security flags must be paid
  # for in the capability grant (§8.4).

  Metadata = {
    name | String,
    version | String | default = "0.0.0",
    description | String | optional,
    author | String | optional,
  },

  Security = {
    # §7 — closed set. An unknown tag is a contract violation, not a warning.
    leash | SecurityLevel,
    trust_level | String | default = "unset",
    allow_network | Bool | default = false,
    allow_filesystem_write | Bool | default = false,
    allow_subprocess | Bool | default = false,
    # §10.1 — REQUIRED to be true when leash == 'Hunt. The validator enforces
    # the implication; this contract cannot, because it is a cross-field rule.
    signature_required | Bool | default = false,
    # §8.3 — the explicit grant. Default-deny: absent means empty.
    capabilities | CapabilityGrant | default = [],
  },

  Pedigree = {
    # §5.2 — a numeric dot-triple whose major segment equals `schema_major`.
    # A component from another major is not "older"; it is unreadable, and
    # must be refused rather than guessed at (§5.4).
    schema_version | std.contract.from_predicate (is_semver_of schema_major),
    # §6.2 — REQUIRED: what this component IS. A template placeholder is not
    # a value; the canonical validator rejects "TODO" here at L1.
    component_type | String,
    security | Security,
    metadata | Metadata,
    # §6.5 — advisory to a reader, mandatory to declare. A 'Hunt component
    # with an empty side_effects list claims full system access and describes
    # none of it, which the validator rejects at L1.
    warnings | Array String | default = [],
    side_effects | Array String | default = [],
    # §10.1 — the signature claim. Presence is per-leash; see Signature above.
    signature | Signature | optional,
  },

  # The whole component: the pedigree plus the optional Nickel payload.
  # `config`, `recipes` and `validation` stay open on purpose — they are the
  # component's own business — but §6.7 makes one thing non-negotiable: a
  # non-empty `recipes` block is an execution surface and therefore forces
  # 'Hunt. The validator enforces that; this contract only shapes the blocks.
  Component = {
    pedigree | Pedigree,
    # An open record: any fields, any values. These three blocks are the
    # component's own business and the contract deliberately does not shape
    # them — §6.7's `recipes`-forces-'Hunt rule is enforced by the validator
    # over the extracted facts, not by a type here.
    #
    # Two names were tried and rejected by Nickel before this one: `Record`
    # and then `Any`. Neither exists; the dynamic type is `Dyn`. Both failures
    # were invisible locally — no nickel binary is obtainable in the sandbox
    # that wrote this file — and both made every L2 verdict downstream void,
    # because a contract that does not typecheck cannot judge anything. That
    # is why k9-contractile.yml now typechecks this file as its own named step
    # before the fixtures run.
    config | { _ : Dyn } | optional,
    recipes | { _ : Dyn } | optional,
    validation | { _ : Dyn } | optional,
  },

  # ── §11 Library dialect ──────────────────────────────────────────────
  #
  # A library is imported; it is never executed as a component and never
  # carries an envelope. The contract it must satisfy is the negative one:
  # no pedigree, no leash claim, no execution licence of any kind.
  is_library = fun lib =>
    !(std.record.has_field "pedigree" lib)
    && !(std.record.has_field "leash" lib),
}
