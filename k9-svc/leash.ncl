# SPDX-License-Identifier: AGPL-3.0-or-later
# leash.ncl - Security Level Detection and Enforcement for K9 SVC
#
# The Leash system controls what a .k9 component can do:
#   'Kennel - Pure data, no execution, safe anywhere
#   'Yard   - Nickel evaluation only, no I/O side effects
#   'Hunt   - Full triad execution, requires cryptographic handshake

let SecurityLevel = [| 'Kennel, 'Yard, 'Hunt |] in

{
  # ─────────────────────────────────────────────────────────────
  # Security Level Definitions
  # ─────────────────────────────────────────────────────────────

  levels = {
    # 'Kennel: Pure data mode
    # - No code execution whatsoever
    # - Safe to parse/display anywhere
    # - Think: JSON, YAML (without anchors)
    kennel = {
      name = "Kennel",
      symbol = "'Kennel",
      description = "Pure data. No execution. Safe to open anywhere.",
      allows = {
        nickel_eval = false,
        just_recipes = false,
        must_shim = false,
        network = false,
        filesystem_write = false,
        subprocess = false,
      },
      risk = "none",
      requires_signature = false,
    },

    # 'Yard: Nickel evaluation only
    # - Can evaluate Nickel expressions
    # - No I/O side effects permitted
    # - Think: CUE, Dhall, pure functional
    yard = {
      name = "Yard",
      symbol = "'Yard",
      description = "Nickel evaluation only. No I/O side effects.",
      allows = {
        nickel_eval = true,
        just_recipes = false,
        must_shim = false,
        network = false,
        filesystem_write = false,
        subprocess = false,
      },
      risk = "low",
      requires_signature = false,
    },

    # 'Hunt: Full triad execution
    # - Complete must-just-nickel triad active
    # - Can run arbitrary Just recipes
    # - REQUIRES cryptographic handshake
    hunt = {
      name = "Hunt",
      symbol = "'Hunt",
      description = "Full triad execution. Requires cryptographic handshake.",
      allows = {
        nickel_eval = true,
        just_recipes = true,
        must_shim = true,
        network = true,
        filesystem_write = true,
        subprocess = true,
      },
      risk = "high",
      requires_signature = true,
    },
  },

  # ─────────────────────────────────────────────────────────────
  # Security Contracts
  # ─────────────────────────────────────────────────────────────

  # Contract: Valid signature for Hunt level
  SignatureContract = {
    # Ed25519 public key (base64)
    public_key | String,
    # Signature over component hash (base64)
    signature | String,
    # SHA256 hash of the component payload
    payload_hash | String,
    # Timestamp of signing (ISO 8601)
    signed_at | String,
    # Key identifier for rotation support
    key_id | String | default = "primary",
  },

  # Contract: Leash enforcement result
  LeashResult = {
    # Whether the requested level is permitted
    permitted | Bool,
    # The actual level that will be enforced
    enforced_level | SecurityLevel,
    # Reason for any downgrade
    reason | String | optional,
    # Signature validation result (for Hunt)
    signature_valid | Bool | optional,
  },

  # ─────────────────────────────────────────────────────────────
  # Detection Logic
  # ─────────────────────────────────────────────────────────────

  # Detect required security level from component content
  detect_level = fun component =>
    let has_recipes = std.record.has_field "recipes" component
      && component.recipes != {} in
    let has_network = std.record.has_field "security" component
      && std.record.has_field "allow_network" component.security
      && component.security.allow_network == true in
    let has_fs_write = std.record.has_field "security" component
      && std.record.has_field "allow_filesystem_write" component.security
      && component.security.allow_filesystem_write == true in

    if has_network || has_fs_write || has_recipes then
      'Hunt
    else if std.record.has_field "validation" component then
      'Yard
    else
      'Kennel,

  # Check if component can run at requested level
  check_level = fun component requested_level =>
    let detected = detect_level component in
    let level_order = fun l =>
      if l == 'Kennel then 0
      else if l == 'Yard then 1
      else 2
    in
    let detected_ord = level_order detected in
    let requested_ord = level_order requested_level in

    if requested_ord < detected_ord then
      # Requesting lower level than component needs
      {
        permitted = false,
        enforced_level = detected,
        reason = "Component requires higher security level",
      }
    else if requested_level == 'Hunt then
      # Hunt requires signature verification
      let has_sig = std.record.has_field "security" component
        && std.record.has_field "signature" component.security
        && component.security.signature != null in
      if has_sig then
        {
          permitted = true,
          enforced_level = 'Hunt,
          signature_valid = true,  # Actual verification done externally
        }
      else
        {
          permitted = false,
          enforced_level = 'Yard,
          reason = "Hunt level requires valid signature",
          signature_valid = false,
        }
    else
      {
        permitted = true,
        enforced_level = requested_level,
      },

  # ─────────────────────────────────────────────────────────────
  # Handshake Protocol (Hunt Level)
  # ─────────────────────────────────────────────────────────────

  # Generate challenge for Hunt authorization
  # In practice, this would be called by the host environment
  handshake = {
    # Algorithm used for signatures
    algorithm = "Ed25519",

    # Challenge format
    challenge_format = m%"
K9-HUNT-CHALLENGE-v1
Component: {name}
Hash: {payload_hash}
Timestamp: {timestamp}
Nonce: {nonce}
"%,

    # Response format
    response_format = m%"
K9-HUNT-RESPONSE-v1
Challenge-Hash: {challenge_hash}
Signature: {signature}
Key-ID: {key_id}
"%,

    # Trusted public keys (would be loaded from system keyring)
    # This is a placeholder - real implementation would use actual keys
    trusted_keys = [
      # Example: "k9-official" = "base64-encoded-ed25519-pubkey"
    ],
  },
}
