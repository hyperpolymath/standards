# SPDX-License-Identifier: AGPL-3.0-or-later
# pedigree.ncl - The Alpha Breed Specification for .k9 SVC
#
# Every .k9 component inherits from this pedigree to define its
# identity, security level, and deployment contracts.

let SecurityLevel = [| 'Kennel, 'Yard, 'Hunt |] in
let Architecture = [| 'Linux, 'Minix, 'MacOS, 'Android, 'PC, 'ASIC, 'Unknown |] in

{
  # ─────────────────────────────────────────────────────────────
  # L1: The Snout - Identity
  # ─────────────────────────────────────────────────────────────
  Metadata = {
    # Component name (lowercase, hyphenated)
    name | String,
    # SemVer with stability suffix
    version | String | default = "1.0.0-alpha",
    # MIME type identifier
    breed | String | default = "application/vnd.k9+nickel",
    # Binary signature for kernel identification
    magic_number | String | default = "K9!",
    # Human-readable component purpose
    description | String | optional,
  },

  # ─────────────────────────────────────────────────────────────
  # L2: The Scent - Target Environment
  # ─────────────────────────────────────────────────────────────
  Target = {
    # Primary deployment architecture
    os | Architecture,
    # Constrained environment (< 1GB RAM, no Podman)
    is_edge | Bool | default = false,
    # Whether deployment needs container runtime
    requires_podman | Bool | default = true,
    # Minimum memory requirement in MB
    min_memory_mb | Number | default = 256,
  },

  # ─────────────────────────────────────────────────────────────
  # L3: The Leash - Security Levels
  # ─────────────────────────────────────────────────────────────
  #
  # 'Kennel: Pure data. No execution. Safe to open anywhere.
  # 'Yard:   Nickel evaluation only. No I/O side effects.
  # 'Hunt:   Full triad execution. Requires handshake.
  #
  Security = {
    # Execution permission tier
    trust_level | SecurityLevel | default = 'Yard,
    # Can fetch external resources
    allow_network | Bool | default = false,
    # Can modify host filesystem
    allow_filesystem_write | Bool | default = false,
    # Can spawn child processes
    allow_subprocess | Bool | default = false,
    # Cryptographic handshake for Hunt level
    signature | String | optional,
  },

  # ─────────────────────────────────────────────────────────────
  # L4: The Gut - Self-Validation (Dogfooding)
  # ─────────────────────────────────────────────────────────────
  Validation = {
    # SHA256 of component payload
    checksum | String,
    # Version of this pedigree schema
    pedigree_version | String | default = "1.0.0",
    # Contract: Hunt requires valid signature
    # Computed: true if Hunt level is safe to execute
    hunt_authorized | Bool | default = false,
  },

  # ─────────────────────────────────────────────────────────────
  # L5: The Muscle - Deployment Recipes
  # ─────────────────────────────────────────────────────────────
  Recipes = {
    # Installation recipe
    install | String | default = "just install",
    # Self-validation recipe
    validate | String | default = "just validate",
    # Deployment recipe (Podman or native)
    deploy | String | default = "just deploy",
    # Version migration recipe (zero-deprecation)
    migrate | String | default = "just migrate",
  },

  # ─────────────────────────────────────────────────────────────
  # Contract: A valid .k9 pedigree
  # ─────────────────────────────────────────────────────────────
  K9Pedigree = {
    metadata | Metadata,
    target | Target,
    security | Security,
    validation | Validation,
    recipes | Recipes,
  },
}
