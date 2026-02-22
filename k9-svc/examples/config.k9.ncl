# SPDX-License-Identifier: AGPL-3.0-or-later
# config.k9.ncl - Example Yard-level component (Nickel validation)
#
# Security Level: 'Yard (Nickel evaluation, no I/O)
#
# This component demonstrates typed configuration with contracts.
# Nickel ensures the config is valid before any deployment.

let pedigree = import "../pedigree.ncl" in

# The component's pedigree (self-description)
let component_pedigree = {
  metadata = {
    name = "example-config",
    version = "1.0.0",
    breed = "application/vnd.k9+nickel",
    magic_number = "K9!",
    description = "Example configuration component with type contracts",
  },
  target = {
    os = 'Linux,
    is_edge = false,
    requires_podman = false,
    min_memory_mb = 64,
  },
  security = {
    trust_level = 'Yard,
    allow_network = false,
    allow_filesystem_write = false,
    allow_subprocess = false,
  },
  validation = {
    checksum = "sha256:placeholder",
    pedigree_version = "1.0.0",
    hunt_authorized = false,
  },
  recipes = {
    install = "echo 'Config component - no install needed'",
    validate = "nickel typecheck config.k9.ncl",
    deploy = "echo 'Config component - use as import'",
    migrate = "echo 'Config component - update version field'",
  },
} in

# The actual configuration this component provides
let config = {
  # Database connection settings
  database | {
    host | String | default = "localhost",
    port | std.contract.from_predicate (fun p => p > 0 && p < 65536) | default = 5432,
    name | String,
  } = {
    host = "db.example.com",
    port = 5432,
    name = "k9_example",
  },

  # Cache settings
  cache | {
    enabled | Bool | default = true,
    ttl_seconds | std.contract.from_predicate (fun t => t > 0) | default = 300,
    max_entries | Number | default = 1000,
  } = {
    enabled = true,
    ttl_seconds = 600,
    max_entries = 5000,
  },

  # Logging configuration
  logging | {
    level | [| 'debug, 'info, 'warn, 'error |] | default = 'info,
    format | [| 'json, 'text, 'structured |] | default = 'json,
    output | String | default = "stdout",
  } = {
    level = 'info,
    format = 'json,
    output = "stdout",
  },

  # Feature flags
  features | {
    enable_metrics | Bool | default = false,
    enable_tracing | Bool | default = false,
    experimental | Bool | default = false,
  } = {
    enable_metrics = true,
    enable_tracing = true,
    experimental = false,
  },
} in

# Export both pedigree and config
{
  pedigree = component_pedigree,
  config = config,

  # Helper: Get config as JSON for external tools
  as_json = std.serialize 'Json config,
}
