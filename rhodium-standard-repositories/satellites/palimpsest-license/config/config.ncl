# Palimpsest License - Nickel Configuration Schema
# SPDX-License-Identifier: Palimpsest-0.4 OR MIT
#
# Nickel is a configuration language for generating validated configs.
# This file defines the schema for Palimpsest configuration across tools.
#
# Usage:
#   nickel export config.ncl > config.json
#   nickel typecheck config.ncl

# =============================================================================
# Type Definitions
# =============================================================================

let ConsentStatus = std.enum.TagOrString in
let TraumaCategory = std.enum.TagOrString in

let ConsentType = {
  ai_training : [| 'permitted, 'prohibited, 'conditional, 'unknown |],
  ni_systems : [| 'permitted, 'prohibited, 'explicit_only, 'unknown |],
  commercial : [| 'permitted, 'prohibited, 'conditional |],
  research : [| 'permitted, 'prohibited, 'conditional |],
} in

let EmotionalLineage = {
  present : Bool,
  context | optional : String,
  trauma_markers | optional : Array String,
  cultural_origin | optional : String,
  ancestors | optional : Array String,
  descendants | optional : Array String,
} in

let WorkMetadata = {
  id : String,
  title : String,
  creator : String,
  version : String,
  created : String,
  license_version : String,
  consent : ConsentType,
  emotional_lineage | optional : EmotionalLineage,
  consent_registry_url | optional : String,
} in

let ProjectConfig = {
  name : String,
  version : String,
  default_consent : ConsentType,
  metadata_format : [| 'json_ld, 'xml, 'yaml |],
  citation_formats : Array String,
  consent_registry : {
    enabled : Bool,
    url | optional : String,
    api_key | optional : String,
  },
  build : {
    ocaml : {
      enabled : Bool,
      melange : Bool,
      ocanren : Bool,
    },
    container : {
      enabled : Bool,
      base_image : String,
    },
    packages : {
      guix : Bool,
      nix : Bool,
      void : Bool,
    },
  },
} in

# =============================================================================
# Default Configuration
# =============================================================================

let default_config : ProjectConfig = {
  name = "palimpsest",
  version = "0.4.0",

  default_consent = {
    ai_training = 'prohibited,
    ni_systems = 'explicit_only,
    commercial = 'conditional,
    research = 'conditional,
  },

  metadata_format = 'json_ld,

  citation_formats = ["oscola", "ieee", "mla", "harvard", "bibtex", "ris"],

  consent_registry = {
    enabled = true,
    url = "https://consent.palimpsestlicense.org",
  },

  build = {
    ocaml = {
      enabled = true,
      melange = true,
      ocanren = true,
    },
    container = {
      enabled = true,
      base_image = "cgr.dev/chainguard/wolfi-base:latest",
    },
    packages = {
      guix = true,
      nix = true,
      void = false,
    },
  },
} in

# =============================================================================
# Environment-Specific Overrides
# =============================================================================

let dev_overrides = {
  consent_registry = {
    enabled = true,
    url = "http://localhost:8080",
  },
} in

let prod_overrides = {
  consent_registry = {
    enabled = true,
    url = "https://consent.palimpsestlicense.org",
    # api_key loaded from environment
  },
} in

let ci_overrides = {
  build = {
    container = {
      enabled = true,
      base_image = "cgr.dev/chainguard/wolfi-base:latest",
    },
  },
} in

# =============================================================================
# Validation Functions
# =============================================================================

let validate_consent | ConsentType -> Bool = fun consent =>
  consent.ai_training != 'unknown
  && consent.ni_systems != 'unknown
in

let validate_work | WorkMetadata -> Bool = fun work =>
  std.string.length work.id > 0
  && std.string.length work.title > 0
  && std.string.length work.creator > 0
  && validate_consent work.consent
in

# =============================================================================
# Helper Functions
# =============================================================================

# Generate JSON-LD metadata block
let to_json_ld | WorkMetadata -> _ = fun work => {
  "@context" = "https://palimpsestlicense.org/context",
  "@type" = "CreativeWork",
  "@id" = work.id,
  "name" = work.title,
  "creator" = work.creator,
  "license" = "https://palimpsestlicense.org/v%{work.license_version}",
  "palimpsest:consent" = {
    "palimpsest:aiTraining" = std.enum.to_tag work.consent.ai_training,
    "palimpsest:niSystems" = std.enum.to_tag work.consent.ni_systems,
  },
} in

# Generate OSCOLA citation
let to_oscola | WorkMetadata -> String = fun work =>
  "%{work.creator}, '%{work.title}' (Palimpsest License v%{work.license_version}, %{work.created})"
in

# =============================================================================
# Example Work Metadata
# =============================================================================

let example_work : WorkMetadata = {
  id = "urn:palimpsest:work:example-001",
  title = "Example Protected Work",
  creator = "Jane Doe",
  version = "1.0.0",
  created = "2025-12-08",
  license_version = "0.4",
  consent = {
    ai_training = 'prohibited,
    ni_systems = 'explicit_only,
    commercial = 'conditional,
    research = 'permitted,
  },
  emotional_lineage = {
    present = true,
    context = "Protest art from 2020 movement",
    trauma_markers = ["collective", "ongoing"],
    cultural_origin = "Social justice movement",
  },
  consent_registry_url = "https://consent.palimpsestlicense.org/verify/example-001",
} in

# =============================================================================
# Exports
# =============================================================================

{
  # Main configuration
  config = default_config,

  # Environment configs
  environments = {
    development = std.record.merge default_config dev_overrides,
    production = std.record.merge default_config prod_overrides,
    ci = std.record.merge default_config ci_overrides,
  },

  # Type schemas (for documentation/validation)
  schemas = {
    ConsentType = ConsentType,
    EmotionalLineage = EmotionalLineage,
    WorkMetadata = WorkMetadata,
    ProjectConfig = ProjectConfig,
  },

  # Helper functions
  helpers = {
    to_json_ld = to_json_ld,
    to_oscola = to_oscola,
    validate_consent = validate_consent,
    validate_work = validate_work,
  },

  # Example data
  example = {
    work = example_work,
    json_ld = to_json_ld example_work,
    citation = to_oscola example_work,
    valid = validate_work example_work,
  },
}
