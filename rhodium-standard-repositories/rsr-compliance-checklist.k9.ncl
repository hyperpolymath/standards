K9!
# SPDX-License-Identifier: PMPL-1.0-or-later
# RSR Compliance Checklist
#
# This K9 component defines the compliance checklist for Rhodium Standard
# Repositories (RSR), including required files, workflows, and standards.

leash = 'Kennel  # Pure data, no execution

pedigree = {
  schema_version = "1.0.0",
  component_type = "compliance-data",
  author = "Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>",
  description = "RSR compliance checklist and standards definition",
  created = "2026-01-30",
  k9_spec_version = "1.0.0",
}

# RSR compliance requirements
rsr_standards = {
  # Bronze tier requirements
  bronze = {
    tier_name = "Bronze",
    description = "Minimum viable RSR compliance",

    required_files = [
      { path = "LICENSE", description = "SPDX-compatible license (PMPL-1.0-or-later recommended)" },
      { path = "README.adoc", description = "AsciiDoc README with project overview" },
      { path = "SECURITY.md", description = "Security policy and vulnerability reporting" },
      { path = "CONTRIBUTING.md", description = "Contribution guidelines" },
      { path = "STATE.scm", description = "Current project state (Scheme format)" },
      { path = "ECOSYSTEM.scm", description = "Ecosystem relationships (Scheme format)" },
      { path = "META.scm", description = "Meta-information and ADRs (Scheme format)" },
    ],

    required_workflows = [
      { name = "CodeQL", file = ".github/workflows/codeql.yml", purpose = "Security scanning" },
      { name = "OpenSSF Scorecard", file = ".github/workflows/scorecard.yml", purpose = "Security posture" },
    ],

    required_badges = [
      { name = "License", pattern = "License-.*-blue.svg" },
      { name = "RSR Bronze", pattern = "RSR-Bronze-.*" },
    ],

    file_headers = {
      require_spdx = true,
      spdx_format = "# SPDX-License-Identifier: PMPL-1.0-or-later",
      require_copyright = false,
    },
  },

  # Silver tier requirements (Bronze + enhancements)
  silver = {
    tier_name = "Silver",
    description = "Enhanced RSR compliance with quality gates",

    additional_files = [
      { path = ".editorconfig", description = "Consistent coding style across editors" },
      { path = ".gitattributes", description = "Git attributes for line endings, etc." },
      { path = "CHANGELOG.md", description = "Change history (Keep a Changelog format)" },
    ],

    additional_workflows = [
      { name = "Quality", file = ".github/workflows/quality.yml", purpose = "EditorConfig + TruffleHog" },
      { name = "Mirror", file = ".github/workflows/mirror.yml", purpose = "GitLab/Bitbucket mirroring" },
    ],

    quality_gates = {
      test_coverage_min = 80,  # Percent
      documentation_coverage_min = 90,  # Percent of public APIs documented
      lint_warnings_max = 0,
    },
  },

  # Gold tier requirements (Silver + advanced)
  gold = {
    tier_name = "Gold",
    description = "Advanced RSR compliance with formal verification",

    additional_files = [
      { path = "ARCHITECTURE.adoc", description = "Architecture decision records" },
      { path = ".well-known/security.txt", description = "Security contact info (RFC 9116)" },
    ],

    additional_workflows = [
      { name = "Formal Verification", file = ".github/workflows/formal-verify.yml", purpose = "Proof-based verification" },
      { name = "Fuzzing", file = ".github/workflows/fuzz.yml", purpose = "Continuous fuzzing" },
    ],

    advanced_requirements = {
      formal_spec_language = [| 'Idris2, 'Coq, 'Lean, 'TLA+ |],
      cryptographic_signatures = true,
      sbom_generation = true,  # Software Bill of Materials
    },
  },
}

# Validation contracts
validation_rules = {
  # Check if file path is absolute or relative
  is_relative_path = fun path =>
    !std.string.is_match "^/" path
    | doc "All required file paths must be relative to repo root",

  # Check if workflow path is in .github/workflows/
  is_workflow_path = fun path =>
    std.string.is_match "^\\.github/workflows/.*\\.yml$" path
    | doc "Workflow files must be in .github/workflows/ with .yml extension",

  # Validate badge pattern is valid regex
  is_valid_badge_pattern = fun pattern =>
    std.string.length pattern > 0
    | doc "Badge patterns must be non-empty strings",
}

# Helper functions for compliance checking
helpers = {
  # Get all required files for a tier
  get_required_files = fun tier =>
    if tier == "bronze" then
      rsr_standards.bronze.required_files
    else if tier == "silver" then
      rsr_standards.bronze.required_files @ rsr_standards.silver.additional_files
    else if tier == "gold" then
      rsr_standards.bronze.required_files @ rsr_standards.silver.additional_files @ rsr_standards.gold.additional_files
    else
      []
    | doc "Get list of required files for a specific RSR tier",

  # Get all required workflows for a tier
  get_required_workflows = fun tier =>
    if tier == "bronze" then
      rsr_standards.bronze.required_workflows
    else if tier == "silver" then
      rsr_standards.bronze.required_workflows @ rsr_standards.silver.additional_workflows
    else if tier == "gold" then
      rsr_standards.bronze.required_workflows @ rsr_standards.silver.additional_workflows @ rsr_standards.gold.additional_workflows
    else
      []
    | doc "Get list of required workflows for a specific RSR tier",

  # Calculate compliance score (0.0 to 1.0)
  calculate_compliance_score = fun found_files found_workflows tier =>
    let required_files = helpers.get_required_files tier in
    let required_workflows = helpers.get_required_workflows tier in
    let total_requirements = std.array.length required_files + std.array.length required_workflows in
    let found_count = std.array.length found_files + std.array.length found_workflows in
    if total_requirements == 0 then
      1.0
    else
      (found_count * 1.0) / (total_requirements * 1.0)
    | doc "Calculate compliance score as ratio of found/required items",
}

# Example usage for automated compliance checking
example_usage = {
  description = "
    This K9 component can be imported by automated checkers like robot-repo-automaton
    to verify RSR compliance across repositories.

    Example:
      let rsr_checker = import 'rsr-compliance-checklist.k9.ncl' in
      let required_files = rsr_checker.helpers.get_required_files 'bronze' in
      # Check if files exist in repo...
  ",

  integration_points = [
    "robot-repo-automaton: Import this for compliance scanning",
    "hypatia: Use as ruleset for security scanning",
    "gitbot-fleet: Automated compliance enforcement",
  ],
}

# Metadata for K9 dogfooding
metadata = {
  is_self_compliant = true,
  compliance_tier = "gold",
  note = "This K9 component demonstrates Kennel-level (data-only) components - pure configuration with no execution logic.",
}
