K9!
# SPDX-License-Identifier: MPL-2.0
# extension-fields.k9.ncl — positive control: §5.5's "Unknown fields anywhere
# | PERMITTED".
#
# WHY THIS CONTROL EXISTS
#   Until it did, not one positive control left the contract's declared field
#   set, so a whole clause of §5.5 was untested. Nickel's record contracts are
#   CLOSED by default — `core/tests/integration/inputs/contracts/
#   record_contract_extra_field.ncl` is the blame control, `EvalError::
#   BlameError` with "extra field" — so `Component`/`Pedigree`/`Security`/
#   `Metadata`/`Signature` in spec/contract/k9_contract.ncl each rejected the
#   extension fields the prose permitted, and nothing in CI noticed. The six
#   `.machine_readable/contractiles/*/*.k9.ncl` components are the live case of
#   that disagreement (MIGRATION-1058 M1), and the contract's own §5.5 note
#   explains the fix. This control fails at L2 the moment any of those records
#   closes again.
#
# WHAT IT DELIBERATELY KEEPS DOING
#   An extension field is DATA, never a grant: `allow_filesystem_write` stays
#   false and `capabilities` stays empty, even though
#   `allow_filesystem_write_conditional` (a contractile extension field, §5.5)
#   says a write path exists. §5.5's last clause is that no reader may consult
#   an unknown field to relax §8 or §9 — so nothing here is derivable from it,
#   and §7.3 caps 'Yard at evaluation in any case.
{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "contractile:fixture",
    semantics = "extension-field-tolerance",
    contractile_verb = "fixture",
    tier = 'Yard,
    authority = 'advisory,
    security = {
      leash = 'Yard,
      trust_level = "contract-eval-only",
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
      capabilities = [],
      probe_scope = 'read_only,
      probe_kinds_allowed = ['file_existence, 'pattern_presence],
      probe_kinds_denied = ['network_call, 'filesystem_mutation],
      allow_filesystem_write_conditional = false,
    },
    metadata = {
      name = "extension-fields",
      version = "1.0.0",
      description = "Positive control: extension fields at pedigree, security and metadata level.",
      paired_xfile = "Examplefile.a2ml",
      paired_runner = "example.ncl",
    },
    warnings = [
      "Extension fields are permitted and are not consulted for permissions.",
    ],
    side_effects = [],
    # Declaration-only fields, no definitions: `not_exported` says what is true
    # (`nickel export` would otherwise abort with "missing field definition").
    # §5.5 permits the block; this asserts the export path too, which is why
    # the control must be exported and not merely typechecked.
    variance_schema | not_exported = {
      entry_id | String,
      reason | String,
    },
  },

  # Top level too: §5.5's clause is "anywhere", and these are the names the
  # axel config files and the contractiles actually carry.
  execution = {
    triggers = ['session_close, 'on_demand],
  },

  failure_mode_defenses = [
    "example entry, present only to occupy an undeclared top-level field",
  ],
}
