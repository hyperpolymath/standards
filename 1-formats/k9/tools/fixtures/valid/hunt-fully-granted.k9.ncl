K9!
# SPDX-License-Identifier: MPL-2.0
# hunt-fully-granted.k9.ncl — positive control: a conforming Hunt component.
#
# Every §8.4-derived capability is granted, §10.1's signature block is
# PRESENT, and side_effects are declared. Note what this file still does NOT
# prove: the signature block below is a claim. K9-C001 will report it
# 'Present_Unverified unless a verifier runs, and 'Present_Unverified does not
# authorise Hunt. Conformance and authorisation are different questions.
{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "deployment",
    security = {
      leash = 'Hunt,
      trust_level = "controlled-execution",
      allow_network = true,
      allow_filesystem_write = true,
      allow_subprocess = true,
      signature_required = true,
      capabilities = [
        "net.fetch",
        "fs.write",
        "process.spawn",
        "deploy.apply",
        "rollback.apply",
      ],
    },
    metadata = {
      name = "hunt-fully-granted",
      version = "1.0.0",
      description = "Reference Hunt component: every request is paid for in the grant.",
      author = "K9 conformance suite",
    },
    warnings = [
      "This component has network, filesystem and subprocess access.",
      "A signature block being present is not a signature being verified.",
      "Use dry-run first: ./must --dry-run run hunt-fully-granted.k9.ncl",
    ],
    side_effects = [
      "writes the deployment tree under /srv/k9-conformance",
      "spawns `just deploy` as a child process",
      "fetches the release manifest from releases.example.org",
    ],
    signature = {
      algorithm = "Ed25519",
      key_id = "conformance-fixture",
      payload_hash = "sha256:0000000000000000000000000000000000000000000000000000000000000000",
      signature = "FIXTURE-NOT-A-REAL-SIGNATURE",
    },
  },

  config = {
    target_dir | String = "/srv/k9-conformance",
    dry_run | Bool = true,
  },

  recipes = {
    default_recipe = { recipe = "deploy" },
    deploy = {
      description = "Apply the deployment.",
      commands = ["echo '[fixture] would deploy to %{config.target_dir}'"],
    },
  },
}
