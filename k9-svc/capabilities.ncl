# SPDX-License-Identifier: AGPL-3.0-or-later
# capabilities.ncl - The K9 capability model.
#
# Closed CORE enumeration + reserved EXTENSION namespace, DEFAULT-DENY.
#
# A K9 component is granted NO capability unless it is explicitly listed in its
# pedigree `policy.capabilities` grant. Capabilities are checked by the leash
# (leash.ncl) before any Hunt-level action: an action that needs a capability
# not present in the grant is refused, even at Hunt level.

{
  # ─────────────────────────────────────────────────────────────
  # The closed CORE capability set
  # ─────────────────────────────────────────────────────────────
  #
  # This list is normative and exhaustive for the core namespace. Adding or
  # removing a core capability is a BREAKING change to the K9 spec (MAJOR bump).
  # Each name is "<resource>.<verb>".
  core = [
    "fs.read",         # read named filesystem paths
    "fs.write",        # write named filesystem paths
    "net.fetch",       # outbound network fetch to named hosts
    "process.spawn",   # spawn a child process
    "container.run",   # run a container image
    "secret.read",     # read a named secret
    "deploy.apply",    # apply a deployment
    "rollback.apply",  # apply a rollback
  ],

  # ─────────────────────────────────────────────────────────────
  # Reserved EXTENSION namespace
  # ─────────────────────────────────────────────────────────────
  #
  # Non-core capabilities MUST be namespaced "x-<vendor>.<path>" so they can
  # never collide with a future core name. Core names (no "x-" prefix) are
  # reserved to this specification. Example: "x-acme.gpu.alloc".
  extension_prefix = "x-",

  is_core = fun name => std.array.any (fun c => c == name) core,

  # A well-formed extension: starts with "x-", and has at least one "." after
  # the vendor segment (i.e. "x-vendor.something").
  is_extension = fun name =>
    std.string.length name > 2
    && std.string.substring 0 2 name == "x-"
    && std.array.length (std.string.split "." name) >= 2,

  # ─────────────────────────────────────────────────────────────
  # Contracts
  # ─────────────────────────────────────────────────────────────
  #
  # A single capability name is valid iff it is a core name or a well-formed
  # extension name.
  Capability = std.contract.from_predicate (fun name =>
    is_core name || is_extension name),

  # A grant is an explicit allow-list of capability names. DEFAULT-DENY is
  # expressed by the empty grant: it permits nothing.
  CapabilityGrant = Array Capability,

  default_grant = [],

  # ─────────────────────────────────────────────────────────────
  # Decision functions (used by leash.ncl and the receipt)
  # ─────────────────────────────────────────────────────────────
  #
  # permits: does `grant` allow `name`? default-deny — only listed names pass.
  permits = fun grant name => std.array.any (fun g => g == name) grant,

  # denied: the subset of `requested` capabilities NOT covered by `grant`.
  # An empty result means every requested capability was granted.
  denied = fun grant requested =>
    std.array.filter (fun r => !(permits grant r)) requested,

  # granted_ok: every requested capability is permitted by the grant.
  granted_ok = fun grant requested =>
    std.array.length (denied grant requested) == 0,
}
