# SPDX-License-Identifier: AGPL-3.0-or-later
# deploy.k9.ncl - Example Hunt-level component (full execution)
#
# Security Level: 'Hunt (requires cryptographic handshake)
#
# WARNING: This component can execute shell commands!
# It requires explicit authorization via the Leash system.
#
# This demonstrates a deployment component that:
# 1. Validates its own configuration
# 2. Checks target environment
# 3. Deploys to the appropriate target

let pedigree = import "../pedigree.ncl" in
let leash = import "../leash.ncl" in

# The component's pedigree (self-description)
let component_pedigree = {
  metadata = {
    name = "example-deploy",
    version = "1.0.0",
    breed = "application/vnd.k9+nickel",
    magic_number = "K9!",
    description = "Example deployment component (Hunt level)",
  },
  target = {
    os = 'Linux,
    is_edge = false,
    requires_podman = true,
    min_memory_mb = 512,
  },
  security = {
    trust_level = 'Hunt,
    allow_network = true,
    allow_filesystem_write = true,
    allow_subprocess = true,
    # In production, this would be a real Ed25519 signature
    signature = "PLACEHOLDER-SIGNATURE-REQUIRED-FOR-HUNT",
  },
  validation = {
    checksum = "sha256:placeholder",
    pedigree_version = "1.0.0",
    hunt_authorized = false,  # Must be set true after handshake
  },
  recipes = {
    install = "just install-deploy-example",
    validate = "just validate-deploy-example",
    deploy = "just deploy-example",
    migrate = "just migrate-deploy-example",
  },
} in

# Deployment configuration
let deployment = {
  # Target environments
  environments = {
    dev = {
      replicas = 1,
      memory = "256Mi",
      cpu = "100m",
      image_tag = "dev",
    },
    staging = {
      replicas = 2,
      memory = "512Mi",
      cpu = "250m",
      image_tag = "staging",
    },
    production = {
      replicas = 3,
      memory = "1Gi",
      cpu = "500m",
      image_tag = "latest",
    },
  },

  # Container configuration
  container = {
    image = "ghcr.io/hyperpolymath/k9-example",
    port = 8080,
    health_check = "/health",
    readiness_check = "/ready",
  },

  # Deployment strategy
  strategy = {
    type = "rolling",
    max_surge = 1,
    max_unavailable = 0,
  },
} in

# Deployment scripts (these would be executed at Hunt level)
let scripts = {
  # Pre-deployment validation
  pre_deploy = m%"
#!/bin/sh
set -eu
echo "K9: Pre-deployment validation..."
nickel typecheck deploy.k9.ncl
echo "K9: Validation passed."
"%,

  # Deployment script
  deploy = m%"
#!/bin/sh
set -eu
ENV="${1:-dev}"
echo "K9: Deploying to $ENV environment..."

# In a real deployment, this would:
# 1. Build/pull the container image
# 2. Apply Kubernetes/Podman manifests
# 3. Wait for rollout completion
# 4. Run smoke tests

echo "K9: Deployment to $ENV complete."
"%,

  # Rollback script
  rollback = m%"
#!/bin/sh
set -eu
echo "K9: Rolling back deployment..."
# Rollback logic here
echo "K9: Rollback complete."
"%,
} in

# Export the component
{
  pedigree = component_pedigree,
  deployment = deployment,
  scripts = scripts,

  # Security check: This component requires Hunt level
  required_level = 'Hunt,

  # Warning for users
  warning = m%"
⚠️  WARNING: This is a Hunt-level component.

It can execute shell commands and modify your system.
Before running, ensure you have:

1. Reviewed the deployment scripts above
2. Verified the signature (when implemented)
3. Explicitly authorized Hunt-level execution

Run with: just authorize examples/deploy.k9.ncl && just deploy-example
"%,
}
