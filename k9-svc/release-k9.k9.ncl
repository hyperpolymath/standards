K9!
# SPDX-License-Identifier: PMPL-1.0-or-later
# K9-SVC Release Automation
#
# This K9 component automates the release process for K9-SVC itself - ultimate
# dogfooding! It handles version bumping, testing, signing, documentation, and
# release artifact creation.

leash = 'Hunt

pedigree = {
  schema_version = "1.0.0",
  component_type = "release-automation",
  author = "Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>",
  description = "Automated K9-SVC release process (K9 eating its own dog food!)",
  created = "2026-01-30",
  k9_spec_version = "1.0.0",
}

config = {
  # Version information
  current_version | String = "1.0.0",
  release_type | [| 'Major, 'Minor, 'Patch |] = 'Patch,

  # Release components
  components = {
    build_k9_sign | Bool = true,
    build_k9_scan | Bool = true,
    update_must_script | Bool = true,
    regenerate_docs | Bool = true,
    run_full_tests | Bool = true,
  },

  # Signing
  signing = {
    sign_release_artifacts | Bool = true,
    signing_key_name | String = "k9-svc-release",
    verify_signatures | Bool = true,
  },

  # Documentation
  documentation = {
    update_changelog | Bool = true,
    update_readme | Bool = true,
    generate_api_docs | Bool = false,  # No API docs yet
    update_security_docs | Bool = true,
  },

  # GitHub release
  github_release = {
    create_release | Bool = true,
    draft | Bool = true,  # Create as draft for review
    prerelease | Bool = false,
    generate_notes | Bool = true,
  },

  # Artifact distribution
  distribution = {
    create_tarball | Bool = true,
    create_checksums | Bool = true,
    upload_to_github | Bool = true,
  },
}

# Calculate new version based on release type
new_version = {
  major =
    let parts = std.string.split "." config.current_version in
    let major = std.string.to_number (std.array.at 0 parts) + 1 in
    "%{std.string.from major}.0.0",

  minor =
    let parts = std.string.split "." config.current_version in
    let major = std.string.to_number (std.array.at 0 parts) in
    let minor = std.string.to_number (std.array.at 1 parts) + 1 in
    "%{std.string.from major}.%{std.string.from minor}.0",

  patch =
    let parts = std.string.split "." config.current_version in
    let major = std.string.to_number (std.array.at 0 parts) in
    let minor = std.string.to_number (std.array.at 1 parts) in
    let patch = std.string.to_number (std.array.at 2 parts) + 1 in
    "%{std.string.from major}.%{std.string.from minor}.%{std.string.from patch}",

  version =
    if config.release_type == 'Major then new_version.major
    else if config.release_type == 'Minor then new_version.minor
    else new_version.patch,
}

recipes = {
  default = {
    recipe = "release",
    description = "Complete release process",
  },

  "check-prerequisites" = {
    description = "Verify prerequisites for release",
    commands = [
      "echo 'Checking prerequisites for K9-SVC release...'",

      # Check clean git state
      "if [ -n \"$(git status --porcelain)\" ]; then echo 'Error: Working directory is not clean'; git status --short; exit 1; fi",
      "echo '✓ Git working directory clean'",

      # Check on main branch
      "if [ \"$(git branch --show-current)\" != 'main' ]; then echo 'Error: Not on main branch'; exit 1; fi",
      "echo '✓ On main branch'",

      # Check GitHub CLI
      "command -v gh >/dev/null 2>&1 || { echo 'Error: gh CLI not found'; exit 1; }",
      "echo '✓ GitHub CLI available'",

      # Check k9-sign installed
      "command -v k9-sign >/dev/null 2>&1 || { echo 'Error: k9-sign not installed'; exit 1; }",
      "k9-sign --version",
      "echo '✓ k9-sign available'",

      # Check signing key exists
      "if [ ! -f ~/.config/k9/keys/%{config.signing.signing_key_name}.key ]; then echo 'Error: Release signing key not found'; exit 1; fi",
      "echo '✓ Release signing key found'",

      "echo 'Prerequisites check complete!'",
    ],
  },

  "bump-version" = {
    description = "Bump version numbers in all files",
    dependencies = ["check-prerequisites"],
    commands = [
      "echo 'Bumping version: %{config.current_version} → %{new_version.version}'",

      # Update must script version
      "sed -i 's/K9_VERSION=\"%{config.current_version}\"/K9_VERSION=\"%{new_version.version}\"/' must",

      # Update k9-sign Cargo.toml
      "sed -i 's/^version = \"%{config.current_version}\"/version = \"%{new_version.version}\"/' k9-sign/Cargo.toml",

      # Update k9-sign/install.sh
      "sed -i 's/VERSION=\"%{config.current_version}\"/VERSION=\"%{new_version.version}\"/' k9-sign/install.sh",

      # Update pedigree.ncl
      "sed -i 's/version = \"%{config.current_version}\"/version = \"%{new_version.version}\"/' pedigree.ncl",

      "echo 'Version bumped to %{new_version.version}'",
    ],
  },

  "build-k9-sign" = {
    description = "Build k9-sign release binary",
    skip = !config.components.build_k9_sign,
    commands = [
      "echo 'Building k9-sign release binary...'",
      "cd k9-sign",
      "cargo build --release",
      "ls -lh target/release/k9-sign",
      "echo 'Binary size:' $(du -h target/release/k9-sign | cut -f1)",
      "cd ..",
    ],
  },

  "run-tests" = {
    description = "Run full test suite",
    skip = !config.components.run_full_tests,
    commands = [
      "echo 'Running K9-SVC test suite...'",

      # Test k9-sign
      "cd k9-sign && cargo test --release -- --test-threads=1",
      "echo '✓ k9-sign tests passed'",

      # Test k9-scan
      "./k9-scan examples/hello.k9.ncl",
      "echo '✓ k9-scan works'",

      # Test must script
      "./must status",
      "echo '✓ must script works'",

      # Test dogfooding (validate K9 files in repo)
      "./must run dogfood",
      "echo '✓ Dogfooding validation passed'",

      "cd ..",
      "echo 'All tests passed!'",
    ],
  },

  "update-changelog" = {
    description = "Update CHANGELOG.md with release notes",
    skip = !config.documentation.update_changelog,
    commands = [
      "echo 'Updating CHANGELOG.md...'",

      # Get commits since last tag
      "last_tag=$(git describe --tags --abbrev=0 2>/dev/null || echo 'v0.0.0')",
      "echo 'Changes since $last_tag:'",
      "git log $last_tag..HEAD --oneline --pretty=format:'- %s' > .k9-release-notes.txt",

      # Prepend to CHANGELOG.md
      "echo '## [%{new_version.version}] - $(date +%Y-%m-%d)' > .k9-changelog-new.txt",
      "echo '' >> .k9-changelog-new.txt",
      "cat .k9-release-notes.txt >> .k9-changelog-new.txt",
      "echo '' >> .k9-changelog-new.txt",
      "if [ -f CHANGELOG.md ]; then cat CHANGELOG.md >> .k9-changelog-new.txt; fi",
      "mv .k9-changelog-new.txt CHANGELOG.md",

      "echo 'CHANGELOG.md updated'",
    ],
  },

  "create-release-tarball" = {
    description = "Create release tarball",
    skip = !config.distribution.create_tarball,
    dependencies = ["build-k9-sign", "run-tests"],
    commands = [
      "echo 'Creating release tarball...'",

      "mkdir -p release/k9-svc-%{new_version.version}",

      # Copy essential files
      "cp -r README.adoc SPEC.adoc GUIDE.adoc LICENSE SECURITY.md must justfile pedigree.ncl register.ncl examples/ release/k9-svc-%{new_version.version}/",

      # Copy k9-sign binary and docs
      "cp k9-sign/target/release/k9-sign release/k9-svc-%{new_version.version}/",
      "cp k9-sign/README.md k9-sign/MIGRATION.md release/k9-svc-%{new_version.version}/",

      # Copy k9-scan
      "cp k9-scan release/k9-svc-%{new_version.version}/",

      # Copy security docs
      "cp -r docs/ release/k9-svc-%{new_version.version}/",

      # Create tarball
      "cd release && tar czf k9-svc-%{new_version.version}.tar.gz k9-svc-%{new_version.version}/",

      # Create checksums
      "cd release && sha256sum k9-svc-%{new_version.version}.tar.gz > k9-svc-%{new_version.version}.tar.gz.sha256",

      "ls -lh release/k9-svc-%{new_version.version}.tar.gz",
      "echo 'Release tarball created!'",
    ],
  },

  "sign-release" = {
    description = "Sign release artifacts with k9-sign",
    skip = !config.signing.sign_release_artifacts,
    dependencies = ["create-release-tarball"],
    commands = [
      "echo 'Signing release artifacts...'",

      # Sign tarball
      "k9-sign sign release/k9-svc-%{new_version.version}.tar.gz %{config.signing.signing_key_name}",

      # Verify signature
      "if [ %{std.string.from config.signing.verify_signatures} = 'true' ]; then k9-sign verify release/k9-svc-%{new_version.version}.tar.gz; fi",

      "echo 'Release artifacts signed!'",
    ],
  },

  "create-github-release" = {
    description = "Create GitHub release",
    skip = !config.github_release.create_release,
    dependencies = ["sign-release"],
    commands = [
      "echo 'Creating GitHub release v%{new_version.version}...'",

      # Create git tag
      "git tag -a v%{new_version.version} -m 'Release v%{new_version.version}'",
      "git push origin v%{new_version.version}",

      # Create GitHub release
      "gh release create v%{new_version.version} " ++
        (if config.github_release.draft then "--draft " else "") ++
        (if config.github_release.prerelease then "--prerelease " else "") ++
        (if config.github_release.generate_notes then "--generate-notes " else "") ++
        "--title 'K9-SVC v%{new_version.version}' " ++
        "release/k9-svc-%{new_version.version}.tar.gz " ++
        "release/k9-svc-%{new_version.version}.tar.gz.sha256 " ++
        "release/k9-svc-%{new_version.version}.tar.gz.sig",

      "echo 'GitHub release created!'",
      "echo 'Release URL: https://github.com/hyperpolymath/k9-svc/releases/tag/v%{new_version.version}'",
    ],
  },

  "release" = {
    description = "Complete release process (default)",
    dependencies = [
      "check-prerequisites",
      "bump-version",
      "build-k9-sign",
      "run-tests",
      "update-changelog",
      "create-release-tarball",
      "sign-release",
      "create-github-release",
    ],
    commands = [
      "echo ''",
      "echo '╔══════════════════════════════════════════════════════════╗'",
      "echo '║  ✅ K9-SVC v%{new_version.version} Release Complete!                    ║'",
      "echo '╚══════════════════════════════════════════════════════════╝'",
      "echo ''",
      "echo 'Version: %{new_version.version}'",
      "echo 'Release type: %{std.string.from config.release_type}'",
      "echo 'Tarball: release/k9-svc-%{new_version.version}.tar.gz'",
      "echo 'GitHub: https://github.com/hyperpolymath/k9-svc/releases/tag/v%{new_version.version}'",
      "echo ''",
      "echo 'Next steps:'",
      "echo '  1. Review GitHub release draft'",
      "echo '  2. Publish release when ready'",
      "echo '  3. Announce on social media / mailing lists'",
      "echo ''",
      "echo '🐕 K9 ate its own dog food - released using a K9 component!'",
      "echo ''",
    ],
  },
}

validation = {
  # Ensure release type is valid
  valid_release_type =
    config.release_type == 'Major
    || config.release_type == 'Minor
    || config.release_type == 'Patch
    | doc "Release type must be Major, Minor, or Patch",

  # Ensure current version is valid semver
  valid_semver =
    std.string.is_match "^[0-9]+\\.[0-9]+\\.[0-9]+$" config.current_version
    | doc "Current version must be valid semver (X.Y.Z)",

  # Ensure signing key name is non-empty if signing enabled
  valid_signing_key =
    !config.signing.sign_release_artifacts
    || std.string.length config.signing.signing_key_name > 0
    | doc "Signing key name must be specified if signing is enabled",
}

# Metadata for ultimate dogfooding
metadata = {
  dogfooding_note = "This K9 component releases K9-SVC itself! It demonstrates how a K9 component can automate complex workflows with validation, signing, and multi-step processes.",

  security_note = "This release process uses k9-sign (built by K9-SVC) to sign release artifacts, creating a trust chain: K9 component → k9-sign → release artifacts.",

  demonstration = [
    "Hunt-level execution (full automation)",
    "Multi-step dependencies (recipe ordering)",
    "Conditional execution (skip recipes)",
    "Version calculation (Nickel functions)",
    "Validation contracts (semver, signing keys)",
    "External tool integration (gh, git, cargo)",
  ],
}
