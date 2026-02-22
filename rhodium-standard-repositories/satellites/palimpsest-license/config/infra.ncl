# Palimpsest License Infrastructure Configuration
# Nickel configuration for reproducible infrastructure-as-code
# Version: 0.4.0
# SPDX-License-Identifier: Palimpsest-0.4 OR MIT

{
  # ============================================================================
  # PROJECT METADATA
  # ============================================================================

  metadata = {
    name = "palimpsest-license",
    version = "0.4.0",
    description = "A future-proof license for creative work in the age of AI",
    repository = "https://github.com/your-username/palimpsest-license",
    license = ["Palimpsest-0.4", "MIT"],
    maintainers = [
      {
        name = "Palimpsest Stewardship Council",
        email = "council@palimpsest.license",
        role = "governance",
      }
    ],
    keywords = [
      "license",
      "creative-commons",
      "ai-ethics",
      "emotional-lineage",
      "quantum-proof",
      "cultural-sensitivity",
    ],
  },

  # ============================================================================
  # BUILD CONFIGURATION
  # ============================================================================

  build = {
    # Build system selector
    system = "just",  # Primary: just, Secondary: nix, make

    # Build targets
    targets = {
      default = ["validate", "test", "build"],

      validate = {
        command = "just validate",
        description = "Run all validation checks (RSR compliance)",
        dependencies = ["lint", "validate-links", "validate-licenses"],
      },

      test = {
        command = "just test",
        description = "Run comprehensive test suite",
        dependencies = ["test-haskell", "test-rescript", "test-integration"],
        coverage_threshold = 80,  # Target for Silver RSR
      },

      build = {
        command = "just build",
        description = "Build all components",
        dependencies = ["build-styles", "build-haskell", "build-rescript"],
        reproducible = true,
        hermetic = false,  # TODO: Enable for Silver RSR
      },

      deploy = {
        command = "just deploy",
        description = "Deploy to production",
        dependencies = ["validate", "test", "build"],
        requires_approval = true,
      },
    },

    # Build environment
    environment = {
      node_version = "18.x",
      haskell_version = "ghc-9.4.8",
      nix_version = "2.18",

      # Environment variables
      env_vars = {
        NODE_ENV = "production",
        SOURCE_DATE_EPOCH = "1609459200",  # 2021-01-01 for reproducibility
        LANG = "en_GB.UTF-8",  # British English
      },
    },

    # Artifact configuration
    artifacts = {
      # Generated CSS
      css = {
        source = "styles/scss/",
        output = "styles/css/",
        format = "compressed",
        source_maps = false,  # Disable for reproducibility
      },

      # Haskell binaries
      validators = {
        source = "TOOLS/validation/haskell/",
        output = "dist-newstyle/build/",
        optimization = "O2",
      },

      # ReScript output
      rescript = {
        source = "rescript/src/",
        output = "rescript/lib/",
        format = "es6",
      },

      # Documentation bundles
      docs = {
        formats = ["html", "pdf", "epub"],
        output = "dist/docs/",
      },
    },
  },

  # ============================================================================
  # SECURITY POLICIES
  # ============================================================================

  security = {
    # Dependency scanning
    dependency_audit = {
      enabled = true,
      schedule = "weekly",
      tools = ["npm audit", "cabal outdated"],
      fail_on = ["critical", "high"],
      auto_fix = false,  # Manual review required
    },

    # SPDX license validation
    license_validation = {
      enabled = true,
      required_headers = ["SPDX-License-Identifier"],
      allowed_licenses = [
        "Palimpsest-0.4",
        "MIT",
        "Apache-2.0",
        "BSD-3-Clause",
        "CC-BY-SA-4.0",
      ],
    },

    # Secret scanning
    secret_scanning = {
      enabled = true,
      patterns = [
        "(?i)(api[_-]?key|apikey)\\s*[:=]\\s*['\"]?[a-zA-Z0-9]{20,}",
        "(?i)(secret[_-]?key|secretkey)\\s*[:=]\\s*['\"]?[a-zA-Z0-9]{20,}",
        "(?i)(private[_-]?key|privatekey)\\s*[:=]\\s*['\"]?[a-zA-Z0-9]{20,}",
        "-----BEGIN (RSA|DSA|EC|OPENSSH) PRIVATE KEY-----",
      ],
      exclude_files = [".git/*", "node_modules/*", "*.lock"],
    },

    # Container security
    container = {
      base_image = "cgr.dev/chainguard/wolfi-base:latest",
      run_as_user = "nonroot",
      read_only_root = true,
      no_new_privileges = true,
      drop_capabilities = ["ALL"],
      seccomp_profile = "runtime/default",
    },

    # Code signing
    signing = {
      enabled = false,  # TODO: Enable for production
      gpg_key_id = "",
      sign_commits = false,
      sign_tags = true,
    },
  },

  # ============================================================================
  # NETWORK CONFIGURATION
  # ============================================================================

  network = {
    # Offline-first compliance
    offline_first = {
      enabled = true,
      allow_network = false,  # Build must succeed without network
      cache_dependencies = true,
    },

    # CDN configuration (optional, fallback only)
    cdn = {
      provider = "cloudflare",
      domains = ["cdn.palimpsest.license"],
      cache_ttl = 86400,  # 24 hours
      compress = true,
    },

    # API endpoints (for future validator service)
    api = {
      base_url = "https://api.palimpsest.license",
      version = "v1",
      endpoints = {
        validate = "/validate",
        metadata = "/metadata",
        provenance = "/provenance",
      },
      rate_limiting = {
        enabled = true,
        requests_per_minute = 60,
      },
    },
  },

  # ============================================================================
  # CONTAINER SPECIFICATIONS
  # ============================================================================

  container = {
    # Base configuration
    base = {
      image = "cgr.dev/chainguard/wolfi-base:latest",
      variant = "minimal",
      architecture = ["amd64", "arm64"],
    },

    # Runtime configuration
    runtime = {
      user = "nonroot",
      uid = 65532,
      gid = 65532,
      working_dir = "/app",
      shell = "/bin/sh",
    },

    # Layer structure
    layers = [
      {
        name = "system-dependencies",
        packages = [
          "ca-certificates",
          "tzdata",
          "curl",
          "git",
        ],
      },
      {
        name = "haskell-toolchain",
        packages = [
          "ghc-9.4",
          "cabal-install",
        ],
      },
      {
        name = "node-toolchain",
        packages = [
          "nodejs-18",
          "npm",
        ],
      },
      {
        name = "application",
        copy = [
          { source = "package*.json", dest = "/app/" },
          { source = "Justfile", dest = "/app/" },
          { source = "LICENSES/", dest = "/app/LICENSES/" },
          { source = "GUIDES_v0.4/", dest = "/app/GUIDES_v0.4/" },
        ],
      },
    ],

    # Build stages
    stages = [
      {
        name = "builder",
        from = "cgr.dev/chainguard/wolfi-base:latest",
        commands = [
          "apk add --no-cache nodejs-18 npm ghc-9.4 cabal-install git",
          "WORKDIR /build",
          "COPY package*.json ./",
          "RUN npm ci --only=production",
          "COPY . .",
          "RUN just build",
        ],
      },
      {
        name = "runtime",
        from = "cgr.dev/chainguard/wolfi-base:latest",
        commands = [
          "apk add --no-cache nodejs-18",
          "USER nonroot:nonroot",
          "WORKDIR /app",
          "COPY --from=builder --chown=nonroot:nonroot /build/dist ./dist",
          "COPY --from=builder --chown=nonroot:nonroot /build/node_modules ./node_modules",
          "EXPOSE 8000",
          "HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 CMD curl -f http://localhost:8000/health || exit 1",
          "CMD [\"node\", \"dist/server.js\"]",
        ],
      },
    ],

    # Volume mounts (for development)
    volumes = [
      { host = "./LICENSES", container = "/app/LICENSES", read_only = true },
      { host = "./docs", container = "/app/docs", read_only = true },
    ],

    # Port mappings
    ports = [
      { host = 8000, container = 8000, protocol = "tcp" },
    ],

    # Health check
    healthcheck = {
      test = ["CMD", "curl", "-f", "http://localhost:8000/health"],
      interval = "30s",
      timeout = "3s",
      start_period = "5s",
      retries = 3,
    },

    # Resource limits
    resources = {
      limits = {
        memory = "512M",
        cpu = "1.0",
      },
      reservations = {
        memory = "256M",
        cpu = "0.5",
      },
    },
  },

  # ============================================================================
  # DEPENDENCY MANAGEMENT
  # ============================================================================

  dependencies = {
    # Node.js dependencies
    npm = {
      production = [],  # Documentation project, no production deps
      development = [
        { name = "eslint", version = "^8.0.0" },
        { name = "prettier", version = "^3.0.0" },
        { name = "sass", version = "^1.69.0" },
        { name = "markdownlint-cli", version = "^0.39.0" },
        { name = "cspell", version = "^8.0.0" },
      ],
    },

    # Haskell dependencies
    cabal = {
      base = [
        { name = "base", constraint = ">=4.16 && <5" },
        { name = "text", constraint = ">=2.0" },
        { name = "aeson", constraint = ">=2.1" },
        { name = "megaparsec", constraint = ">=9.3" },
        { name = "containers", constraint = ">=0.6" },
      ],
      development = [
        { name = "hspec", constraint = ">=2.11" },
        { name = "QuickCheck", constraint = ">=2.14" },
        { name = "hedgehog", constraint = ">=1.2" },
      ],
    },

    # System dependencies (Nix)
    nix = {
      packages = [
        "nodejs-18_x",
        "cabal-install",
        "ghc",
        "git",
        "just",
        "curl",
        "jq",
      ],
    },

    # SBOM generation
    sbom = {
      enabled = true,
      format = "cyclonedx",  # or "spdx"
      output = "sbom.json",
      include = ["npm", "cabal", "nix"],
    },
  },

  # ============================================================================
  # TESTING CONFIGURATION
  # ============================================================================

  testing = {
    # Unit tests
    unit = {
      haskell = {
        framework = "hspec",
        directory = "TOOLS/validation/haskell/test/",
        coverage_threshold = 80,
        property_tests = true,
      },
      rescript = {
        framework = "jest",
        directory = "rescript/__tests__/",
        coverage_threshold = 80,
      },
    },

    # Integration tests
    integration = {
      enabled = true,
      directory = "tests/integration/",
      scenarios = [
        "metadata-validation-pipeline",
        "spdx-header-enforcement",
        "bilingual-consistency-check",
      ],
    },

    # End-to-end tests
    e2e = {
      enabled = false,  # Not applicable for documentation project
    },

    # Performance tests
    performance = {
      enabled = false,
      benchmarks = ["validator-throughput", "metadata-parsing"],
    },

    # Spell checking (British English)
    spell_check = {
      enabled = true,
      language = "en-GB",
      dictionaries = ["british-english", "technical-terms"],
      custom_words = [
        "palimpsest",
        "blockchain",
        "metadata",
        "Haskell",
        "ReScript",
        "Nickel",
      ],
    },
  },

  # ============================================================================
  # DEPLOYMENT TARGETS
  # ============================================================================

  deployment = {
    # GitHub Pages
    github_pages = {
      enabled = true,
      branch = "gh-pages",
      directory = "dist/",
      custom_domain = "palimpsest.license",
      enforce_https = true,
    },

    # Container registry
    container_registry = {
      provider = "ghcr.io",  # GitHub Container Registry
      namespace = "palimpsest-license",
      repository = "palimpsest-license",
      tags = ["latest", "v0.4.0"],
    },

    # Documentation hosting
    docs_hosting = {
      provider = "readthedocs",
      project = "palimpsest-license",
      versions = ["latest", "stable", "v0.4", "v0.3"],
    },

    # Release artifacts
    releases = {
      enabled = true,
      platforms = ["github", "gitlab", "ipfs"],
      artifacts = [
        "palimpsest-v0.4.md",
        "palimpsest-v0.4.pdf",
        "palimpsest-v0.4.epub",
        "sbom.json",
      ],
      checksum_algorithm = "sha256",
      signing = {
        enabled = false,  # TODO: Enable for production
        method = "gpg",
      },
    },
  },

  # ============================================================================
  # COMPLIANCE & GOVERNANCE
  # ============================================================================

  compliance = {
    # RSR (Rhodium Standard Repository) compliance
    rsr = {
      tier = "bronze",  # Current: Bronze, Target: Silver
      categories = {
        documentation = { score = 10, max = 10 },
        well_known = { score = 10, max = 10 },
        build_system = { score = 9, max = 10 },
        testing = { score = 6, max = 10 },
        type_safety = { score = 10, max = 10 },
        memory_safety = { score = 10, max = 10 },
        offline_first = { score = 10, max = 10 },
        tpcf = { score = 10, max = 10 },
        dependency_audit = { score = 8, max = 10 },
        reproducibility = { score = 8, max = 10 },
        cultural_sensitivity = { score = 10, max = 10 },
      },
      total_score = 91,
      max_score = 110,
      percentage = 82.7,
    },

    # License compliance
    licenses = {
      primary = "Palimpsest-0.4",
      fallback = "MIT",
      bilingual = true,
      languages = ["en", "nl"],
    },

    # Governance model
    governance = {
      model = "stewardship-council",
      council_size = 7,
      term_length = "2 years",
      staggered_terms = true,
      voting_threshold = {
        minor_changes = 4,  # 4/7 votes
        major_changes = 5,  # 5/7 votes
        constitutional = 6,  # 6/7 votes
      },
    },
  },

  # ============================================================================
  # MONITORING & OBSERVABILITY
  # ============================================================================

  monitoring = {
    # Logging
    logging = {
      level = "info",
      format = "json",
      destination = "stdout",
    },

    # Metrics
    metrics = {
      enabled = false,  # Not applicable for static site
    },

    # Error tracking
    error_tracking = {
      enabled = false,  # Not applicable for documentation
    },

    # Uptime monitoring
    uptime = {
      enabled = true,
      endpoints = [
        "https://palimpsest.license",
        "https://palimpsest.license/.well-known/security.txt",
      ],
      interval = "5m",
      notifications = ["email"],
    },
  },

  # ============================================================================
  # EXTENSIBILITY
  # ============================================================================

  extensibility = {
    # Plugins
    plugins = {
      enabled = true,
      directories = ["plugins/", ".claude/"],
    },

    # Hooks
    hooks = {
      pre_commit = ".git-hooks/pre-commit",
      pre_push = ".git-hooks/pre-push",
      post_checkout = null,
    },

    # Custom validators
    validators = {
      emotional_lineage = "TOOLS/validation/emotional-lineage.hs",
      bilingual_consistency = "TOOLS/validation/bilingual-check.hs",
      metadata_schemas = "TOOLS/validation/metadata-validator.hs",
    },
  },
}
