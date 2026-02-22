# SPDX-License-Identifier: MIT AND LicenseRef-Palimpsest-0.8
# SPDX-FileCopyrightText: 2024-2025 Ehsaneddin Asgari and Contributors
#
# 1000Langs Nickel Configuration
# ==============================
# Flexible, type-safe configuration for the parallel corpus crawler

let Contracts = {
  Url = fun label value =>
    if std.string.is_match "^https?://" value then value
    else std.contract.blame_with_message "Invalid URL format" label,

  PositiveInt = fun label value =>
    if std.is_number value && value > 0 then value
    else std.contract.blame_with_message "Must be a positive integer" label,

  Iso639_3 = fun label value =>
    if std.string.is_match "^[a-z]{3}$" value then value
    else std.contract.blame_with_message "Must be valid ISO 639-3 code" label,

  NonEmptyString = fun label value =>
    if std.is_string value && std.string.length value > 0 then value
    else std.contract.blame_with_message "Must be non-empty string" label,
} in

let default_crawler_config = {
  timeout_ms = 30000,
  retries = 3,
  rate_limit_ms = 1000,
  user_agent = "1000Langs/0.1.0 (Parallel Corpus Crawler)",
  respect_robots_txt = true,
  concurrent_requests = 5,
} in

let default_source = {
  enabled = true,
  priority = 1,
  crawler_config = default_crawler_config,
} in

{
  # Project metadata
  project = {
    name = "1000Langs",
    version = "0.1.0",
    description = "Super-parallel corpus crawler for multilingual NLP research",
    homepage = "https://github.com/Hyperpolymath/1000Langs",
    repository = "https://github.com/Hyperpolymath/1000Langs.git",
    license = "MIT AND LicenseRef-Palimpsest-0.8",
    authors = [
      {
        name = "Ehsaneddin Asgari",
        email = "asgari@cis.lmu.de",
        affiliation = "LMU Munich",
      },
    ],
  },

  # Environment configuration
  environment | {
    development = {
      log_level = "debug",
      enable_cache = true,
      cache_ttl_seconds = 3600,
      data_directory = "./data",
      output_directory = "./output",
    },
    production = {
      log_level = "info",
      enable_cache = true,
      cache_ttl_seconds = 86400,
      data_directory = "/var/lib/1000langs/data",
      output_directory = "/var/lib/1000langs/output",
    },
    test = {
      log_level = "warn",
      enable_cache = false,
      cache_ttl_seconds = 0,
      data_directory = "./test/fixtures/data",
      output_directory = "./test/output",
    },
  },

  # Source configurations
  sources = {
    bible_cloud = default_source & {
      name = "Bible Cloud",
      type = "api",
      base_url | Contracts.Url = "https://api.scripture.api.bible/v1",
      requires_auth = true,
      auth_type = "api_key",
      auth_env_var = "BIBLE_CLOUD_API_KEY",
      priority = 1,
    },
    bible_com = default_source & {
      name = "Bible.com (YouVersion)",
      type = "scraper",
      base_url | Contracts.Url = "https://www.bible.com",
      requires_auth = false,
      crawler_config = default_crawler_config & {
        rate_limit_ms = 2000,
        concurrent_requests = 2,
      },
      priority = 2,
    },
    png_scriptures = default_source & {
      name = "PNG Scriptures",
      type = "download",
      base_url | Contracts.Url = "https://pngscriptures.org",
      requires_auth = false,
      formats = ["zip", "html"],
      priority = 3,
    },
    ebible = default_source & {
      name = "eBible.org",
      type = "download",
      base_url | Contracts.Url = "https://ebible.org",
      requires_auth = false,
      formats = ["usfm", "usx", "html"],
      priority = 4,
    },
    find_bible = default_source & {
      name = "Find.Bible",
      type = "api",
      base_url | Contracts.Url = "https://find.bible",
      requires_auth = false,
      priority = 5,
    },
  },

  # Corpus configuration
  corpus = {
    alignment = {
      method = "verse_reference",
      canonical_versification = "original",
      handle_missing = "skip",
      store_metadata = true,
    },
    storage = {
      format = "jsonl",
      compression = "gzip",
      partition_by = "language",
      max_file_size_mb = 100,
    },
    validation = {
      verify_checksums = true,
      validate_encoding = true,
      detect_duplicates = true,
      minimum_verse_coverage = 0.1,
    },
  },

  # OpenCyc integration
  opencyc = {
    enabled = true,
    endpoint | Contracts.Url = "http://localhost:3602",
    connection_timeout_ms = 5000,
    cache_queries = true,
    concept_mappings = {
      language_concept = "#$HumanLanguage",
      script_concept = "#$WritingScript",
      region_concept = "#$GeographicalRegion",
    },
  },

  # Echidna proof integration
  proofs = {
    enabled = true,
    echidna_path = "./proofs",
    auto_verify = false,
    verification_level = "full",
    proof_categories = [
      "data_integrity",
      "alignment_correctness",
      "statistical_properties",
      "type_safety",
    ],
  },

  # Export configurations
  export = {
    formats = {
      json = {
        pretty_print = false,
        include_metadata = true,
      },
      csv = {
        delimiter = "\t",
        quote_char = "\"",
        escape_char = "\\",
      },
      parquet = {
        compression = "snappy",
        row_group_size = 10000,
      },
      conllu = {
        include_morphology = true,
        include_dependencies = false,
      },
    },
    default_format = "json",
  },

  # Containerization (Podman)
  container = {
    runtime = "podman",
    image_name = "1000langs",
    base_image = "docker.io/library/node:20-alpine",
    labels = {
      "org.opencontainers.image.title" = "1000Langs",
      "org.opencontainers.image.description" = "Super-parallel corpus crawler",
      "org.opencontainers.image.source" = "https://github.com/Hyperpolymath/1000Langs",
      "org.opencontainers.image.licenses" = "MIT AND LicenseRef-Palimpsest-0.8",
    },
    security = {
      rootless = true,
      read_only_rootfs = true,
      no_new_privileges = true,
      drop_capabilities = ["ALL"],
      add_capabilities = [],
    },
  },

  # RSR Compliance
  rsr = {
    target_level = "gold",
    checks = {
      documentation = true,
      security = true,
      accessibility = true,
      licensing = true,
      testing = true,
      containerization = true,
    },
    exemptions = [],
  },

  # Logging and monitoring
  logging = {
    format = "json",
    outputs = ["stdout", "file"],
    file_path = "logs/1000langs.log",
    rotation = {
      max_size_mb = 50,
      max_files = 10,
      compress = true,
    },
    levels = {
      default = "info",
      crawlers = "debug",
      proofs = "debug",
      api = "info",
    },
  },

  # Future: GIS integration placeholder
  gis = {
    enabled = false,
    provider = "arcgis",  # or "openlayers", "mapbox", "leaflet"
    server = {
      type = "openlitespeed",
      port = 8080,
    },
    layers = {
      language_distribution = {
        source = "glottolog",
        style = "choropleth",
      },
      typological_features = {
        source = "wals",
        style = "points",
      },
    },
  },
}
