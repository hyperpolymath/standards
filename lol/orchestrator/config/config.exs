# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

import Config

config :lol,
  deno_path: System.get_env("DENO_PATH") || "deno",
  julia_path: System.get_env("JULIA_PATH") || "julia",
  verisimdb_data_dir:
    System.get_env("VERISIMDB_DATA_DIR") ||
      Path.expand("~/Documents/hyperpolymath-repos/verisimdb-data"),
  default_workers: 5,
  default_timeout: 60_000

config :logger,
  level: :info

import_config "#{config_env()}.exs"
