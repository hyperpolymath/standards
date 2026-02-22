# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

import Config

if config_env() == :prod do
  config :lol,
    default_workers: String.to_integer(System.get_env("LOL_WORKERS") || "10"),
    default_timeout: String.to_integer(System.get_env("LOL_TIMEOUT") || "120000")
end
