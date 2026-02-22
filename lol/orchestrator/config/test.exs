# SPDX-License-Identifier: PMPL-1.0-or-later

import Config

config :logger, level: :warning

config :lol,
  default_workers: 2,
  default_timeout: 5_000
