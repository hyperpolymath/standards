# SPDX-License-Identifier: MPL-2.0

import Config

config :logger, level: :warning

config :lol,
  default_workers: 2,
  default_timeout: 5_000
