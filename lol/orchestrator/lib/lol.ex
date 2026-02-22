# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

defmodule Lol do
  @moduledoc """
  1000Langs Elixir Orchestrator

  Manages parallel crawling across multiple Bible corpus sources
  with rate limiting, fault tolerance, and Julia analysis integration.
  """
end

defmodule Lol.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {Lol.RateLimiter, []},
      {Lol.SourceRegistry, []},
      {Lol.CorpusAggregator, []},
      {Lol.VeriSimDBReporter, []},
      {Lol.JuliaBridge, []},
      {DynamicSupervisor, strategy: :one_for_one, name: Lol.CrawlerSupervisor}
    ]

    opts = [strategy: :one_for_one, name: Lol.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
