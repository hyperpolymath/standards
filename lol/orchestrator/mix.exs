# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

defmodule Lol.MixProject do
  use Mix.Project

  def project do
    [
      app: :lol,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: "Elixir orchestrator for 1000Langs parallel corpus crawler",
      package: package()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {Lol.Application, []}
    ]
  end

  defp deps do
    [
      {:jason, "~> 1.4"},
      {:nimble_pool, "~> 1.1"},
      {:telemetry, "~> 1.3"}
    ]
  end

  defp package do
    [
      licenses: ["PMPL-1.0-or-later"],
      maintainers: ["Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>"]
    ]
  end
end
