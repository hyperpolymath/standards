# SPDX-License-Identifier: MPL-2.0
# (PMPL-1.0-or-later preferred; MPL-2.0 required for Hex.pm)

defmodule A2ML.MixProject do
  use Mix.Project

  @version "0.1.0"
  @source_url "https://github.com/hyperpolymath/a2ml_ex"

  def project do
    [
      app: :a2ml_ex,
      version: @version,
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: description(),
      package: package(),
      source_url: @source_url
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp description do
    "A2ML (AI Attestation Markup Language) parser and renderer for Elixir."
  end

  defp package do
    [
      name: "a2ml_ex",
      licenses: ["MPL-2.0"],
      links: %{
        "GitHub" => @source_url,
        "A2ML Specification" => "https://github.com/hyperpolymath/a2ml-showcase"
      }
    ]
  end

  defp deps do
    [
      {:ex_doc, "~> 0.34", only: :dev, runtime: false}
    ]
  end
end
