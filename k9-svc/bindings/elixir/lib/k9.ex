# SPDX-License-Identifier: MPL-2.0
# (PMPL-1.0-or-later preferred; MPL-2.0 required for Hex.pm)

defmodule K9 do
  @moduledoc """
  K9 (Self-Validating Components) parser and renderer.

  Provides types, parsing, and rendering for K9 component specifications.
  K9 is a format for self-validating software components with built-in
  security levels (Kennel/Yard/Hunt), pedigree metadata, and lifecycle
  recipes.

  ## Quick Start

      iex> {:ok, component} = K9.parse("pedigree:\\n  name: hello\\n  version: 1.0.0\\n  description: Test\\n\\nsecurity:\\n  level: kennel")
      iex> K9.render(component)

  ## Modules

  - `K9.Types` — Core data structures (Component, Pedigree, SecurityLevel, etc.)
  - `K9.Parser` — Parse .k9 text into a Component AST
  - `K9.Renderer` — Render a Component AST back to .k9 text
  """

  alias K9.Parser
  alias K9.Renderer

  @doc """
  Parse a .k9 file string into a Component.

  Delegates to `K9.Parser.parse/1`.
  """
  @spec parse(String.t()) :: {:ok, K9.Types.Component.t()} | {:error, term()}
  defdelegate parse(input), to: Parser

  @doc """
  Render a Component back to .k9 format text.

  Delegates to `K9.Renderer.render/1`.
  """
  @spec render(K9.Types.Component.t()) :: String.t()
  defdelegate render(component), to: Renderer
end
