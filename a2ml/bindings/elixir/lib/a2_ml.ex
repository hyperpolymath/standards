# SPDX-License-Identifier: MPL-2.0
# (PMPL-1.0-or-later preferred; MPL-2.0 required for Hex.pm)

defmodule A2ML do
  @moduledoc """
  A2ML (AI Attestation Markup Language) parser and renderer.

  Provides types, parsing, and rendering for A2ML documents — a lightweight
  markup format designed for AI-generated content with provenance tracking
  through attestation chains and trust levels.

  ## Quick Start

      iex> {:ok, doc} = A2ML.parse("# Hello\\n\\n@version 1.0\\n\\nA paragraph.")
      iex> A2ML.render(doc)

  ## Modules

  - `A2ML.Types` — Core data structures (Document, Block, Attestation, etc.)
  - `A2ML.Parser` — Parse A2ML text into a Document AST
  - `A2ML.Renderer` — Render a Document AST back to A2ML text
  """

  alias A2ML.Parser
  alias A2ML.Renderer

  @doc """
  Parse an A2ML-formatted string into a Document.

  Delegates to `A2ML.Parser.parse/1`.
  """
  @spec parse(String.t()) :: {:ok, A2ML.Types.Document.t()} | {:error, term()}
  defdelegate parse(input), to: Parser

  @doc """
  Render a Document back to A2ML-formatted text.

  Delegates to `A2ML.Renderer.render/1`.
  """
  @spec render(A2ML.Types.Document.t()) :: String.t()
  defdelegate render(doc), to: Renderer
end
