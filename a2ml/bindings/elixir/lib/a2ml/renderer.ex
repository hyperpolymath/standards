# SPDX-License-Identifier: MPL-2.0
# (PMPL-1.0-or-later preferred; MPL-2.0 required for Hex.pm)

defmodule A2ML.Renderer do
  @moduledoc """
  Render an A2ML Document AST back to A2ML text.

  Produces A2ML-formatted output from a parsed Document structure,
  suitable for round-tripping through parse -> modify -> render.
  """

  alias A2ML.Types.{Attestation, Document, TrustLevel}

  @doc """
  Render a Document to A2ML-formatted text.

  Produces a complete document string with title, blocks, and
  appropriate blank-line separation.
  """
  @spec render(Document.t()) :: String.t()
  def render(%Document{} = doc) do
    parts =
      case doc.title do
        nil -> []
        title -> ["# #{title}", ""]
      end

    block_strings =
      doc.blocks
      |> Enum.map(&render_block/1)
      |> Enum.intersperse("")

    parts = parts ++ block_strings

    parts
    |> Enum.join("\n")
    |> String.trim()
    |> Kernel.<>("\n")
  end

  @doc """
  Render a single Block to A2ML text.
  """
  @spec render_block(A2ML.Types.Block.t()) :: String.t()
  def render_block({:heading, level, inlines}) do
    hashes = String.duplicate("#", level)
    "#{hashes} #{render_inlines(inlines)}"
  end

  def render_block({:paragraph, inlines}) do
    render_inlines(inlines)
  end

  def render_block({:code_block, lang, content}) do
    opener = if lang, do: "```#{lang}", else: "```"
    "#{opener}\n#{content}\n```"
  end

  def render_block({:directive, d}) do
    base = "@#{d.name} #{d.value}"

    case d.attributes do
      [] ->
        base

      attrs ->
        attr_strs = Enum.map(attrs, fn {k, v} -> "#{k}=#{v}" end)
        "#{base} [#{Enum.join(attr_strs, ", ")}]"
    end
  end

  def render_block({:attestation, a}) do
    render_attestation(a)
  end

  def render_block(:thematic_break) do
    "---"
  end

  def render_block({:block_quote, blocks}) do
    blocks
    |> Enum.map(fn b ->
      render_block(b)
      |> String.split("\n")
      |> Enum.map(fn line -> "> #{line}" end)
      |> Enum.join("\n")
    end)
    |> Enum.join("\n>\n")
  end

  def render_block({:list_block, ordered, items}) do
    items
    |> Enum.with_index(1)
    |> Enum.map(fn {item, idx} ->
      prefix = if ordered, do: "#{idx}. ", else: "- "

      item_text =
        item
        |> Enum.map(&render_block/1)
        |> Enum.join("\n")

      "#{prefix}#{item_text}"
    end)
    |> Enum.join("\n")
  end

  @doc """
  Render an Attestation to its A2ML block format.
  """
  @spec render_attestation(Attestation.t()) :: String.t()
  def render_attestation(%Attestation{} = a) do
    lines = [
      "!attest",
      "  identity: #{a.identity}",
      "  role: #{a.role}",
      "  trust-level: #{TrustLevel.to_string(a.trust_level)}"
    ]

    lines =
      case a.timestamp do
        nil -> lines
        ts -> lines ++ ["  timestamp: #{ts}"]
      end

    lines =
      case a.note do
        nil -> lines
        n -> lines ++ ["  note: #{n}"]
      end

    Enum.join(lines, "\n")
  end

  @doc """
  Render a list of Inline elements to text.
  """
  @spec render_inlines([A2ML.Types.Inline.t()]) :: String.t()
  def render_inlines(inlines) do
    inlines
    |> Enum.map(&render_inline/1)
    |> Enum.join("")
  end

  defp render_inline({:text, v}), do: v
  defp render_inline({:emphasis, c}), do: "*#{render_inlines(c)}*"
  defp render_inline({:strong, c}), do: "**#{render_inlines(c)}**"
  defp render_inline({:code, v}), do: "`#{v}`"
  defp render_inline({:link, c, u}), do: "[#{render_inlines(c)}](#{u})"
end
