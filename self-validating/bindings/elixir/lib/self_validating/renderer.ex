# SPDX-License-Identifier: MPL-2.0
# (PMPL-1.0-or-later preferred; MPL-2.0 required for Hex.pm)

defmodule K9.Renderer do
  @moduledoc """
  Render a K9 Component AST back to .k9 text.

  Produces K9-formatted output (YAML-like at Kennel level) from a
  parsed Component structure.
  """

  alias K9.Types.{Component, Recipes, SecurityLevel, SecurityPolicy, Target, Validation}

  @doc """
  Render a Component to K9-formatted text.

  Produces a complete .k9 file string with pedigree, security,
  and optional target/recipes/validation sections.
  """
  @spec render(Component.t()) :: String.t()
  def render(%Component{} = c) do
    parts = render_pedigree(c)
    parts = parts ++ ["" | render_security(c.security)]

    parts =
      case c.target do
        nil -> parts
        target -> parts ++ ["" | render_target(target)]
      end

    parts =
      case c.recipes do
        nil -> parts
        recipes -> parts ++ ["" | render_recipes(recipes)]
      end

    parts =
      case c.validation do
        nil -> parts
        v -> parts ++ ["" | render_validation(v)]
      end

    parts =
      case c.tags do
        [] -> parts
        tags -> parts ++ ["", "tags: #{Enum.join(tags, ", ")}"]
      end

    Enum.join(parts, "\n") <> "\n"
  end

  # ---------------------------------------------------------------------------
  # Internal section renderers
  # ---------------------------------------------------------------------------

  defp render_pedigree(%Component{pedigree: p}) do
    lines = [
      "pedigree:",
      "  name: #{p.name}",
      "  version: #{p.version}",
      "  description: #{p.description}"
    ]

    lines = if p.author, do: lines ++ ["  author: #{p.author}"], else: lines
    lines = if p.license, do: lines ++ ["  license: #{p.license}"], else: lines
    lines
  end

  defp render_security(%SecurityPolicy{} = s) do
    [
      "security:",
      "  level: #{SecurityLevel.to_string(s.level)}",
      "  allow-network: #{bool_to_string(s.allow_network)}",
      "  allow-fs-write: #{bool_to_string(s.allow_fs_write)}",
      "  allow-subprocess: #{bool_to_string(s.allow_subprocess)}"
    ]
  end

  defp render_target(%Target{} = t) do
    lines = ["target:"]
    lines = if t.os, do: lines ++ ["  os: #{t.os}"], else: lines

    lines =
      lines ++
        [
          "  edge: #{bool_to_string(t.is_edge)}",
          "  requires-podman: #{bool_to_string(t.requires_podman)}"
        ]

    lines = if t.memory, do: lines ++ ["  memory: #{t.memory}"], else: lines
    lines
  end

  defp render_recipes(%Recipes{} = r) do
    lines = ["recipes:"]
    lines = if r.install, do: lines ++ ["  install: #{r.install}"], else: lines
    lines = if r.validate, do: lines ++ ["  validate: #{r.validate}"], else: lines
    lines = if r.deploy, do: lines ++ ["  deploy: #{r.deploy}"], else: lines
    lines = if r.migrate, do: lines ++ ["  migrate: #{r.migrate}"], else: lines

    lines =
      Enum.reduce(r.custom, lines, fn {k, v}, acc ->
        acc ++ ["  #{k}: #{v}"]
      end)

    lines
  end

  defp render_validation(%Validation{} = v) do
    [
      "validation:",
      "  checksum: #{v.checksum}",
      "  pedigree-version: #{v.pedigree_version}",
      "  hunt-authorized: #{bool_to_string(v.hunt_authorized)}"
    ]
  end

  defp bool_to_string(true), do: "true"
  defp bool_to_string(false), do: "false"
end
