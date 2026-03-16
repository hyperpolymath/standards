# SPDX-License-Identifier: MPL-2.0
# (PMPL-1.0-or-later preferred; MPL-2.0 required for Hex.pm)

defmodule K9.Parser do
  @moduledoc """
  K9 component specification parser.

  Parses .k9 files (YAML-like at Kennel level) into the Component AST.
  K9 files use a simple key-value format with sections denoted by
  indentation and section headers ending with a colon.
  """

  alias K9.Types.{
    Component,
    Pedigree,
    Recipes,
    SecurityLevel,
    SecurityPolicy,
    Target,
    Validation
  }

  @doc """
  Parse a .k9 file string into a Component.

  Returns `{:ok, component}` on success or `{:error, reason}` on failure.
  """
  @spec parse(String.t()) :: {:ok, Component.t()} | {:error, term()}
  def parse(input) when is_binary(input) do
    trimmed = String.trim(input)

    case trimmed do
      "" -> {:error, :empty_input}
      _ -> parse_component(trimmed)
    end
  end

  # ---------------------------------------------------------------------------
  # Internal parsing
  # ---------------------------------------------------------------------------

  defp parse_component(input) do
    lines = String.split(input, "\n")
    sections = split_sections(lines)

    with {:ok, pedigree} <- parse_pedigree(find_section(sections, "pedigree")),
         {:ok, security} <- parse_security(find_section(sections, "security")) do
      target = parse_target(find_section(sections, "target"))
      recipes = parse_recipes(find_section(sections, "recipes"))
      validation = parse_validation(find_section(sections, "validation"))

      # Extract tags — may appear at top level or within the last section
      # because "tags: a, b" is parsed as a field (not a section header).
      tags =
        sections
        |> Enum.flat_map(fn {_name, fields} ->
          case Map.get(fields, "tags", "") do
            "" -> []
            tag_str ->
              tag_str
              |> String.split(",")
              |> Enum.map(&String.trim/1)
              |> Enum.filter(fn t -> t != "" end)
          end
        end)

      {:ok,
       %Component{
         pedigree: pedigree,
         security: security,
         target: target,
         recipes: recipes,
         validation: validation,
         content: %{},
         tags: tags
       }}
    end
  end

  defp split_sections(lines) do
    split_sections(lines, "", %{}, %{})
  end

  defp split_sections([], current_section, current_fields, acc) do
    case map_size(current_fields) do
      0 -> acc
      _ -> Map.put(acc, current_section, current_fields)
    end
  end

  defp split_sections([line | rest], current_section, current_fields, acc) do
    trimmed = String.trim(line)

    cond do
      # Skip blank lines and comments.
      trimmed == "" or String.starts_with?(trimmed, "#") ->
        split_sections(rest, current_section, current_fields, acc)

      # Section header: non-indented, ends with ":", no internal ":".
      section_header?(line, trimmed) ->
        section_name = trimmed |> String.slice(0..-2//1) |> String.trim() |> String.downcase()

        acc =
          case map_size(current_fields) do
            0 -> acc
            _ -> Map.put(acc, current_section, current_fields)
          end

        split_sections(rest, section_name, %{}, acc)

      # Field line.
      true ->
        case String.split(trimmed, ":", parts: 2) do
          [key, value] ->
            field_key = key |> String.trim() |> String.downcase()
            field_value = String.trim(value)
            updated = Map.put(current_fields, field_key, field_value)
            split_sections(rest, current_section, updated, acc)

          _ ->
            split_sections(rest, current_section, current_fields, acc)
        end
    end
  end

  defp section_header?(raw_line, trimmed) do
    String.ends_with?(trimmed, ":") and
      not String.starts_with?(raw_line, " ") and
      not String.starts_with?(raw_line, "\t") and
      not String.contains?(String.slice(trimmed, 0..-2//1), ":")
  end

  defp find_section(sections, name) do
    Map.get(sections, name, %{})
  end

  defp parse_pedigree(fields) do
    case Map.get(fields, "name", "") do
      "" ->
        {:error, {:missing_field, "pedigree.name"}}

      name ->
        {:ok,
         %Pedigree{
           name: name,
           version: Map.get(fields, "version", "0.1.0"),
           description: Map.get(fields, "description", ""),
           author: Map.get(fields, "author"),
           license: Map.get(fields, "license")
         }}
    end
  end

  defp parse_security(fields) do
    level_str = Map.get(fields, "level", "kennel")

    case SecurityLevel.from_string(level_str) do
      {:ok, level} ->
        {:ok,
         %SecurityPolicy{
           level: level,
           allow_network: parse_bool(Map.get(fields, "allow-network", "false")),
           allow_fs_write: parse_bool(Map.get(fields, "allow-fs-write", "false")),
           allow_subprocess: parse_bool(Map.get(fields, "allow-subprocess", "false"))
         }}

      {:error, _} = err ->
        err
    end
  end

  defp parse_bool(s) when is_binary(s) do
    String.downcase(s) in ["true", "yes"]
  end

  defp parse_bool(_), do: false

  defp parse_target(fields) when map_size(fields) == 0, do: nil

  defp parse_target(fields) do
    %Target{
      os: Map.get(fields, "os"),
      is_edge: parse_bool(Map.get(fields, "edge", "false")),
      requires_podman: parse_bool(Map.get(fields, "requires-podman", "false")),
      memory: Map.get(fields, "memory")
    }
  end

  defp parse_recipes(fields) when map_size(fields) == 0, do: nil

  defp parse_recipes(fields) do
    standard_keys = ["install", "validate", "deploy", "migrate"]

    custom =
      fields
      |> Enum.reject(fn {k, v} -> k in standard_keys or v == "" end)
      |> Map.new()

    %Recipes{
      install: Map.get(fields, "install"),
      validate: Map.get(fields, "validate"),
      deploy: Map.get(fields, "deploy"),
      migrate: Map.get(fields, "migrate"),
      custom: custom
    }
  end

  defp parse_validation(fields) when map_size(fields) == 0, do: nil

  defp parse_validation(fields) do
    case Map.get(fields, "checksum", "") do
      "" ->
        nil

      checksum ->
        %Validation{
          checksum: checksum,
          pedigree_version: Map.get(fields, "pedigree-version", "1.0"),
          hunt_authorized: parse_bool(Map.get(fields, "hunt-authorized", "false"))
        }
    end
  end
end
