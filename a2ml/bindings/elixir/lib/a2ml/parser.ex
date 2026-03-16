# SPDX-License-Identifier: MPL-2.0
# (PMPL-1.0-or-later preferred; MPL-2.0 required for Hex.pm)

defmodule A2ML.Parser do
  @moduledoc """
  A2ML document parser.

  Parses A2ML-formatted text into a structured `A2ML.Types.Document` AST.
  A2ML syntax is similar to Markdown with extensions for directives (`@`)
  and attestation blocks (`!attest`).
  """

  alias A2ML.Types.{Attestation, Directive, Document, TrustLevel}

  @doc """
  Parse an A2ML-formatted string into a Document.

  Returns `{:ok, document}` on success or `{:error, reason}` on failure.

  ## Examples

      iex> A2ML.Parser.parse("# Hello\\n\\nA paragraph.")
      {:ok, %A2ML.Types.Document{title: "Hello", blocks: [_], ...}}

  """
  @spec parse(String.t()) :: {:ok, Document.t()} | {:error, term()}
  def parse(input) when is_binary(input) do
    trimmed = String.trim(input)

    case trimmed do
      "" ->
        {:error, :empty_input}

      _ ->
        {:ok, parse_document(trimmed)}
    end
  end

  # ---------------------------------------------------------------------------
  # Internal parsing
  # ---------------------------------------------------------------------------

  defp parse_document(input) do
    lines = String.split(input, "\n")
    {title, rest_lines} = extract_title(lines)
    blocks = parse_blocks(rest_lines)

    directives = extract_directives(blocks)
    attestations = extract_attestations(blocks)

    %Document{
      title: title,
      directives: directives,
      blocks: blocks,
      attestations: attestations
    }
  end

  defp extract_title([first | rest]) do
    trimmed = String.trim(first)

    case trimmed do
      "# " <> title -> {title, rest}
      _ -> {nil, [first | rest]}
    end
  end

  defp extract_title([]), do: {nil, []}

  defp parse_blocks(lines), do: parse_blocks(lines, [])

  defp parse_blocks([], acc), do: Enum.reverse(acc)

  defp parse_blocks([line | rest], acc) do
    trimmed = String.trim(line)

    cond do
      # Skip blank lines.
      trimmed == "" ->
        parse_blocks(rest, acc)

      # Thematic break.
      trimmed in ["---", "----", "-----"] ->
        parse_blocks(rest, [:thematic_break | acc])

      # Code block.
      String.starts_with?(trimmed, "```") ->
        lang = String.trim(String.slice(trimmed, 3..-1//1))
        lang = if lang == "", do: nil, else: lang
        {code_lines, remaining} = collect_code_block(rest)
        content = Enum.join(code_lines, "\n")
        parse_blocks(remaining, [{:code_block, lang, content} | acc])

      # Directive.
      String.starts_with?(trimmed, "@") ->
        directive = parse_directive_line(trimmed)
        parse_blocks(rest, [{:directive, directive} | acc])

      # Attestation.
      String.starts_with?(trimmed, "!attest") ->
        {attest_lines, remaining} = collect_indented_block(rest)
        attestation = parse_attestation_block([trimmed | attest_lines])
        parse_blocks(remaining, [{:attestation, attestation} | acc])

      # Heading.
      String.starts_with?(trimmed, "#") ->
        {level, text} = parse_heading_line(trimmed)
        inlines = parse_inlines(text)
        parse_blocks(rest, [{:heading, level, inlines} | acc])

      # Block quote.
      String.starts_with?(trimmed, ">") ->
        {quote_lines, remaining} = collect_prefixed_block([line | rest], ">")

        stripped =
          Enum.map(quote_lines, fn l ->
            t = String.trim(l)

            cond do
              String.starts_with?(t, "> ") -> String.slice(t, 2..-1//1)
              String.starts_with?(t, ">") -> String.slice(t, 1..-1//1)
              true -> t
            end
          end)

        inner_blocks = parse_blocks(stripped)
        parse_blocks(remaining, [{:block_quote, inner_blocks} | acc])

      # Unordered list.
      String.starts_with?(trimmed, "- ") or String.starts_with?(trimmed, "* ") ->
        {list_lines, remaining} = collect_list_block([line | rest])
        items = parse_list_items(list_lines, false)
        parse_blocks(remaining, [{:list_block, false, items} | acc])

      # Ordered list.
      ordered_list_start?(trimmed) ->
        {list_lines, remaining} = collect_list_block([line | rest])
        items = parse_list_items(list_lines, true)
        parse_blocks(remaining, [{:list_block, true, items} | acc])

      # Paragraph (default).
      true ->
        {para_lines, remaining} = collect_paragraph([line | rest])
        text = Enum.join(para_lines, " ")
        inlines = parse_inlines(text)
        parse_blocks(remaining, [{:paragraph, inlines} | acc])
    end
  end

  defp collect_code_block(lines), do: collect_code_block(lines, [])

  defp collect_code_block([], acc), do: {Enum.reverse(acc), []}

  defp collect_code_block([line | rest], acc) do
    if String.starts_with?(String.trim(line), "```") do
      {Enum.reverse(acc), rest}
    else
      collect_code_block(rest, [line | acc])
    end
  end

  defp collect_indented_block(lines), do: collect_indented_block(lines, [])

  defp collect_indented_block([], acc), do: {Enum.reverse(acc), []}

  defp collect_indented_block([line | rest] = lines, acc) do
    if String.starts_with?(line, "  ") or String.starts_with?(line, "\t") do
      collect_indented_block(rest, [String.trim(line) | acc])
    else
      {Enum.reverse(acc), lines}
    end
  end

  defp collect_prefixed_block(lines, prefix),
    do: collect_prefixed_block(lines, prefix, [])

  defp collect_prefixed_block([], _prefix, acc), do: {Enum.reverse(acc), []}

  defp collect_prefixed_block([line | rest] = lines, prefix, acc) do
    trimmed = String.trim(line)

    if String.starts_with?(trimmed, prefix) do
      collect_prefixed_block(rest, prefix, [line | acc])
    else
      {Enum.reverse(acc), lines}
    end
  end

  defp collect_list_block(lines), do: collect_list_block(lines, [])

  defp collect_list_block([], acc), do: {Enum.reverse(acc), []}

  defp collect_list_block([line | rest] = lines, acc) do
    trimmed = String.trim(line)

    cond do
      trimmed == "" ->
        {Enum.reverse(acc), rest}

      String.starts_with?(trimmed, "- ") or
        String.starts_with?(trimmed, "* ") or
        ordered_list_start?(trimmed) or
        String.starts_with?(line, "  ") or
          String.starts_with?(line, "\t") ->
        collect_list_block(rest, [line | acc])

      true ->
        {Enum.reverse(acc), lines}
    end
  end

  defp collect_paragraph(lines), do: collect_paragraph(lines, [])

  defp collect_paragraph([], acc), do: {Enum.reverse(acc), []}

  defp collect_paragraph([line | rest] = lines, acc) do
    trimmed = String.trim(line)

    cond do
      trimmed == "" ->
        {Enum.reverse(acc), rest}

      String.starts_with?(trimmed, "#") or
        String.starts_with?(trimmed, "@") or
        String.starts_with?(trimmed, "!attest") or
        String.starts_with?(trimmed, "```") or
        String.starts_with?(trimmed, ">") or
          trimmed in ["---", "----", "-----"] ->
        {Enum.reverse(acc), lines}

      true ->
        collect_paragraph(rest, [trimmed | acc])
    end
  end

  defp parse_heading_line(line) do
    level = count_leading_hashes(line, 0)
    text = String.trim(String.slice(line, level..-1//1))
    {level, text}
  end

  defp count_leading_hashes("#" <> rest, n), do: count_leading_hashes(rest, n + 1)
  defp count_leading_hashes(_, n), do: n

  defp ordered_list_start?(line) do
    case String.split(line, ". ", parts: 2) do
      [prefix, _rest] when prefix != "" -> String.match?(prefix, ~r/^\d+$/)
      _ -> false
    end
  end

  defp parse_directive_line(line) do
    without_at = String.trim(String.slice(line, 1..-1//1))

    case String.split(without_at, " ", parts: 2) do
      [name, value] ->
        %Directive{name: String.trim(name), value: String.trim(value)}

      [name] ->
        %Directive{name: String.trim(name), value: ""}
    end
  end

  defp parse_attestation_block(lines) do
    fields = parse_key_value_lines(lines)
    identity = Map.get(fields, "identity", "")
    role = Map.get(fields, "role", "")

    trust_level =
      case TrustLevel.from_string(Map.get(fields, "trust-level", "")) do
        {:ok, tl} -> tl
        {:error, _} -> :unverified
      end

    %Attestation{
      identity: identity,
      role: role,
      trust_level: trust_level,
      timestamp: Map.get(fields, "timestamp"),
      note: Map.get(fields, "note")
    }
  end

  defp parse_key_value_lines(lines) do
    lines
    |> Enum.reduce(%{}, fn line, acc ->
      trimmed = String.trim(line)

      case String.split(trimmed, ":", parts: 2) do
        [key, value] ->
          Map.put(acc, String.downcase(String.trim(key)), String.trim(value))

        _ ->
          acc
      end
    end)
  end

  defp parse_list_items(lines, _ordered) do
    lines
    |> split_list_items()
    |> Enum.map(fn item_lines ->
      text = Enum.join(item_lines, " ")
      inlines = parse_inlines(text)
      [{:paragraph, inlines}]
    end)
  end

  defp split_list_items(lines), do: split_list_items(lines, [], [])

  defp split_list_items([], current, acc) do
    case current do
      [] -> Enum.reverse(acc)
      _ -> Enum.reverse([Enum.reverse(current) | acc])
    end
  end

  defp split_list_items([line | rest], current, acc) do
    trimmed = String.trim(line)

    if String.starts_with?(trimmed, "- ") or
         String.starts_with?(trimmed, "* ") or
         ordered_list_start?(trimmed) do
      item_text = strip_list_marker(trimmed)

      new_acc =
        case current do
          [] -> acc
          _ -> [Enum.reverse(current) | acc]
        end

      split_list_items(rest, [item_text], new_acc)
    else
      split_list_items(rest, [trimmed | current], acc)
    end
  end

  defp strip_list_marker("- " <> rest), do: rest
  defp strip_list_marker("* " <> rest), do: rest

  defp strip_list_marker(line) do
    case String.split(line, ". ", parts: 2) do
      [prefix, rest] ->
        if String.match?(prefix, ~r/^\d+$/), do: rest, else: line

      _ ->
        line
    end
  end

  @doc """
  Parse inline elements from a text string.

  Handles: `**bold**`, `*italic*`, `` `code` ``, and `[text](url)` links.
  """
  @spec parse_inlines(String.t()) :: [Inline.t()]
  def parse_inlines(input), do: parse_inlines_acc(input, []) |> Enum.reverse()

  defp parse_inlines_acc("", acc), do: acc

  defp parse_inlines_acc("**" <> rest, acc) do
    case String.split(rest, "**", parts: 2) do
      [inner, after_close] ->
        children = parse_inlines(inner)
        parse_inlines_acc(after_close, [{:strong, children} | acc])

      _ ->
        {text, remaining} = take_until_special("**" <> rest)
        parse_inlines_acc(remaining, [{:text, text} | acc])
    end
  end

  defp parse_inlines_acc("*" <> rest, acc) do
    case String.split(rest, "*", parts: 2) do
      [inner, after_close] ->
        children = parse_inlines(inner)
        parse_inlines_acc(after_close, [{:emphasis, children} | acc])

      _ ->
        {text, remaining} = take_until_special("*" <> rest)
        parse_inlines_acc(remaining, [{:text, text} | acc])
    end
  end

  defp parse_inlines_acc("`" <> rest, acc) do
    case String.split(rest, "`", parts: 2) do
      [code, after_close] ->
        parse_inlines_acc(after_close, [{:code, code} | acc])

      _ ->
        {text, remaining} = take_until_special("`" <> rest)
        parse_inlines_acc(remaining, [{:text, text} | acc])
    end
  end

  defp parse_inlines_acc("[" <> rest, acc) do
    case String.split(rest, "](", parts: 2) do
      [link_text, after_bracket] ->
        case String.split(after_bracket, ")", parts: 2) do
          [url, after_close] ->
            content = parse_inlines(link_text)
            parse_inlines_acc(after_close, [{:link, content, url} | acc])

          _ ->
            {text, remaining} = take_until_special("[" <> rest)
            parse_inlines_acc(remaining, [{:text, text} | acc])
        end

      _ ->
        {text, remaining} = take_until_special("[" <> rest)
        parse_inlines_acc(remaining, [{:text, text} | acc])
    end
  end

  defp parse_inlines_acc(input, acc) do
    {text, remaining} = take_until_special(input)
    parse_inlines_acc(remaining, [{:text, text} | acc])
  end

  defp take_until_special(input), do: take_until_special(input, "")

  defp take_until_special("", acc), do: {acc, ""}

  defp take_until_special(<<c, rest::binary>> = input, acc)
       when c in [?*, ?`, ?[] do
    case acc do
      "" ->
        # Take one char literally to avoid infinite loop.
        {<<c>>, rest}

      _ ->
        {acc, input}
    end
  end

  defp take_until_special(<<c, rest::binary>>, acc) do
    take_until_special(rest, acc <> <<c>>)
  end

  defp extract_directives(blocks) do
    Enum.flat_map(blocks, fn
      {:directive, d} -> [d]
      _ -> []
    end)
  end

  defp extract_attestations(blocks) do
    Enum.flat_map(blocks, fn
      {:attestation, a} -> [a]
      _ -> []
    end)
  end
end
