# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

defmodule Lol.SourceRegistry do
  @moduledoc """
  Stores known language lists per source.

  Populated on startup from cached data or API discovery.
  Provides language lookup and source metadata.
  """

  use Agent

  @initial_languages %{
    bible_cloud: ~w(eng fra spa deu por ita nld rus zho jpn kor ara hin ben tha vie ind msa),
    bible_com: ~w(eng fra spa deu por ita nld rus zho jpn kor ara hin ben tha vie),
    png_scriptures: ~w(tpi tok niu meu aey ksd wos bef),
    ebible: ~w(eng fra spa deu por ita nld rus zho jpn kor ara hin ben tha vie ind msa swa yor hau),
    find_bible: ~w(eng fra spa deu por ita nld rus zho jpn kor ara),
    bible_is: ~w(eng fra spa deu por ita nld rus zho jpn kor ara hin ben tha vie ind msa)
  }

  def start_link(_opts) do
    Agent.start_link(fn -> @initial_languages end, name: __MODULE__)
  end

  @doc "List all known languages for a source"
  def list_languages(source) do
    Agent.get(__MODULE__, fn state -> Map.get(state, source, []) end)
  end

  @doc "List all available sources"
  def list_sources do
    Agent.get(__MODULE__, fn state -> Map.keys(state) end)
  end

  @doc "Update language list for a source"
  def update_languages(source, languages) do
    Agent.update(__MODULE__, fn state -> Map.put(state, source, languages) end)
  end

  @doc "Add a language to a source"
  def add_language(source, language) do
    Agent.update(__MODULE__, fn state ->
      current = Map.get(state, source, [])

      if language in current do
        state
      else
        Map.put(state, source, current ++ [language])
      end
    end)
  end

  @doc "Get total language count across all sources"
  def total_languages do
    Agent.get(__MODULE__, fn state ->
      state
      |> Map.values()
      |> List.flatten()
      |> Enum.uniq()
      |> length()
    end)
  end

  @doc "Get sources that cover a specific language"
  def sources_for_language(language) do
    Agent.get(__MODULE__, fn state ->
      state
      |> Enum.filter(fn {_source, langs} -> language in langs end)
      |> Enum.map(fn {source, _} -> source end)
    end)
  end
end
