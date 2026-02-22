# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

defmodule Lol.CrawlerSupervisor do
  @moduledoc """
  Manages concurrent CrawlerWorker processes via DynamicSupervisor.

  Controls concurrency per source and handles worker lifecycle
  with one_for_one restart strategy.
  """

  @default_concurrency %{
    bible_cloud: 5,
    bible_com: 2,
    png_scriptures: 3,
    ebible: 5,
    find_bible: 3,
    bible_is: 5
  }

  @doc "Start crawling a source with specified concurrency"
  def start_crawl(source, languages, opts \\ []) do
    workers = Keyword.get(opts, :workers, Map.get(@default_concurrency, source, 3))

    languages
    |> Enum.chunk_every(workers)
    |> Enum.each(fn batch ->
      tasks =
        Enum.map(batch, fn lang ->
          DynamicSupervisor.start_child(
            Lol.CrawlerSupervisor,
            {Lol.CrawlerWorker, {source, lang, opts}}
          )
        end)

      # Wait for batch to complete before starting next
      Enum.each(tasks, fn
        {:ok, pid} -> ref = Process.monitor(pid)
                       receive do
                         {:DOWN, ^ref, :process, ^pid, _} -> :ok
                       after
                         Keyword.get(opts, :timeout, 120_000) -> :timeout
                       end
        {:error, reason} ->
          require Logger
          Logger.warning("Failed to start worker: #{inspect(reason)}")
      end)
    end)
  end

  @doc "Start crawling all sources"
  def start_crawl_all(opts \\ []) do
    sources = [:bible_cloud, :bible_com, :png_scriptures, :ebible, :find_bible]

    Enum.each(sources, fn source ->
      languages = Lol.SourceRegistry.list_languages(source)
      start_crawl(source, languages, opts)
    end)
  end
end
