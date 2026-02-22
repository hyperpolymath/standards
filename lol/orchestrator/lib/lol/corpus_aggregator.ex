# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

defmodule Lol.CorpusAggregator do
  @moduledoc """
  Collects crawl results from CrawlerWorker processes into a unified corpus.

  Tracks progress per source and triggers Julia analysis and VeriSimDB
  export when crawl batches complete.
  """

  use GenServer
  require Logger

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  @doc "Report a successful crawl result"
  def report_success(source, language, json_data) do
    GenServer.cast(__MODULE__, {:success, source, language, json_data})
  end

  @doc "Report a failed crawl"
  def report_failure(source, language, reason) do
    GenServer.cast(__MODULE__, {:failure, source, language, reason})
  end

  @doc "Get current progress"
  def progress do
    GenServer.call(__MODULE__, :progress)
  end

  @doc "Get all collected results"
  def results do
    GenServer.call(__MODULE__, :results)
  end

  @doc "Reset the aggregator state"
  def reset do
    GenServer.cast(__MODULE__, :reset)
  end

  # Server callbacks

  @impl true
  def init(_opts) do
    {:ok,
     %{
       results: %{},
       progress: %{},
       failures: %{}
     }}
  end

  @impl true
  def handle_cast({:success, source, language, json_data}, state) do
    source_results = Map.get(state.results, source, %{})
    updated_results = Map.put(source_results, language, json_data)

    source_progress = Map.get(state.progress, source, %{completed: 0, failed: 0, total: 0})
    updated_progress = %{source_progress | completed: source_progress.completed + 1}

    new_state = %{
      state
      | results: Map.put(state.results, source, updated_results),
        progress: Map.put(state.progress, source, updated_progress)
    }

    Logger.info(
      "#{source}/#{language}: success " <>
        "(#{updated_progress.completed}/#{updated_progress.total})"
    )

    # Check if source batch is complete
    if updated_progress.completed + Map.get(state.failures, source, %{}) |> map_size() >=
         updated_progress.total and updated_progress.total > 0 do
      Logger.info("Source #{source} batch complete, triggering analysis")
      trigger_analysis(source, new_state)
    end

    {:noreply, new_state}
  end

  @impl true
  def handle_cast({:failure, source, language, reason}, state) do
    source_failures = Map.get(state.failures, source, %{})
    updated_failures = Map.put(source_failures, language, reason)

    source_progress = Map.get(state.progress, source, %{completed: 0, failed: 0, total: 0})
    updated_progress = %{source_progress | failed: source_progress.failed + 1}

    Logger.warning("#{source}/#{language}: failed - #{reason}")

    {:noreply,
     %{
       state
       | failures: Map.put(state.failures, source, updated_failures),
         progress: Map.put(state.progress, source, updated_progress)
     }}
  end

  @impl true
  def handle_cast(:reset, _state) do
    {:noreply, %{results: %{}, progress: %{}, failures: %{}}}
  end

  @impl true
  def handle_call(:progress, _from, state) do
    {:reply, state.progress, state}
  end

  @impl true
  def handle_call(:results, _from, state) do
    {:reply, state.results, state}
  end

  defp trigger_analysis(source, state) do
    # Merge all results for this source
    source_results = Map.get(state.results, source, %{})

    # Trigger Julia analysis if bridge is available
    case Process.whereis(Lol.JuliaBridge) do
      nil ->
        Logger.info("Julia bridge not available, skipping analysis")

      _pid ->
        corpus_json = Jason.encode!(source_results)
        Lol.JuliaBridge.analyze(corpus_json)
    end

    # Check if all sources are done for VeriSimDB export
    all_done =
      Enum.all?(state.progress, fn {_source, prog} ->
        prog.completed + prog.failed >= prog.total and prog.total > 0
      end)

    if all_done do
      Logger.info("All sources complete, triggering VeriSimDB export")
      Lol.VeriSimDBReporter.export(state.results)
    end
  end
end
