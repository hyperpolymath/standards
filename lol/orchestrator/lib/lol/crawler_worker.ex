# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

defmodule Lol.CrawlerWorker do
  @moduledoc """
  GenServer that crawls a single {source, language} pair.

  Spawns a Deno subprocess to execute the ReScript crawler,
  monitors the process, and reports results to CorpusAggregator.
  """

  use GenServer, restart: :temporary
  require Logger

  @default_timeout 60_000

  def start_link({source, language, opts}) do
    GenServer.start_link(__MODULE__, {source, language, opts})
  end

  @impl true
  def init({source, language, opts}) do
    # Start crawling immediately
    send(self(), :crawl)

    {:ok,
     %{
       source: source,
       language: language,
       opts: opts,
       timeout: Keyword.get(opts, :timeout, @default_timeout),
       port: nil,
       output: ""
     }}
  end

  @impl true
  def handle_info(:crawl, state) do
    Logger.info("Crawling #{state.source}/#{state.language}")

    # Acquire rate limit token
    Lol.RateLimiter.acquire(state.source)

    # Build Deno command
    source_str = source_to_string(state.source)
    output_path = output_path(state.source, state.language)

    cmd = "deno"
    args = [
      "run", "-A",
      "src/Lang1000.res.mjs",
      "crawl",
      "--source", source_str,
      "--lang", state.language,
      "--output", output_path
    ]

    # Get project root (one level up from orchestrator/)
    project_root =
      Application.get_env(:lol, :project_root) ||
        Path.join([__DIR__, "..", "..", ".."]) |> Path.expand()

    port =
      Port.open(
        {:spawn_executable, System.find_executable(cmd)},
        [
          :binary,
          :exit_status,
          :stderr_to_stdout,
          args: args,
          cd: project_root
        ]
      )

    # Set timeout
    Process.send_after(self(), :timeout, state.timeout)

    {:noreply, %{state | port: port}}
  end

  @impl true
  def handle_info({port, {:data, data}}, %{port: port} = state) do
    {:noreply, %{state | output: state.output <> data}}
  end

  @impl true
  def handle_info({port, {:exit_status, 0}}, %{port: port} = state) do
    Logger.info("Completed #{state.source}/#{state.language}")

    # Read output JSON and report to aggregator
    output_path = output_path(state.source, state.language)

    case File.read(output_path) do
      {:ok, json} ->
        Lol.CorpusAggregator.report_success(state.source, state.language, json)

      {:error, _} ->
        Lol.CorpusAggregator.report_success(state.source, state.language, state.output)
    end

    {:stop, :normal, state}
  end

  @impl true
  def handle_info({port, {:exit_status, code}}, %{port: port} = state) do
    Logger.warning("#{state.source}/#{state.language} exited with code #{code}")
    Lol.CorpusAggregator.report_failure(state.source, state.language, "Exit code: #{code}")
    {:stop, :normal, state}
  end

  @impl true
  def handle_info(:timeout, state) do
    Logger.warning("#{state.source}/#{state.language} timed out")

    if state.port do
      Port.close(state.port)
    end

    Lol.CorpusAggregator.report_failure(state.source, state.language, "Timeout")
    {:stop, :normal, state}
  end

  defp source_to_string(:bible_cloud), do: "bible.cloud"
  defp source_to_string(:bible_com), do: "bible.com"
  defp source_to_string(:bible_is), do: "bible.is"
  defp source_to_string(:png_scriptures), do: "pngscriptures.org"
  defp source_to_string(:ebible), do: "ebible.org"
  defp source_to_string(:find_bible), do: "find.bible"

  defp output_path(source, language) do
    dir = Path.join([System.tmp_dir!(), "lol-crawl"])
    File.mkdir_p!(dir)
    Path.join(dir, "#{source}-#{language}.json")
  end
end
