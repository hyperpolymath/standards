# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

defmodule Lol.VeriSimDBReporter do
  @moduledoc """
  Exports crawl + analysis results to verisimdb-data format.

  Merges ReScript quality checks and Julia analysis results into a
  single scan JSON compatible with the VeriSimDB pipeline.
  """

  use GenServer
  require Logger

  @verisimdb_data_dir System.get_env("VERISIMDB_DATA_DIR") ||
                        Path.expand("~/Documents/hyperpolymath-repos/verisimdb-data")

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  @doc "Export corpus results as VeriSimDB scan"
  def export(results) do
    GenServer.cast(__MODULE__, {:export, results})
  end

  @doc "Get path to last exported scan"
  def last_export do
    GenServer.call(__MODULE__, :last_export)
  end

  @impl true
  def init(_opts) do
    {:ok, %{last_export: nil}}
  end

  @impl true
  def handle_cast({:export, results}, state) do
    scan = build_scan(results)
    output_path = Path.join(System.tmp_dir!(), "lol-scan.json")

    case File.write(output_path, Jason.encode!(scan, pretty: true)) do
      :ok ->
        Logger.info("VeriSimDB scan written to: #{output_path}")
        maybe_ingest(output_path)
        {:noreply, %{state | last_export: output_path}}

      {:error, reason} ->
        Logger.error("Failed to write scan: #{inspect(reason)}")
        {:noreply, state}
    end
  end

  @impl true
  def handle_call(:last_export, _from, state) do
    {:reply, state.last_export, state}
  end

  defp build_scan(results) do
    weak_points = analyze_results(results)

    total_lines =
      results
      |> Enum.flat_map(fn {_source, langs} -> Map.values(langs) end)
      |> length()

    %{
      repo: "lol",
      version: "0.1.0",
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
      scanner: "lol-elixir-orchestrator",
      scanner_version: "0.1.0",
      weak_points: weak_points,
      statistics: %{
        total_files: map_size(results),
        total_lines: total_lines,
        total_weak_points: length(weak_points),
        total_unsafe_blocks:
          Enum.count(weak_points, fn wp -> wp.severity in ["critical", "high"] end),
        files:
          Enum.map(results, fn {source, langs} ->
            source_wps = Enum.filter(weak_points, fn wp -> wp.source == to_string(source) end)

            %{
              file: to_string(source),
              total_lines: map_size(langs),
              weak_points: length(source_wps),
              unsafe_blocks:
                Enum.count(source_wps, fn wp -> wp.severity in ["critical", "high"] end)
            }
          end)
      }
    }
  end

  defp analyze_results(results) do
    results
    |> Enum.flat_map(fn {source, langs} ->
      Enum.flat_map(langs, fn {lang, _data} ->
        # Basic quality checks at the orchestrator level
        check_coverage(source, lang, results)
      end)
    end)
  end

  defp check_coverage(source, lang, results) do
    # Check if this language appears in other sources
    other_sources =
      results
      |> Enum.filter(fn {s, _} -> s != source end)
      |> Enum.filter(fn {_, langs} -> Map.has_key?(langs, lang) end)

    if Enum.empty?(other_sources) do
      [
        %{
          category: "coverage-gap",
          severity: "low",
          location: lang,
          description: "Only available from #{source}",
          context: nil,
          language: lang,
          source: to_string(source)
        }
      ]
    else
      []
    end
  end

  defp maybe_ingest(scan_path) do
    ingest_script = Path.join(@verisimdb_data_dir, "scripts/ingest-scan.sh")

    if File.exists?(ingest_script) do
      Logger.info("Ingesting scan into verisimdb-data...")

      case System.cmd("bash", [ingest_script, "lol", scan_path],
             cd: @verisimdb_data_dir,
             stderr_to_stdout: true
           ) do
        {output, 0} ->
          Logger.info("Ingest complete: #{String.trim(output)}")

        {output, code} ->
          Logger.warning("Ingest failed (exit #{code}): #{String.trim(output)}")
      end
    else
      Logger.info("verisimdb-data not found, skipping ingest")
    end
  end
end
