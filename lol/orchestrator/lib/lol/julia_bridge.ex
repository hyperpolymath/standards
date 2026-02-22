# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

defmodule Lol.JuliaBridge do
  @moduledoc """
  Manages Julia analysis process via Erlang Port.

  Sends corpus data as JSON over stdin, receives analysis results
  over stdout. Supports commands: analyze_corpus, distance_matrix,
  cluster_languages, frequency_spectra.
  """

  use GenServer
  require Logger

  @analysis_dir Path.expand("../analysis", __DIR__) |> Path.expand()

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  @doc "Analyze corpus data with Julia"
  def analyze(corpus_json) do
    GenServer.call(__MODULE__, {:analyze, corpus_json}, 300_000)
  end

  @doc "Compute pairwise distance matrix"
  def distance_matrix(corpus_json, metric \\ "jensen_shannon") do
    GenServer.call(__MODULE__, {:distance_matrix, corpus_json, metric}, 600_000)
  end

  @doc "Cluster languages from distance matrix"
  def cluster_languages(corpus_json, method \\ "upgma") do
    GenServer.call(__MODULE__, {:cluster, corpus_json, method}, 300_000)
  end

  @doc "Compute frequency spectra for corpus"
  def frequency_spectra(corpus_json) do
    GenServer.call(__MODULE__, {:frequency_spectra, corpus_json}, 300_000)
  end

  @doc "Check if Julia bridge is available"
  def available? do
    GenServer.call(__MODULE__, :available?)
  end

  @impl true
  def init(_opts) do
    {:ok, %{port: nil, pending: nil, buffer: ""}}
  end

  @impl true
  def handle_call(:available?, _from, state) do
    available = System.find_executable("julia") != nil and File.dir?(@analysis_dir)
    {:reply, available, state}
  end

  @impl true
  def handle_call({cmd, corpus_json}, from, state) do
    handle_call({cmd, corpus_json, nil}, from, state)
  end

  @impl true
  def handle_call({cmd, corpus_json, extra}, from, state) do
    case ensure_port(state) do
      {:ok, port, new_state} ->
        request =
          %{
            cmd: to_string(cmd),
            corpus: corpus_json,
            extra: extra
          }
          |> Jason.encode!()

        Port.command(port, request <> "\n")
        {:noreply, %{new_state | pending: from, buffer: ""}}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_info({port, {:data, data}}, %{port: port} = state) do
    buffer = state.buffer <> data

    # Check for complete JSON response (terminated by newline)
    if String.contains?(buffer, "\n") do
      [response | _] = String.split(buffer, "\n", parts: 2)

      case Jason.decode(response) do
        {:ok, result} ->
          if state.pending do
            GenServer.reply(state.pending, {:ok, result})
          end

          {:noreply, %{state | pending: nil, buffer: ""}}

        {:error, _} ->
          # Incomplete JSON, keep buffering
          {:noreply, %{state | buffer: buffer}}
      end
    else
      {:noreply, %{state | buffer: buffer}}
    end
  end

  @impl true
  def handle_info({port, {:exit_status, code}}, %{port: port} = state) do
    Logger.warning("Julia process exited with code #{code}")

    if state.pending do
      GenServer.reply(state.pending, {:error, "Julia process exited: #{code}"})
    end

    {:noreply, %{state | port: nil, pending: nil, buffer: ""}}
  end

  defp ensure_port(%{port: nil} = state) do
    julia = System.find_executable("julia")

    if julia && File.dir?(@analysis_dir) do
      port =
        Port.open(
          {:spawn_executable, julia},
          [
            :binary,
            :exit_status,
            {:line, 1_048_576},
            args: ["--project=#{@analysis_dir}", "#{@analysis_dir}/src/server.jl"],
            cd: @analysis_dir
          ]
        )

      # Wait for Julia startup
      Process.sleep(2000)
      {:ok, port, %{state | port: port}}
    else
      {:error, "Julia not found or analysis directory missing"}
    end
  end

  defp ensure_port(%{port: port} = state) do
    {:ok, port, state}
  end
end
