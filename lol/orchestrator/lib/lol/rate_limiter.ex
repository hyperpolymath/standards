# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

defmodule Lol.RateLimiter do
  @moduledoc """
  Global per-source rate limiter using token bucket algorithm.

  Workers call `acquire/1` before making HTTP requests.
  Shared across all workers to prevent overwhelming sources.
  """

  use GenServer
  require Logger

  @default_delays %{
    bible_cloud: 500,
    bible_com: 2000,
    png_scriptures: 2000,
    ebible: 1000,
    find_bible: 1000,
    bible_is: 100
  }

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  @doc """
  Acquire a rate limit token for the given source.
  Blocks until the token is available.
  """
  def acquire(source) do
    GenServer.call(__MODULE__, {:acquire, source}, 30_000)
  end

  @doc "Get current rate limit status for all sources"
  def status do
    GenServer.call(__MODULE__, :status)
  end

  @doc "Update rate limit delay for a source"
  def set_delay(source, delay_ms) do
    GenServer.cast(__MODULE__, {:set_delay, source, delay_ms})
  end

  @impl true
  def init(_opts) do
    {:ok, %{last_request: %{}, delays: @default_delays}}
  end

  @impl true
  def handle_call({:acquire, source}, _from, state) do
    delay = Map.get(state.delays, source, 1000)
    last = Map.get(state.last_request, source, 0)
    now = System.monotonic_time(:millisecond)
    elapsed = now - last

    if elapsed >= delay do
      new_state = %{state | last_request: Map.put(state.last_request, source, now)}
      {:reply, :ok, new_state}
    else
      wait = delay - elapsed
      Process.sleep(wait)
      now_after = System.monotonic_time(:millisecond)
      new_state = %{state | last_request: Map.put(state.last_request, source, now_after)}
      {:reply, :ok, new_state}
    end
  end

  @impl true
  def handle_call(:status, _from, state) do
    now = System.monotonic_time(:millisecond)

    status =
      Enum.map(state.delays, fn {source, delay} ->
        last = Map.get(state.last_request, source, 0)
        elapsed = now - last
        available = elapsed >= delay

        {source,
         %{
           delay_ms: delay,
           last_request_ms_ago: elapsed,
           available: available
         }}
      end)
      |> Map.new()

    {:reply, status, state}
  end

  @impl true
  def handle_cast({:set_delay, source, delay_ms}, state) do
    {:noreply, %{state | delays: Map.put(state.delays, source, delay_ms)}}
  end
end
