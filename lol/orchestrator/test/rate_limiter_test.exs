# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

defmodule Lol.RateLimiterTest do
  use ExUnit.Case

  setup do
    {:ok, pid} = Lol.RateLimiter.start_link([])
    on_exit(fn -> Process.alive?(pid) && GenServer.stop(pid) end)
    %{pid: pid}
  end

  describe "acquire/1" do
    test "allows first request immediately" do
      start = System.monotonic_time(:millisecond)
      assert :ok = Lol.RateLimiter.acquire(:bible_cloud)
      elapsed = System.monotonic_time(:millisecond) - start
      assert elapsed < 100
    end

    test "enforces rate limit delay between requests" do
      assert :ok = Lol.RateLimiter.acquire(:bible_com)
      start = System.monotonic_time(:millisecond)
      assert :ok = Lol.RateLimiter.acquire(:bible_com)
      elapsed = System.monotonic_time(:millisecond) - start
      # bible_com has 2000ms delay
      assert elapsed >= 1900
    end

    test "different sources have independent rate limits" do
      assert :ok = Lol.RateLimiter.acquire(:bible_cloud)
      start = System.monotonic_time(:millisecond)
      assert :ok = Lol.RateLimiter.acquire(:ebible)
      elapsed = System.monotonic_time(:millisecond) - start
      # Different source should be immediate
      assert elapsed < 100
    end
  end

  describe "status/0" do
    test "returns status for all sources" do
      status = Lol.RateLimiter.status()
      assert is_map(status)
      assert Map.has_key?(status, :bible_cloud)
      assert Map.has_key?(status, :bible_com)
    end

    test "shows availability correctly" do
      status = Lol.RateLimiter.status()
      # All sources should be available initially
      Enum.each(status, fn {_source, info} ->
        assert info.available == true
      end)
    end
  end

  describe "set_delay/2" do
    test "updates delay for a source" do
      Lol.RateLimiter.set_delay(:bible_cloud, 100)
      assert :ok = Lol.RateLimiter.acquire(:bible_cloud)
      start = System.monotonic_time(:millisecond)
      assert :ok = Lol.RateLimiter.acquire(:bible_cloud)
      elapsed = System.monotonic_time(:millisecond) - start
      # Should now be ~100ms instead of 500ms
      assert elapsed < 200
    end
  end
end
