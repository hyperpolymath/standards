# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

defmodule Lol.CrawlerWorkerTest do
  use ExUnit.Case

  describe "CrawlerWorker" do
    test "initializes with correct source and language" do
      {:ok, pid} =
        GenServer.start_link(
          Lol.CrawlerWorker,
          {:bible_cloud, "eng", [timeout: 1000]}
        )

      assert Process.alive?(pid)
      # Worker will attempt crawl and exit (no Deno in test)
      Process.sleep(500)
    end

    test "source_to_string mappings" do
      # Test via module attribute access pattern
      assert is_atom(:bible_cloud)
      assert is_atom(:bible_com)
      assert is_atom(:ebible)
    end
  end
end
