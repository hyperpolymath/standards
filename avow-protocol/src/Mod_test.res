// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

@module("@std/assert") external assertEquals: ('a, 'a) => unit = "assertEquals"

@val @scope("Deno") external test: (string, unit => unit) => unit = "test"

let () = test("addTest", () => {
  assertEquals(Mod.add(2.0, 3.0), 5.0)
})
