// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
// FFI bindings for SQLite (deno.land/x/sqlite)

type db

@new @module("https://deno.land/x/sqlite@v3.9.1/mod.ts") external makeDB: string => db = "DB"

@send external execute: (db, string) => unit = "execute"

@send external query: (db, string, array<JSON.t>) => array<array<JSON.t>> = "query"

@send external close: db => unit = "close"
