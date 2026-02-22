// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
// FFI bindings for Deno runtime APIs

module Env = {
  @val @scope(("Deno", "env")) external get: string => option<string> = "get"
}

@val @scope("Deno") external exit: int => unit = "exit"

@val @scope("Deno") external readTextFile: string => promise<string> = "readTextFile"

@val @scope("Deno") external writeTextFile: (string, string) => promise<unit> = "writeTextFile"

type fileInfo = {isFile: bool, isDirectory: bool, isSymlink: bool}
@val @scope("Deno") external stat: string => promise<fileInfo> = "stat"

type commandOptions = {
  args: array<string>,
  stdout: string,
  stderr: string,
  cwd?: string,
}

type commandOutput = {
  success: bool,
  code: int,
  stdout: Js_typed_array2.Uint8Array.t,
  stderr: Js_typed_array2.Uint8Array.t,
}

type command

@new @scope("Deno") external makeCommand: (string, commandOptions) => command = "Command"

@send external output: command => promise<commandOutput> = "output"
