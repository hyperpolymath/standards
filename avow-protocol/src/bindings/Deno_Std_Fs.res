// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
// FFI bindings for @std/fs (Deno standard library)

type walkEntry = {
  path: string,
  name: string,
  isFile: bool,
  isDirectory: bool,
  isSymlink: bool,
}

type walkOptions = {exts?: array<string>}

// walk returns an async iterable; collect into array via for-await in JS glue
@module("@std/fs") external walk: (string, walkOptions) => 'asyncIterable = "walk"
