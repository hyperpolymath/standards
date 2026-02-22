// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
// FFI bindings for Fetch API (Cloudflare API calls)

type headers = {
  @as("Authorization") authorization: string,
  @as("Content-Type") contentType: string,
}

type requestInit = {
  method?: string,
  headers: headers,
  body?: string,
}

type response = {
  ok: bool,
  status: int,
}

@send external json: response => promise<{..}> = "json"

@val external fetch: (string, requestInit) => promise<response> = "fetch"

@val external fetchGet: string => promise<response> = "fetch"
