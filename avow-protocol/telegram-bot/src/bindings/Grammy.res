// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
// FFI bindings for Grammy Telegram bot framework

type context

module From = {
  type t = {id: int, username: Nullable.t<string>}
}

@get external getFrom: context => option<From.t> = "from"

@send external reply: (context, string, ~options: {..}=?) => promise<unit> = "reply"

type bot

@new @module("https://deno.land/x/grammy@v1.19.2/mod.ts")
external makeBot: string => bot = "Bot"

@send external command: (bot, string, context => promise<unit>) => unit = "command"

@send external catch_: (bot, 'err => unit) => unit = "catch"

type botInfo = {username: string}

type startOptions = {onStart: botInfo => unit}

@send external start: (bot, startOptions) => promise<unit> = "start"

module Api = {
  type t

  @get external api: bot => t = "api"

  @send
  external sendMessage: (t, int, string, ~options: {..}=?) => promise<unit> = "sendMessage"
}
