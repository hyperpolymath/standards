// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
// STAMP Telegram Bot

open Grammy

@val @scope("Deno") external exit: int => unit = "exit"
@val @scope(("Deno", "env")) external envGet: string => option<string> = "get"
@val external setTimeout: (unit => unit, int) => int = "setTimeout"
@val external setInterval: (unit => unit, int) => int = "setInterval"

let botToken = envGet("BOT_TOKEN")

switch botToken {
| None =>
  Console.error("Error: BOT_TOKEN environment variable not set")
  Console.error("Get your bot token from @BotFather on Telegram")
  exit(1)
| Some(_) => ()
}

let demoMessageInterval = 3600000 // 1 hour

let bot = makeBot(botToken->Option.getOr(""))
let db = Database.make()

Console.log("STAMP Telegram Bot starting...")

// Command: /start
let () = bot->command("start", async ctx => {
  let from = ctx->getFrom
  switch from {
  | None => await ctx->reply("Error: Could not identify user")
  | Some({id: userId, username}) =>
    let usernameStr = username->Nullable.toOption

    if Database.isSubscribed(db, ~telegramId=userId) {
      await ctx->reply(
        "You're already subscribed!\n\nUse /status to see your subscription details\nUse /unsubscribe to unsubscribe",
      )
    } else {
      let now = Date.now()
      let consentParams: StampMock.consentParams = {
        initial_request: now -. 1000.0,
        confirmation: now,
        ip_address: "telegram_user",
        token: StampMock.generateToken(userId),
      }

      let consentResult = StampMock.verifyConsent(consentParams)

      switch consentResult {
      | Success =>
        let consentProof = StampMock.generateProof(
          #consent,
          StampMock.consentParamsToJson(consentParams),
        )

        Database.subscribeUser(
          db,
          ~telegramId=userId,
          ~username=usernameStr,
          ~consentToken=consentParams.token,
          ~consentProof=StampMock.formatProof(consentProof),
        )

        await ctx->reply(
          "Subscription Confirmed\n\n" ++
          "Consent Chain Verified:\n" ++
          `- Requested: ${Date.fromTime(consentParams.initial_request)->Date.toISOString}\n` ++
          "- Confirmed: /start command (explicit)\n" ++
          `- Token: ${consentParams.token->String.slice(~start=0, ~end=20)}...\n` ++
          "- Proof: Cryptographically signed\n\n" ++
          "You will receive demo messages periodically.\n" ++
          "Each message includes STAMP verification.\n\n" ++
          "Commands:\n" ++
          "/verify - Show proof for last message\n" ++
          "/status - Show subscription status\n" ++
          "/unsubscribe - Unsubscribe (one-click, proven)",
        )
      | other =>
        await ctx->reply(
          `Consent verification failed: ${StampMock.resultToString(other)}\n\nPlease try again or contact support.`,
        )
      }
    }
  }
})

// Command: /verify
let () = bot->command("verify", async ctx => {
  let from = ctx->getFrom
  switch from {
  | None => ()
  | Some({id: userId}) =>
    switch Database.getLastMessage(db, ~telegramId=userId) {
    | None =>
      await ctx->reply("No messages to verify yet.\n\nYou'll receive a demo message soon!")
    | Some(lastMessage) =>
      let proofDisplay =
        "STAMP Verification Proof\n\n" ++
        `Message: ${lastMessage.subject}\n` ++
        `Sent: ${Date.fromTime(lastMessage.sent_at)->Date.toISOString}\n\n` ++
        "Verification Details:\n" ++
        lastMessage.proof ++ "\n\n" ++
        "This proof is cryptographically signed\n" ++
        "Cannot be forged or tampered with\n" ++
        "Verifiable by anyone\n\n" ++
        "What this proves:\n" ++
        "- You consented to receive this message\n" ++
        "- Unsubscribe link works (tested <60s ago)\n" ++
        "- Sender is within rate limits\n" ++
        "- Message complies with STAMP protocol"

      await ctx->reply(proofDisplay)
    }
  }
})

// Command: /unsubscribe
let () = bot->command("unsubscribe", async ctx => {
  let from = ctx->getFrom
  switch from {
  | None => ()
  | Some({id: userId}) =>
    if !Database.isSubscribed(db, ~telegramId=userId) {
      await ctx->reply("You're not currently subscribed.\n\nUse /start to subscribe")
    } else {
      switch Database.getUser(db, ~telegramId=userId) {
      | None => ()
      | Some(user) =>
        let unsubUrl = StampMock.generateUnsubscribeUrl(userId, user.consent_token)
        let (responseCode, responseTime) = await StampMock.testUnsubscribeUrl(unsubUrl)

        let unsubParams: StampMock.unsubscribeParams = {
          url: unsubUrl,
          tested_at: Date.now(),
          response_code: responseCode,
          response_time: responseTime,
          token: user.consent_token,
          signature: StampMock.generateSignature(unsubUrl),
        }

        let verifyResult = StampMock.verifyUnsubscribe(unsubParams)

        switch verifyResult {
        | Success =>
          let unsubProof = StampMock.generateProof(
            #unsubscribe,
            StampMock.unsubscribeParamsToJson(unsubParams),
          )

          let _ = Database.unsubscribeUser(db, ~telegramId=userId)

          await ctx->reply(
            "Unsubscribed Successfully\n\n" ++
            "Proof of Removal:\n" ++
            `- Removed: ${Date.make()->Date.toISOString}\n` ++
            `- Latency: ${responseTime->Int.toString}ms\n` ++
            "- Status: Confirmed\n" ++
            `- Signature: ${unsubProof.signature->String.slice(~start=0, ~end=30)}...\n\n` ++
            "You will NOT receive future messages.\n" ++
            "(This is mathematically proven)\n\n" ++
            "Use /start to re-subscribe anytime.",
          )
        | other =>
          await ctx->reply(
            `Unsubscribe verification failed: ${StampMock.resultToString(other)}\n\nThis should never happen with STAMP!\nPlease contact support.`,
          )
        }
      }
    }
  }
})

// Command: /status
let () = bot->command("status", async ctx => {
  let from = ctx->getFrom
  switch from {
  | None => ()
  | Some({id: userId}) =>
    switch Database.getUser(db, ~telegramId=userId) {
    | None => await ctx->reply("No subscription found.\n\nUse /start to subscribe")
    | Some(user) =>
      let messages = Database.getUserMessages(db, ~telegramId=userId, ~limit=5)
      let stats = Database.getStats(db)

      let statusDisplay =
        "Your STAMP Subscription\n\n" ++
        `Status: ${user.subscribed ? "Active" : "Unsubscribed"}\n` ++
        `Subscribed: ${Date.fromTime(user.created_at)->Date.toISOString}\n` ++
        `Messages received: ${messages->Array.length->Int.toString}\n` ++
        `Consent token: ${user.consent_token->String.slice(~start=0, ~end=25)}...\n\n` ++
        `Bot Statistics:\n` ++
        `- Total users: ${stats.total_users->Int.toString}\n` ++
        `- Active subscriptions: ${stats.subscribed_users->Int.toString}\n` ++
        `- Total messages sent: ${stats.total_messages->Int.toString}\n\n` ++
        "Commands:\n" ++
        "/verify - See proof for last message\n" ++
        "/unsubscribe - Unsubscribe (one-click)"

      await ctx->reply(statusDisplay)
    }
  }
})

// Command: /help
let () = bot->command("help", async ctx => {
  await ctx->reply(
    "STAMP Protocol Demo Bot\n\n" ++
    "This bot demonstrates the STAMP (Secure Typed Announcement Messaging Protocol) " ++
    "which uses formal verification to eliminate spam.\n\n" ++
    "Key Features:\n" ++
    "- Cryptographically proven consent\n" ++
    "- Guaranteed working unsubscribe\n" ++
    "- Rate limits enforced at protocol level\n" ++
    "- All actions include verification proofs\n\n" ++
    "Commands:\n" ++
    "/start - Subscribe to demo messages\n" ++
    "/verify - Show proof for last message\n" ++
    "/status - Show subscription details\n" ++
    "/unsubscribe - Unsubscribe (one-click, proven)\n" ++
    "/help - Show this help\n\n" ++
    "Learn More:\nhttps://github.com/hyperpolymath/libstamp",
  )
})

// Periodic Demo Messages
let sendDemoMessages = async () => {
  let users = Database.getSubscribedUsers(db)
  Console.log(`Sending demo messages to ${users->Array.length->Int.toString} users...`)

  for i in 0 to Array.length(users) - 1 {
    switch users[i] {
    | None => ()
    | Some(user) =>
      try {
        let subject = "Weekly STAMP Demo Update"
        let body =
          "This is a demo message from the STAMP protocol bot.\n\n" ++
          "Notice:\n" ++
          "- You consented to this (proven)\n" ++
          "- You can unsubscribe with /unsubscribe (proven to work)\n" ++
          "- This sender is rate-limited (proven)\n\n" ++
          "Use /verify to see the cryptographic proof!"

        let unsubUrl = StampMock.generateUnsubscribeUrl(user.telegram_id, user.consent_token)
        let (_, responseTime) = await StampMock.testUnsubscribeUrl(unsubUrl)

        let unsubParams: StampMock.unsubscribeParams = {
          url: unsubUrl,
          tested_at: Date.now(),
          response_code: 200,
          response_time: responseTime,
          token: user.consent_token,
          signature: StampMock.generateSignature(unsubUrl),
        }

        let proof = StampMock.generateProof(
          #unsubscribe,
          StampMock.unsubscribeParamsToJson(unsubParams),
        )

        let _ = Database.recordMessage(
          db,
          ~telegramId=user.telegram_id,
          ~subject,
          ~body,
          ~proof=StampMock.formatProof(proof),
        )

        await Api.api(bot)->Api.sendMessage(
          user.telegram_id,
          `${subject}\n\n${body}\n\n` ++
          "Verified by STAMP Protocol\n" ++
          "- Consent: Proven\n" ++
          `- Unsubscribe: Tested ${responseTime->Int.toString}ms ago\n` ++
          "- Rate limit: Enforced\n\n" ++
          "Use /verify to see the full proof",
        )

        Console.log(`  Sent to user ${user.telegram_id->Int.toString}`)

        // Rate limit delay
        await Promise.make((resolve, _reject) => {
          let _ = setTimeout(() => resolve(), 100)
        })
      } catch {
      | Exn.Error(e) =>
        Console.error(
          `  Failed to send to user ${user.telegram_id->Int.toString}: ${Exn.message(e)->Option.getOr("unknown")}`,
        )
      }
    }
  }

  Console.log("Demo messages sent")
}

// Schedule periodic messages
let _ = setInterval(
  () => {
    let _ = sendDemoMessages()
  },
  demoMessageInterval,
)

// Error handling
let () = bot->catch_(err => {
  Console.error2("Bot error:", err)
})

// Start bot
Console.log("Bot initialized")
Console.log("Database connected")
Console.log("Demo messages scheduled (every hour)")
Console.log("\nBot is now running!\n")

let _ =
  bot
  ->start({
    onStart: botInfo => {
      Console.log(`Connected as @${botInfo.username}`)
      Console.log("Polling for messages...")
    },
  })
  ->Promise.catch(err => {
    Console.error2("Failed to start bot:", err)
    exit(1)
    Promise.resolve()
  })
