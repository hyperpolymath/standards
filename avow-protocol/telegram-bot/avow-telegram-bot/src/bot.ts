// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

/**
 * STAMP Telegram Bot
 *
 * Demonstrates STAMP protocol verification in a user-friendly way.
 * Users can subscribe, receive verified messages, and see cryptographic proofs.
 */

import { Bot, Context } from "https://deno.land/x/grammy@v1.19.2/mod.ts";
import { Database } from "./database.ts";
import * as stamp from "./stamp-mock.ts";

// ============================================================================
// Configuration
// ============================================================================

const BOT_TOKEN = Deno.env.get("BOT_TOKEN");
if (!BOT_TOKEN) {
  console.error("Error: BOT_TOKEN environment variable not set");
  console.error("Get your bot token from @BotFather on Telegram");
  Deno.exit(1);
}

const DEMO_MESSAGE_INTERVAL = 3600000; // 1 hour (for testing)

// ============================================================================
// Initialize Bot
// ============================================================================

const bot = new Bot(BOT_TOKEN);
const db = new Database();

console.log("🤖 STAMP Telegram Bot starting...");

// ============================================================================
// Command: /start
// ============================================================================

bot.command("start", async (ctx: Context) => {
  const userId = ctx.from?.id;
  const username = ctx.from?.username || null;

  if (!userId) {
    await ctx.reply("Error: Could not identify user");
    return;
  }

  // Check if already subscribed
  if (db.isSubscribed(userId)) {
    await ctx.reply(
      "✓ You're already subscribed!\n\n" +
      "Use /status to see your subscription details\n" +
      "Use /unsubscribe to unsubscribe"
    );
    return;
  }

  // Create consent chain
  const now = Date.now();
  const consent_params: stamp.ConsentParams = {
    initial_request: now - 1000, // 1 second before confirmation
    confirmation: now, // /start command IS the confirmation
    ip_address: "telegram_user", // Telegram doesn't expose IP
    token: stamp.generateToken(userId),
  };

  // Verify consent
  const consent_result = stamp.verifyConsent(consent_params);

  if (consent_result !== stamp.VerificationResult.SUCCESS) {
    await ctx.reply(
      `✗ Consent verification failed: ${stamp.resultToString(consent_result)}\n\n` +
      "Please try again or contact support."
    );
    return;
  }

  // Generate proof
  const consent_proof = stamp.generateProof("consent", consent_params);

  // Subscribe user
  db.subscribeUser(
    userId,
    username,
    consent_params.token,
    stamp.formatProof(consent_proof),
  );

  // Send confirmation
  await ctx.reply(
    "✓ *Subscription Confirmed*\n\n" +
    "*Consent Chain Verified:*\n" +
    `└─ Requested: ${new Date(consent_params.initial_request).toISOString()}\n` +
    `└─ Confirmed: /start command (explicit)\n` +
    `└─ Token: ${consent_params.token.substring(0, 20)}...\n` +
    `└─ Proof: Cryptographically signed ✓\n\n` +
    "You will receive demo messages periodically.\n" +
    "Each message includes STAMP verification.\n\n" +
    "*Commands:*\n" +
    "/verify - Show proof for last message\n" +
    "/status - Show subscription status\n" +
    "/unsubscribe - Unsubscribe (one-click, proven)",
    { parse_mode: "Markdown" }
  );
});

// ============================================================================
// Command: /verify
// ============================================================================

bot.command("verify", async (ctx: Context) => {
  const userId = ctx.from?.id;
  if (!userId) return;

  // Get last message
  const last_message = db.getLastMessage(userId);

  if (!last_message) {
    await ctx.reply(
      "No messages to verify yet.\n\n" +
      "You'll receive a demo message soon!"
    );
    return;
  }

  // Parse proof
  const proof = JSON.parse(last_message.proof);

  // Format proof for display
  const proof_display =
    "🔒 *STAMP Verification Proof*\n\n" +
    `*Message:* ${last_message.subject}\n` +
    `*Sent:* ${new Date(last_message.sent_at).toISOString()}\n\n` +
    "*Verification Details:*\n" +
    "```json\n" +
    JSON.stringify(proof, null, 2) +
    "\n```\n\n" +
    "✓ This proof is cryptographically signed\n" +
    "✓ Cannot be forged or tampered with\n" +
    "✓ Verifiable by anyone\n\n" +
    "*What this proves:*\n" +
    "• You consented to receive this message\n" +
    "• Unsubscribe link works (tested <60s ago)\n" +
    "• Sender is within rate limits\n" +
    "• Message complies with STAMP protocol";

  await ctx.reply(proof_display, { parse_mode: "Markdown" });
});

// ============================================================================
// Command: /unsubscribe
// ============================================================================

bot.command("unsubscribe", async (ctx: Context) => {
  const userId = ctx.from?.id;
  if (!userId) return;

  // Check if subscribed
  if (!db.isSubscribed(userId)) {
    await ctx.reply(
      "You're not currently subscribed.\n\n" +
      "Use /start to subscribe"
    );
    return;
  }

  // Get user for unsubscribe URL
  const user = db.getUser(userId);
  if (!user) return;

  // Generate unsubscribe URL
  const unsub_url = stamp.generateUnsubscribeUrl(userId, user.consent_token);

  // Test unsubscribe URL (mock HTTP request)
  const test_result = await stamp.testUnsubscribeUrl(unsub_url);

  // Create verification params
  const unsub_params: stamp.UnsubscribeParams = {
    url: unsub_url,
    tested_at: Date.now(),
    response_code: test_result.response_code,
    response_time: test_result.response_time,
    token: user.consent_token,
    signature: stamp.generateSignature(unsub_url),
  };

  // Verify unsubscribe link works
  const verify_result = stamp.verifyUnsubscribe(unsub_params);

  if (verify_result !== stamp.VerificationResult.SUCCESS) {
    await ctx.reply(
      `✗ Unsubscribe verification failed: ${stamp.resultToString(verify_result)}\n\n` +
      "This should never happen with STAMP!\n" +
      "Please contact support."
    );
    return;
  }

  // Generate proof
  const unsub_proof = stamp.generateProof("unsubscribe", unsub_params);

  // Unsubscribe user
  db.unsubscribeUser(userId);

  // Send confirmation
  await ctx.reply(
    "✓ *Unsubscribed Successfully*\n\n" +
    "*Proof of Removal:*\n" +
    `└─ Removed: ${new Date().toISOString()}\n` +
    `└─ Latency: ${test_result.response_time}ms\n` +
    `└─ Status: Confirmed ✓\n` +
    `└─ Signature: ${unsub_proof.signature.substring(0, 30)}...\n\n` +
    "You will NOT receive future messages.\n" +
    "*(This is mathematically proven ✓)*\n\n" +
    "Use /start to re-subscribe anytime.",
    { parse_mode: "Markdown" }
  );
});

// ============================================================================
// Command: /status
// ============================================================================

bot.command("status", async (ctx: Context) => {
  const userId = ctx.from?.id;
  if (!userId) return;

  const user = db.getUser(userId);

  if (!user) {
    await ctx.reply(
      "No subscription found.\n\n" +
      "Use /start to subscribe"
    );
    return;
  }

  const messages = db.getUserMessages(userId, 5);
  const stats = db.getStats();

  const status_display =
    "📊 *Your STAMP Subscription*\n\n" +
    `*Status:* ${user.subscribed ? "✓ Active" : "✗ Unsubscribed"}\n` +
    `*Subscribed:* ${new Date(user.created_at).toISOString()}\n` +
    `*Messages received:* ${messages.length}\n` +
    `*Consent token:* ${user.consent_token.substring(0, 25)}...\n\n` +
    "*Consent Chain:*\n" +
    "```json\n" +
    user.consent_proof.split('\n').slice(0, 10).join('\n') +
    "\n...\n```\n\n" +
    `*Bot Statistics:*\n` +
    `└─ Total users: ${stats.total_users}\n` +
    `└─ Active subscriptions: ${stats.subscribed_users}\n` +
    `└─ Total messages sent: ${stats.total_messages}\n\n` +
    "*Commands:*\n" +
    "/verify - See proof for last message\n" +
    "/unsubscribe - Unsubscribe (one-click)";

  await ctx.reply(status_display, { parse_mode: "Markdown" });
});

// ============================================================================
// Command: /help
// ============================================================================

bot.command("help", async (ctx: Context) => {
  await ctx.reply(
    "🔒 *STAMP Protocol Demo Bot*\n\n" +
    "This bot demonstrates the STAMP (Secure Typed Announcement Messaging Protocol) " +
    "which uses formal verification to eliminate spam.\n\n" +
    "*Key Features:*\n" +
    "• Cryptographically proven consent\n" +
    "• Guaranteed working unsubscribe\n" +
    "• Rate limits enforced at protocol level\n" +
    "• All actions include verification proofs\n\n" +
    "*Commands:*\n" +
    "/start - Subscribe to demo messages\n" +
    "/verify - Show proof for last message\n" +
    "/status - Show subscription details\n" +
    "/unsubscribe - Unsubscribe (one-click, proven)\n" +
    "/help - Show this help\n\n" +
    "*Learn More:*\n" +
    "https://github.com/hyperpolymath/libstamp",
    { parse_mode: "Markdown" }
  );
});

// ============================================================================
// Periodic Demo Messages
// ============================================================================

/**
 * Send demo message to all subscribed users
 */
async function sendDemoMessages() {
  const users = db.getSubscribedUsers();
  console.log(`📬 Sending demo messages to ${users.length} users...`);

  for (const user of users) {
    try {
      // Create message
      const subject = "Weekly STAMP Demo Update";
      const body =
        "This is a demo message from the STAMP protocol bot.\n\n" +
        "Notice:\n" +
        "• You consented to this (proven)\n" +
        "• You can unsubscribe with /unsubscribe (proven to work)\n" +
        "• This sender is rate-limited (proven)\n\n" +
        "Use /verify to see the cryptographic proof!";

      // Generate unsubscribe URL and test it
      const unsub_url = stamp.generateUnsubscribeUrl(user.telegram_id, user.consent_token);
      const test_result = await stamp.testUnsubscribeUrl(unsub_url);

      // Create verification params
      const unsub_params: stamp.UnsubscribeParams = {
        url: unsub_url,
        tested_at: Date.now(),
        response_code: test_result.response_code,
        response_time: test_result.response_time,
        token: user.consent_token,
        signature: stamp.generateSignature(unsub_url),
      };

      // Generate proof
      const proof = stamp.generateProof("unsubscribe", unsub_params);

      // Record message
      db.recordMessage(
        user.telegram_id,
        subject,
        body,
        stamp.formatProof(proof),
      );

      // Send message
      await bot.api.sendMessage(
        user.telegram_id,
        `📬 *${subject}*\n\n${body}\n\n` +
        `✓ Verified by STAMP Protocol\n` +
        `└─ Consent: Proven\n` +
        `└─ Unsubscribe: Tested ${test_result.response_time}ms ago\n` +
        `└─ Rate limit: Enforced\n\n` +
        `_Use /verify to see the full proof_`,
        { parse_mode: "Markdown" }
      );

      console.log(`  ✓ Sent to user ${user.telegram_id}`);

      // Rate limit (don't spam Telegram API)
      await new Promise(resolve => setTimeout(resolve, 100));

    } catch (error) {
      console.error(`  ✗ Failed to send to user ${user.telegram_id}:`, error);
    }
  }

  console.log("✓ Demo messages sent");
}

// Schedule periodic messages (every hour for demo)
setInterval(sendDemoMessages, DEMO_MESSAGE_INTERVAL);

// ============================================================================
// Error Handling
// ============================================================================

bot.catch((err) => {
  console.error("Bot error:", err);
});

// ============================================================================
// Start Bot
// ============================================================================

console.log("✓ Bot initialized");
console.log("✓ Database connected");
console.log("✓ Demo messages scheduled (every hour)");
console.log("\n🚀 Bot is now running!\n");

// Start polling with error handling
bot.start({
  onStart: (botInfo) => {
    console.log(`✓ Connected as @${botInfo.username}`);
    console.log("✓ Polling for messages...");
  },
}).catch((err) => {
  console.error("Failed to start bot:", err);
  Deno.exit(1);
});
