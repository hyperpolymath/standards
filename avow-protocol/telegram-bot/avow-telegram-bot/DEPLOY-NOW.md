# Deploy Your STAMP Bot RIGHT NOW

## ✅ Everything is ready! Follow these steps:

### Step 1: Get Your Bot Token (2 minutes)

1. Open Telegram
2. Search for: `@BotFather`
3. Send: `/newbot`
4. When asked for name, reply: `STAMP Demo Bot`
5. When asked for username, reply: `stamp_demo_YOUR_NAME_bot` (must be unique and end in "bot")
6. Copy the token (looks like: `1234567890:ABCdef-GHIjklMNOpqrs`)

### Step 2: Configure (30 seconds)

```bash
cd ~/Documents/hyperpolymath-repos/stamp-telegram-bot

# Create config file with your token
echo "BOT_TOKEN=YOUR_TOKEN_HERE" > .env

# Example:
# echo "BOT_TOKEN=1234567890:ABCdef-GHIjklMNOpqrs" > .env
```

### Step 3: Run (10 seconds)

```bash
deno task start
```

You should see:
```
🤖 STAMP Telegram Bot starting...
✓ Bot initialized
✓ Database connected
✓ Demo messages scheduled (every hour)

🚀 Bot is now running!
```

### Step 4: Test (1 minute)

1. Open Telegram
2. Search for your bot username (e.g., `@stamp_demo_YOUR_NAME_bot`)
3. Send: `/start`

You should get:
```
✓ Subscription Confirmed

Consent Chain Verified:
└─ Requested: 2026-01-30...
└─ Confirmed: /start command (explicit)
└─ Token: ...
└─ Proof: Cryptographically signed ✓
```

**Try these commands:**
- `/verify` - See cryptographic proof
- `/status` - Show subscription details
- `/unsubscribe` - Unsubscribe (one-click)

### Step 5: Keep It Running (Optional)

**Option A: Leave terminal open**
Just don't close the terminal window

**Option B: Use `screen` (recommended)**
```bash
# Start screen session
screen -S stamp-bot

# Run bot
deno task start

# Detach: Press Ctrl+A, then D
# Bot keeps running in background

# Later: Reattach with
screen -r stamp-bot
```

---

## ✅ Success Checklist

- [ ] Bot responds to `/start`
- [ ] Bot shows consent proof
- [ ] `/verify` command works
- [ ] `/unsubscribe` command works
- [ ] Bot stays running

---

## 🎉 Once Working:

1. **Test with friends** - Share bot link with 3-5 people
2. **Record demo video** - Screen record the `/start`, `/verify`, `/unsubscribe` flow (2 min)
3. **Take screenshots** - For pitch deck

---

## ⚠️ If Something Goes Wrong:

**Bot doesn't start:**
```bash
# Check Deno is installed
deno --version

# If not installed:
curl -fsSL https://deno.land/install.sh | sh
```

**Bot doesn't respond:**
- Check BOT_TOKEN in `.env` is correct
- Check bot username matches what you set
- Try stopping (Ctrl+C) and restarting

**Permission errors:**
```bash
# Give write permissions
chmod +x ~/Documents/hyperpolymath-repos/stamp-telegram-bot/db
```

---

## 📝 What to Tell People When Demoing:

> "This is a demo of the STAMP protocol - it uses formal verification to prove:
>
> 1. You actually consented to receive messages
> 2. The unsubscribe link works (tested and proven)
> 3. The sender is rate-limited
>
> Try it: /start to subscribe, /verify to see the proof, /unsubscribe to leave"

---

## Next: Buy Domain

While bot is running, buy: **`stamp-protocol.org`** (£10/year)

Go to: https://www.cloudflare.com/products/registrar/

---

**Ready? Run these commands now:**

```bash
cd ~/Documents/hyperpolymath-repos/stamp-telegram-bot
echo "BOT_TOKEN=YOUR_TOKEN_HERE" > .env
deno task start
```

**Then message me when it's working!** 🚀
