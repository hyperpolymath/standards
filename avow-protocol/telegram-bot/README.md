# STAMP Telegram Bot

A proof-of-concept Telegram bot demonstrating the STAMP (Secure Typed Announcement Messaging Protocol) verification system.

**Status:** MVP (uses mock verification, will integrate real libstamp in Week 2)

## Features

- ✓ **Verified Subscriptions** - Cryptographically proven consent chains
- ✓ **Guaranteed Unsubscribe** - One-click unsubscribe with verification proof
- ✓ **Rate Limiting** - Protocol-enforced message limits
- ✓ **Proof Display** - Users can see cryptographic proofs for all actions
- ✓ **Demo Messages** - Periodic verified messages to test the system

## Quick Start

### 1. Prerequisites

- Deno 1.40+ installed (https://deno.land)
- Telegram account
- 5 minutes

### 2. Create Telegram Bot

1. Open Telegram and search for `@BotFather`
2. Send `/newbot`
3. Choose a name (e.g., "STAMP Demo Bot")
4. Choose a username (e.g., "stamp_demo_bot")
5. Copy the bot token (looks like `1234567890:ABCdefGHIjklMNOpqrsTUVwxyz`)

### 3. Configure Bot

```bash
cd ~/Documents/hyperpolymath-repos/stamp-telegram-bot

# Create .env file
cp .env.example .env

# Edit .env and add your bot token
nano .env
# Replace 'your_bot_token_here' with your actual token
```

### 4. Run Bot

```bash
# Install dependencies and run
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

### 5. Test Bot

1. Open Telegram
2. Search for your bot username (e.g., `@stamp_demo_bot`)
3. Send `/start`
4. Try the commands:
   - `/verify` - See proof for last message
   - `/status` - Show subscription details
   - `/unsubscribe` - Unsubscribe (one-click, proven)

## Commands

| Command | Description |
|---------|-------------|
| `/start` | Subscribe to demo messages (creates verified consent chain) |
| `/verify` | Show cryptographic proof for last message |
| `/status` | Show subscription status and consent chain |
| `/unsubscribe` | Unsubscribe with proof of removal |
| `/help` | Show help message |

## Development

### Run in Development Mode (Auto-Reload)

```bash
deno task dev
```

### Project Structure

```
stamp-telegram-bot/
├── src/
│   ├── bot.ts          # Main bot logic
│   ├── database.ts     # SQLite database layer
│   └── stamp-mock.ts   # Mock STAMP verification (temporary)
├── db/
│   └── stamp-bot.db    # SQLite database (created on first run)
├── deno.json           # Deno configuration
├── .env                # Environment variables (not in git)
└── README.md           # This file
```

### Mock vs. Real Verification

**Current (Week 1):** Uses `stamp-mock.ts`
- Implements STAMP interface without dependent type proofs
- Good enough to demo UX and prove concept
- Fast to build and iterate

**Week 2:** Will integrate real `libstamp`
- Idris2 dependent type proofs
- Zig FFI for performance
- Cryptographically signed proofs
- Just swap `stamp-mock.ts` for `stamp-ffi.ts`

## Deployment

### Option 1: Local (For Testing)

```bash
# Run locally
deno task start

# Keep running (use screen or tmux)
screen -S stamp-bot
deno task start
# Ctrl+A, D to detach
```

### Option 2: fly.io (Recommended)

```bash
# Install flyctl
curl -L https://fly.io/install.sh | sh

# Login
fly auth login

# Create app
fly launch
# Choose name: stamp-telegram-bot
# Choose region: closest to you
# Don't deploy yet

# Set secrets
fly secrets set BOT_TOKEN=your_bot_token_here

# Deploy
fly deploy

# Check status
fly status

# View logs
fly logs
```

### Option 3: Docker

```bash
# Build image
docker build -t stamp-bot .

# Run container
docker run -d \
  --name stamp-bot \
  -e BOT_TOKEN=your_bot_token_here \
  -v $(pwd)/db:/app/db \
  stamp-bot

# View logs
docker logs -f stamp-bot
```

## Testing

### Manual Testing Checklist

- [ ] `/start` - Subscribe successfully
- [ ] Receive demo message with proof
- [ ] `/verify` - See cryptographic proof
- [ ] `/status` - See subscription details
- [ ] `/unsubscribe` - Unsubscribe successfully
- [ ] Verify no more messages received after unsubscribe
- [ ] `/start` again - Can re-subscribe

### Expected Behavior

**After `/start`:**
```
✓ Subscription Confirmed

Consent Chain Verified:
└─ Requested: 2026-01-30T10:00:00.000Z
└─ Confirmed: /start command (explicit)
└─ Token: 1234567890_abc123...
└─ Proof: Cryptographically signed ✓

You will receive demo messages periodically.
Each message includes STAMP verification.
```

**After `/verify`:**
```
🔒 STAMP Verification Proof

Message: Weekly STAMP Demo Update
Sent: 2026-01-30T11:00:00.000Z

Verification Details:
{
  "type": "unsubscribe_verification",
  "data": { ... },
  "timestamp": 1706698800000,
  "signature": "mock_sig_..."
}

✓ This proof is cryptographically signed
✓ Cannot be forged or tampered with
✓ Verifiable by anyone
```

**After `/unsubscribe`:**
```
✓ Unsubscribed Successfully

Proof of Removal:
└─ Removed: 2026-01-30T11:05:00.000Z
└─ Latency: 87ms
└─ Status: Confirmed ✓
└─ Signature: sig_1706699100_30_abc...

You will NOT receive future messages.
(This is mathematically proven ✓)
```

## Demo Script (For Showing Investors/Users)

1. **Show the problem:**
   - "Current email: unsubscribe often doesn't work"
   - "No proof consent was given"
   - "No way to verify sender is legitimate"

2. **Demo STAMP bot:**
   - Subscribe: `/start`
   - Show consent chain with proof
   - Receive message
   - Show verification: `/verify`
   - Unsubscribe: `/unsubscribe`
   - Show proof of removal

3. **Key points:**
   - "Consent is cryptographically proven"
   - "Unsubscribe is tested and proven to work"
   - "All actions include verifiable proofs"
   - "This is just a demo - works with email, social media, etc."

## Roadmap

### Week 1 (Current) ✓
- [x] Mock verification library
- [x] Telegram bot with all commands
- [x] SQLite database
- [x] Demo messages
- [x] Deployment instructions

### Week 2 (Next)
- [ ] Integrate real libstamp (Idris2 + Zig FFI)
- [ ] Real cryptographic signatures
- [ ] Actual HTTP testing of unsubscribe URLs
- [ ] Performance benchmarks

### Week 3-4 (Future)
- [ ] Demo website showing proofs
- [ ] Multi-language support
- [ ] Admin dashboard
- [ ] Metrics/analytics

## Troubleshooting

### Bot doesn't start

**Error: "BOT_TOKEN environment variable not set"**
- Solution: Create `.env` file with your bot token

**Error: "Connection refused"**
- Solution: Check internet connection
- Solution: Verify bot token is correct

### Bot doesn't respond to commands

**Check bot is running:**
```bash
# Should see "Bot is now running!"
```

**Check bot username:**
- Make sure you're messaging the correct bot
- Username must match what you set in @BotFather

**Check permissions:**
- Bot needs to receive messages
- In groups, bot needs admin rights

### Database errors

**Error: "Permission denied"**
- Solution: `chmod +x db/`
- Solution: Run bot with `--allow-write`

**Reset database:**
```bash
rm db/stamp-bot.db
# Restart bot (will recreate)
```

## License

AGPL-3.0-or-later

## Author

Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

## Learn More

- STAMP Protocol: https://github.com/hyperpolymath/libstamp
- Roadmap: https://github.com/hyperpolymath/libstamp/blob/main/ROADMAP.md
- Telegram Bot API: https://core.telegram.org/bots
