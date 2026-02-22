# Building libstamp

## Current Status

**Core Idris2 types**: ✓ Complete (`src/abi/`)
**Zig FFI implementation**: ✓ Complete (`ffi/zig/`)
**Build system**: ⚠️  Zig version compatibility issue

## The Zig Version Issue

The FFI code is written for **Zig 0.13.0**, but your system has **Zig 0.15.2** installed.

Zig 0.15 is a development version with breaking API changes. The build system API is completely different.

## Option 1: Downgrade to Zig 0.13.0 (Recommended)

```bash
# You have asdf installed, so:
asdf install zig 0.13.0
cd ~/Documents/hyperpolymath-repos/libstamp
asdf local zig 0.13.0

# Now build works:
cd ffi/zig
zig build          # Build library
zig build test     # Run tests
zig build example  # Run example
```

## Option 2: Skip FFI for MVP

For the **Telegram bot demo** (your Week 1 goal), you don't actually need the full FFI yet.

**What you can do instead:**

1. **Mock the verification functions** in TypeScript/ReScript
2. Build the Telegram bot with mocked verification
3. Prove the UX works
4. Come back and integrate real FFI later

This gets you to a working demo **faster**.

### Mock Implementation (TypeScript for Deno)

```typescript
// mock-stamp.ts - Temporary until Zig FFI is built

export interface UnsubscribeParams {
  url: string;
  tested_at: number;
  response_code: number;
  response_time: number;
  token: string;
  signature: string;
}

export function verifyUnsubscribe(params: UnsubscribeParams): boolean {
  // Mock implementation - checks basic properties
  if (!params.url.startsWith('https://')) return false;
  if (params.response_code !== 200) return false;
  if (params.response_time >= 200) return false;

  const now = Date.now();
  const age = now - params.tested_at;
  if (age > 60000) return false; // > 60 seconds old

  return true;
}

export function generateProof(params: UnsubscribeParams): string {
  return JSON.stringify({
    type: "unsubscribe_verification",
    url: params.url,
    tested_at: params.tested_at,
    response_code: params.response_code,
    response_time_ms: params.response_time,
    verified: true,
    timestamp: Date.now(),
  }, null, 2);
}
```

This lets you:
- ✓ Build Telegram bot **now**
- ✓ Demo the UX **this week**
- ✓ Show investors/users
- → Add real FFI later (Week 2-3)

## Option 3: Update build.zig for Zig 0.15

If you want to use Zig 0.15, I can research the new API and update `build.zig`. But this will take time.

## My Recommendation

**For Week 1 (this week):**
- Use **Option 2** (mock implementation)
- Build Telegram bot with mocked verification
- Get feedback from users

**For Week 2:**
- Install Zig 0.13.0 (Option 1)
- Build real FFI
- Replace mocks with real verification

**Why:** Gets you to a demo **faster**. The UX is more important than the implementation for early feedback.

## Next Steps

Tell me which option you want:

1. **"Help me downgrade Zig"** → I'll guide you through asdf setup
2. **"Let's use mocks for now"** → I'll help you build the Telegram bot immediately
3. **"Fix build.zig for 0.15"** → I'll research and update (takes longer)

What's your priority?
