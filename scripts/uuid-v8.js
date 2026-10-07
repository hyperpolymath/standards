#!/usr/bin/env bun
// SPDX-License-Identifier: MPL-2.0
// ESTATE-UUID-V8 generator (ADR-008): the one shared bun implementation of
// both profiles. Import profileT / profileC, or run it:
//
//   bun scripts/uuid-v8.js t                  # profile T: time-ordered
//   bun scripts/uuid-v8.js c <domain> <name>  # profile C: SHA-256(domain ":" name)
//
// Profile T takes a library v7 and sets the version nibble to 8, the only bit
// operation ADR-008 permits. Profile C is bytes 0..15 of SHA-256 of the
// preimage, with the version nibble set to 8 and the variant bits to 10. A deed
// `#u8"domain:name"` literal names exactly the profile C preimage.

const HEX = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/;

/** Format 16 bytes as an 8-4-4-4-12 lower-case UUID string. */
function format(bytes) {
  const h = Array.from(bytes, (b) => b.toString(16).padStart(2, "0")).join("");
  return `${h.slice(0, 8)}-${h.slice(8, 12)}-${h.slice(12, 16)}-${h.slice(16, 20)}-${h.slice(20, 32)}`;
}

/** Return a fresh profile T id: a v7 from Bun with its version nibble set to 8. */
export function profileT() {
  const v7 = Bun.randomUUIDv7();
  if (!HEX.test(v7) || v7[14] !== "7") throw new Error(`Bun.randomUUIDv7 returned a non-v7 value: ${v7}`);
  return v7.slice(0, 14) + "8" + v7.slice(15);
}

/**
 * Return the profile C id for (domain, name). The domain must be non-empty
 * printable ASCII without ":" (ADR-008), so the first ":" of the preimage
 * always ends it; the name is unconstrained.
 */
export function profileC(domain, name) {
  if (typeof domain !== "string" || !/^[\x21-\x39\x3b-\x7e]+$/.test(domain)) {
    throw new Error(`profile C domain must be non-empty printable ASCII without ":" (got ${JSON.stringify(domain)})`);
  }
  const digest = new Bun.CryptoHasher("sha256").update(`${domain}:${name}`).digest();
  const b = new Uint8Array(digest.buffer, digest.byteOffset, 16).slice();
  b[6] = (b[6] & 0x0f) | 0x80; // version 8
  b[8] = (b[8] & 0x3f) | 0x80; // variant 10
  return format(b);
}

/** Return the profile C id named by a deed `#u8` body ("domain:name"). */
export function fromDeedBody(body) {
  const i = body.indexOf(":");
  if (i < 0) throw new Error(`#u8 body has no ":" (got ${JSON.stringify(body)})`);
  return profileC(body.slice(0, i), body.slice(i + 1));
}

/** Command-line entry point; returns a process exit code. */
function main(argv) {
  const [mode, ...rest] = argv;
  try {
    if (mode === "t" && rest.length === 0) console.log(profileT());
    else if (mode === "c" && rest.length === 2) console.log(profileC(rest[0], rest[1]));
    else if (mode === "deed" && rest.length === 1) console.log(fromDeedBody(rest[0]));
    else {
      console.error("usage: uuid-v8.js t | c <domain> <name> | deed <domain:name>");
      return 2;
    }
  } catch (e) {
    console.error(`uuid-v8: ${e.message}`);
    return 1;
  }
  return 0;
}

if (import.meta.main) process.exit(main(process.argv.slice(2)));
