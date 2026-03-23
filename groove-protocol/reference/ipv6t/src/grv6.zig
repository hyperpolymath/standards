// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// grv6.zig — GRV6 typed payload frame implementation.
//
// The core of IPv6T: a 105-byte frame header that embeds type metadata,
// capability hashes, and provenance chain links inside standard TCP payloads.
// Passes through every middlebox on the internet because it is application
// data, not a protocol extension.
//
// This file provides:
//   - Frame header struct (packed, zero-copy readable)
//   - Frame writer (construct + send)
//   - Frame reader (detect + parse + validate)
//   - Type hash computation (SHA-256 of A2ML type signature)
//   - Provenance chain management

const std = @import("std");
const crypto = std.crypto.hash.sha2;
const mem = std.mem;
const net = std.net;

// =========================================================================
// Constants
// =========================================================================

/// GRV6 magic number: ASCII "GRV6" = 0x47, 0x52, 0x56, 0x36
pub const MAGIC: [4]u8 = .{ 0x47, 0x52, 0x56, 0x36 };

/// Current protocol version.
pub const VERSION: u8 = 1;

/// Header size in bytes (fixed).
pub const HEADER_SIZE: usize = 108;

/// SHA-256 hash size.
pub const HASH_SIZE: usize = 32;

/// Zero hash (genesis / no previous provenance).
pub const ZERO_HASH: [HASH_SIZE]u8 = .{0} ** HASH_SIZE;

// =========================================================================
// Flags
// =========================================================================

/// GRV6 frame flags (bitfield).
pub const Flags = packed struct {
    typed: bool = false,      // bit 0: payload conforms to declared type
    proven: bool = false,     // bit 1: sender compiled with Idris2 proofs
    attested: bool = false,   // bit 2: provenance chain exists
    compressed: bool = false, // bit 3: payload is compressed
    _reserved: u4 = 0,       // bits 4-7: reserved
};

/// Trust level derived from flags.
pub const TrustLevel = enum {
    untyped,     // no GRV6 header at all
    declared,    // TYPED only
    attested,    // TYPED + ATTESTED
    verified,    // TYPED + ATTESTED + structural match (set by receiver)
    proven,      // TYPED + PROVEN (+ optional ATTESTED)
};

/// Derive the trust level from flags.
pub fn trustLevel(flags: Flags) TrustLevel {
    if (flags.proven) return .proven;
    if (flags.attested) return .attested;
    if (flags.typed) return .declared;
    return .untyped;
}

// =========================================================================
// Frame Header
// =========================================================================

/// GRV6 frame header. 108 bytes, naturally aligned.
/// Can be read from a buffer via @ptrCast for zero-copy parsing.
pub const FrameHeader = extern struct {
    magic: [4]u8,
    version: u8,
    flags: u8,
    reserved: [2]u8,
    type_hash: [HASH_SIZE]u8,
    cap_hash: [HASH_SIZE]u8,
    prov_hash: [HASH_SIZE]u8,
    payload_length: u32,

    /// Check if the magic bytes are correct.
    pub fn isValid(self: *const FrameHeader) bool {
        return mem.eql(u8, &self.magic, &MAGIC) and self.version == VERSION;
    }

    /// Get the flags as a structured bitfield.
    pub fn getFlags(self: *const FrameHeader) Flags {
        return @bitCast(self.flags);
    }

    /// Get the payload length (converting from big-endian).
    pub fn getPayloadLength(self: *const FrameHeader) u32 {
        return mem.bigToNative(u32, self.payload_length);
    }

    /// Get the trust level of this frame.
    pub fn getTrustLevel(self: *const FrameHeader) TrustLevel {
        return trustLevel(self.getFlags());
    }
};

// =========================================================================
// Type Hash Computation
// =========================================================================

/// Compute a SHA-256 type hash from an A2ML type signature string.
/// In production Idris2 builds, this is computed at comptime.
/// This function provides the runtime equivalent for polyglot participants.
pub fn computeTypeHash(a2ml_type_sig: []const u8) [HASH_SIZE]u8 {
    var hash: [HASH_SIZE]u8 = undefined;
    crypto.Sha256.hash(a2ml_type_sig, &hash, .{});
    return hash;
}

/// Compute a SHA-256 hash of a capability manifest.
pub fn computeCapHash(manifest: []const u8) [HASH_SIZE]u8 {
    var hash: [HASH_SIZE]u8 = undefined;
    crypto.Sha256.hash(manifest, &hash, .{});
    return hash;
}

/// Compute the next provenance hash in the chain.
/// Hash = SHA-256(type_hash || cap_hash || timestamp || payload_hash || prev_hash)
pub fn computeProvHash(
    type_hash: [HASH_SIZE]u8,
    cap_hash: [HASH_SIZE]u8,
    timestamp_ns: i128,
    payload: []const u8,
    prev_hash: [HASH_SIZE]u8,
) [HASH_SIZE]u8 {
    var hasher = crypto.Sha256.init(.{});
    hasher.update(&type_hash);
    hasher.update(&cap_hash);
    hasher.update(mem.asBytes(&timestamp_ns));
    // Hash the payload itself to bind provenance to content
    var payload_hash: [HASH_SIZE]u8 = undefined;
    crypto.Sha256.hash(payload, &payload_hash, .{});
    hasher.update(&payload_hash);
    hasher.update(&prev_hash);
    var result: [HASH_SIZE]u8 = undefined;
    hasher.final(&result);
    return result;
}

// =========================================================================
// Frame Writer
// =========================================================================

/// Configuration for writing GRV6 frames.
pub const WriterConfig = struct {
    type_hash: [HASH_SIZE]u8,
    cap_hash: [HASH_SIZE]u8,
    flags: Flags = .{ .typed = true },
};

/// A GRV6 frame writer that maintains provenance chain state.
pub const FrameWriter = struct {
    config: WriterConfig,
    prev_prov_hash: [HASH_SIZE]u8,
    frames_written: u64,

    const Self = @This();

    /// Create a new frame writer.
    pub fn init(config: WriterConfig) Self {
        return .{
            .config = config,
            .prev_prov_hash = ZERO_HASH,
            .frames_written = 0,
        };
    }

    /// Write a GRV6 frame header + payload to a stream.
    /// Returns the provenance hash of this frame.
    pub fn writeFrame(self: *Self, stream: net.Stream, payload: []const u8) !ProvenanceRecord {
        const timestamp = std.time.nanoTimestamp();

        // Compute provenance hash
        var prov_hash = ZERO_HASH;
        const flags = self.config.flags;
        if (flags.attested) {
            prov_hash = computeProvHash(
                self.config.type_hash,
                self.config.cap_hash,
                timestamp,
                payload,
                self.prev_prov_hash,
            );
        }

        // Build header
        var header: FrameHeader = .{
            .magic = MAGIC,
            .version = VERSION,
            .flags = @bitCast(flags),
            .reserved = .{ 0, 0 },
            .type_hash = self.config.type_hash,
            .cap_hash = self.config.cap_hash,
            .prov_hash = prov_hash,
            .payload_length = mem.nativeToBig(u32, @intCast(payload.len)),
        };

        // Write header + payload
        const header_bytes = mem.asBytes(&header);
        try stream.writeAll(header_bytes);
        try stream.writeAll(payload);

        // Update chain state
        self.prev_prov_hash = prov_hash;
        self.frames_written += 1;

        return .{
            .type_hash = self.config.type_hash,
            .cap_hash = self.config.cap_hash,
            .prov_hash = prov_hash,
            .prev_hash = self.prev_prov_hash,
            .timestamp = timestamp,
            .payload_len = payload.len,
            .frame_number = self.frames_written,
        };
    }

    /// Write raw header bytes (for manual construction / testing).
    pub fn writeRawHeader(stream: net.Stream, header: FrameHeader) !void {
        try stream.writeAll(mem.asBytes(&header));
    }
};

// =========================================================================
// Frame Reader
// =========================================================================

/// Result of reading a GRV6 frame.
pub const ReadResult = union(enum) {
    /// Successfully read a GRV6 frame.
    frame: struct {
        header: FrameHeader,
        payload: []const u8,
    },
    /// Not a GRV6 frame (magic didn't match). Raw bytes returned.
    untyped: []const u8,
    /// Connection closed (EOF).
    eof: void,
    /// Error reading.
    err: anyerror,
};

/// Validation result for a received frame.
pub const ValidationResult = enum {
    /// Type hash matches expected type.
    accepted,
    /// Type hash does not match any expected type.
    rejected_type_mismatch,
    /// Frame header is malformed.
    rejected_invalid_header,
    /// Provenance chain is broken.
    rejected_provenance_broken,
};

/// A GRV6 frame reader that validates incoming frames.
pub const FrameReader = struct {
    expected_types: []const [HASH_SIZE]u8,
    prev_prov_hash: [HASH_SIZE]u8,
    frames_read: u64,
    frames_rejected: u64,
    buf: [HEADER_SIZE]u8,
    payload_buf: []u8,
    allocator: mem.Allocator,

    const Self = @This();

    /// Create a new frame reader.
    /// expected_types: array of type hashes this reader accepts.
    pub fn init(
        allocator: mem.Allocator,
        expected_types: []const [HASH_SIZE]u8,
        max_payload: usize,
    ) !Self {
        const payload_buf = try allocator.alloc(u8, max_payload);
        return .{
            .expected_types = expected_types,
            .prev_prov_hash = ZERO_HASH,
            .frames_read = 0,
            .frames_rejected = 0,
            .buf = undefined,
            .payload_buf = payload_buf,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.payload_buf);
    }

    /// Read and validate a single GRV6 frame from a stream.
    pub fn readFrame(self: *Self, stream: net.Stream) ReadResult {
        // Read header bytes
        const n = stream.read(&self.buf) catch |e| return .{ .err = e };
        if (n == 0) return .eof;
        if (n < HEADER_SIZE) return .{ .untyped = self.buf[0..n] };

        // Check magic
        const header: *const FrameHeader = @ptrCast(@alignCast(&self.buf));
        if (!header.isValid()) {
            return .{ .untyped = self.buf[0..n] };
        }

        // Read payload
        const payload_len = header.getPayloadLength();
        if (payload_len > self.payload_buf.len) {
            self.frames_rejected += 1;
            return .{ .err = error.PayloadTooLarge };
        }

        if (payload_len > 0) {
            const pn = stream.readAtLeast(self.payload_buf[0..payload_len], payload_len) catch |e| return .{ .err = e };
            if (pn < payload_len) return .{ .err = error.UnexpectedEof };
        }

        self.frames_read += 1;

        return .{
            .frame = .{
                .header = @as(*const FrameHeader, @ptrCast(@alignCast(&self.buf))).*,
                .payload = self.payload_buf[0..payload_len],
            },
        };
    }

    /// Validate a received frame's type hash against expected types.
    pub fn validate(self: *Self, header: *const FrameHeader) ValidationResult {
        if (!header.isValid()) return .rejected_invalid_header;

        // Check type hash against expected types
        var type_matched = false;
        for (self.expected_types) |expected| {
            if (mem.eql(u8, &header.type_hash, &expected)) {
                type_matched = true;
                break;
            }
        }

        if (!type_matched) {
            self.frames_rejected += 1;
            return .rejected_type_mismatch;
        }

        return .accepted;
    }
};

// =========================================================================
// Provenance Record
// =========================================================================

/// A provenance record for a single frame in the chain.
pub const ProvenanceRecord = struct {
    type_hash: [HASH_SIZE]u8,
    cap_hash: [HASH_SIZE]u8,
    prov_hash: [HASH_SIZE]u8,
    prev_hash: [HASH_SIZE]u8,
    timestamp: i128,
    payload_len: usize,
    frame_number: u64,
};

// =========================================================================
// Utility
// =========================================================================

/// Format a hash as a hex string (for logging/display).
pub fn hashToHex(hash: [HASH_SIZE]u8) [HASH_SIZE * 2]u8 {
    const hex_chars = "0123456789abcdef";
    var out: [HASH_SIZE * 2]u8 = undefined;
    for (hash, 0..) |byte, i| {
        out[i * 2] = hex_chars[byte >> 4];
        out[i * 2 + 1] = hex_chars[byte & 0x0f];
    }
    return out;
}
