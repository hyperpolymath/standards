// LOL i18n Service — Translation Store
//
// Thread-safe translation store backed by a HashMap. Loads translations
// from the corpus data directory and provides lookup with fallback chain
// resolution. Matches the I18nStore interface from src/abi/I18nStore.idr.
//
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

const std = @import("std");
const locale_mod = @import("locale.zig");

/// A single translation entry in the store.
pub const Entry = struct {
    /// The translation text (UTF-8, owned by the store)
    text: []const u8,
    /// The locale this translation belongs to
    locale: []const u8,
};

/// Composite key for the translation map: (locale, key) pair.
/// Uses a deterministic hash for thread-safe concurrent reads.
const MapKey = struct {
    locale: []const u8,
    key: []const u8,

    pub fn hash(self: MapKey) u64 {
        var h = std.hash.Wyhash.init(0);
        h.update(self.locale);
        h.update(&[_]u8{0}); // separator
        h.update(self.key);
        return h.final();
    }

    pub fn eql(a: MapKey, b: MapKey) bool {
        return std.mem.eql(u8, a.locale, b.locale) and
            std.mem.eql(u8, a.key, b.key);
    }
};

/// Thread-safe translation store.
/// Supports concurrent reads; writes are serialised via a mutex.
/// Translations are loaded from filesystem at init time.
pub const Store = struct {
    allocator: std.mem.Allocator,
    data_dir: []const u8,
    default_locale: []const u8,
    enable_fallback: bool,
    /// All owned strings (translations, keys, locales) for bulk dealloc
    owned_strings: std.ArrayListUnmanaged([]const u8),
    /// Translation entries keyed by "locale\x00key"
    entries: std.StringHashMapUnmanaged([]const u8),
    mutex: std.Thread.Mutex,

    /// Initialise a new empty store.
    pub fn init(allocator: std.mem.Allocator, data_dir: []const u8, default_locale: []const u8, enable_fallback: bool) Store {
        return .{
            .allocator = allocator,
            .data_dir = data_dir,
            .default_locale = default_locale,
            .enable_fallback = enable_fallback,
            .owned_strings = .{},
            .entries = .{},
            .mutex = .{},
        };
    }

    /// Release all resources held by the store.
    pub fn deinit(self: *Store) void {
        // Free all owned strings
        for (self.owned_strings.items) |s| {
            self.allocator.free(s);
        }
        self.owned_strings.deinit(self.allocator);
        self.entries.deinit(self.allocator);
    }

    /// Look up a translation by locale and key.
    /// Returns null if not found (does NOT perform fallback).
    pub fn get(self: *Store, locale_str: []const u8, key: []const u8) ?[]const u8 {
        const composite = self.makeCompositeKey(locale_str, key) catch return null;
        defer self.allocator.free(composite);
        return self.entries.get(composite);
    }

    /// Look up a translation with fallback chain resolution.
    /// Tries the full locale first, then each prefix, then default locale.
    pub fn getWithFallback(
        self: *Store,
        locale_tag: []const u8,
        key: []const u8,
    ) ?FallbackResult {
        // Try exact match first
        if (self.get(locale_tag, key)) |text| {
            return .{
                .text = text,
                .resolved_locale = locale_tag,
                .is_fallback = false,
            };
        }

        if (!self.enable_fallback) return null;

        // Walk the fallback chain (strip subtags right to left)
        var tag = locale_tag;
        while (std.mem.lastIndexOfScalar(u8, tag, '-')) |dash| {
            tag = tag[0..dash];
            if (self.get(tag, key)) |text| {
                return .{
                    .text = text,
                    .resolved_locale = tag,
                    .is_fallback = true,
                };
            }
        }

        // Try default locale
        if (!std.mem.eql(u8, tag, self.default_locale)) {
            if (self.get(self.default_locale, key)) |text| {
                return .{
                    .text = text,
                    .resolved_locale = self.default_locale,
                    .is_fallback = true,
                };
            }
        }

        return null;
    }

    /// Insert a translation into the store.
    /// Thread-safe (acquires mutex).
    pub fn put(self: *Store, locale_str: []const u8, key: []const u8, text: []const u8) !void {
        self.mutex.lock();
        defer self.mutex.unlock();

        const owned_text = try self.allocator.dupe(u8, text);
        errdefer self.allocator.free(owned_text);
        try self.owned_strings.append(self.allocator,owned_text);

        const composite = try self.makeCompositeKeyOwned(locale_str, key);
        try self.entries.put(self.allocator,composite, owned_text);
    }

    /// Load translations from the filesystem.
    /// Reads <data_dir>/translations/<locale>/<key>.txt for each file found.
    pub fn loadFromDisk(self: *Store) !void {
        const translations_path = try std.fmt.allocPrint(self.allocator, "{s}/translations", .{self.data_dir});
        defer self.allocator.free(translations_path);

        var locale_dir = std.fs.cwd().openDir(translations_path, .{ .iterate = true }) catch return;
        defer locale_dir.close();

        var locale_iter = locale_dir.iterate();
        while (try locale_iter.next()) |locale_entry| {
            if (locale_entry.kind != .directory) continue;

            var key_dir = locale_dir.openDir(locale_entry.name, .{ .iterate = true }) catch continue;
            defer key_dir.close();

            var key_iter = key_dir.iterate();
            while (try key_iter.next()) |key_entry| {
                if (!std.mem.endsWith(u8, key_entry.name, ".txt")) continue;

                const key_name = key_entry.name[0 .. key_entry.name.len - 4]; // strip .txt

                const file = key_dir.openFile(key_entry.name, .{}) catch continue;
                defer file.close();

                const content = file.readToEndAlloc(self.allocator, 1024 * 1024) catch continue;
                errdefer self.allocator.free(content);

                try self.owned_strings.append(self.allocator,content);
                const composite = try self.makeCompositeKeyOwned(locale_entry.name, key_name);
                try self.entries.put(self.allocator,composite, content);
            }
        }
    }

    // --- Internal helpers ---

    /// Build a composite key "locale\x00key" for HashMap lookup (temporary).
    fn makeCompositeKey(self: *Store, locale_str: []const u8, key: []const u8) ![]const u8 {
        return std.fmt.allocPrint(self.allocator, "{s}\x00{s}", .{ locale_str, key });
    }

    /// Build a composite key "locale\x00key" and track ownership.
    fn makeCompositeKeyOwned(self: *Store, locale_str: []const u8, key: []const u8) ![]const u8 {
        const composite = try std.fmt.allocPrint(self.allocator, "{s}\x00{s}", .{ locale_str, key });
        errdefer self.allocator.free(composite);
        try self.owned_strings.append(self.allocator,composite);
        return composite;
    }
};

/// Result of a fallback-aware lookup.
pub const FallbackResult = struct {
    text: []const u8,
    resolved_locale: []const u8,
    is_fallback: bool,
};

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "store put and get" {
    var store = Store.init(std.testing.allocator, "test", "en", true);
    defer store.deinit();

    try store.put("en", "greeting", "Hello");
    const result = store.get("en", "greeting");
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("Hello", result.?);
}

test "store get missing key returns null" {
    var store = Store.init(std.testing.allocator, "test", "en", true);
    defer store.deinit();

    const result = store.get("en", "nonexistent");
    try std.testing.expect(result == null);
}

test "store fallback resolution" {
    var store = Store.init(std.testing.allocator, "test", "en", true);
    defer store.deinit();

    try store.put("en", "greeting", "Hello");

    // Look up "en-US" which should fall back to "en"
    const result = store.getWithFallback("en-US", "greeting");
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("Hello", result.?.text);
    try std.testing.expect(result.?.is_fallback);
}

test "store no fallback when disabled" {
    var store = Store.init(std.testing.allocator, "test", "en", false);
    defer store.deinit();

    try store.put("en", "greeting", "Hello");

    const result = store.getWithFallback("en-US", "greeting");
    try std.testing.expect(result == null);
}

test "store default locale fallback" {
    var store = Store.init(std.testing.allocator, "test", "en", true);
    defer store.deinit();

    try store.put("en", "greeting", "Hello");

    // Look up "fr" which should fall back to default "en"
    const result = store.getWithFallback("fr", "greeting");
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("Hello", result.?.text);
    try std.testing.expect(result.?.is_fallback);
}
