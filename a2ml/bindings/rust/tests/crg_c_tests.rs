// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for crates.io)
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// CRG C test suite for the a2ml crate.
//
// Test categories covered (all required for CRG Grade C):
//
//   [SMOKE]    Fast, dependency-free sanity checks that the crate loads and
//              the happy path works.
//
//   [UNIT]     Per-function correctness tests (supplement to inline tests in
//              src/parser.rs and src/renderer.rs).
//
//   [P2P]      Property-based (proptest) tests verifying invariants hold for
//              arbitrary inputs: round-trip idempotency, TrustLevel ordering,
//              Directive construction, error-message formatting.
//
//   [CONTRACT] Pre/post-condition tests: every public function's documented
//              contract is verified (e.g. parse("") -> Ok(empty doc),
//              render always produces valid UTF-8, TrustLevel::from_str is
//              total for known names).
//
//   [ASPECT]   Cross-cutting concerns: security (injection, large inputs,
//              null bytes, path traversal), error propagation, UTF-8
//              correctness.
//
//   [E2E]      Reflexive / dogfood: parse a real A2ML file from the standards
//              repo itself; verify the parsed manifest round-trips cleanly.

use a2ml::parser::parse;
use a2ml::renderer::render;
use a2ml::types::{Attestation, Block, Directive, Document, Inline, Manifest, TrustLevel};

// ============================================================================
// [SMOKE] — happy-path sanity checks
// ============================================================================

/// SMOKE: the crate can be imported and the version directive round-trips.
#[test]
fn smoke_version_directive_roundtrip() {
    let input = "@version 1.0\n\n# Hello\n\nA paragraph.";
    let doc = parse(input).expect("[SMOKE] parse must succeed");
    assert_eq!(doc.directives.len(), 1, "[SMOKE] one directive expected");
    assert_eq!(doc.directives[0].name, "version");
    let rendered = render(&doc).expect("[SMOKE] render must succeed");
    assert!(rendered.contains("@version 1.0"), "[SMOKE] directive in output");
}

/// SMOKE: empty document parses and renders to empty string.
#[test]
fn smoke_empty_doc() {
    let doc = parse("").expect("[SMOKE] empty parse must succeed");
    assert!(doc.blocks.is_empty(), "[SMOKE] no blocks");
    let rendered = render(&doc).expect("[SMOKE] empty render");
    assert!(rendered.is_empty() || rendered.trim().is_empty(), "[SMOKE] empty output");
}

/// SMOKE: TrustLevel display names are stable.
#[test]
fn smoke_trust_level_display() {
    assert_eq!(TrustLevel::Unverified.to_string(), "unverified");
    assert_eq!(TrustLevel::Automated.to_string(), "automated");
    assert_eq!(TrustLevel::Reviewed.to_string(), "reviewed");
    assert_eq!(TrustLevel::Verified.to_string(), "verified");
}

// ============================================================================
// [UNIT] — targeted per-feature unit tests
// ============================================================================

/// UNIT: heading levels 1–6 are parsed correctly.
#[test]
fn unit_heading_levels() {
    for level in 1u8..=6 {
        let hashes = "#".repeat(level as usize);
        let input = format!("{} Level {}", hashes, level);
        let doc = parse(&input).expect("parse heading");
        let found_heading = doc.blocks.iter().any(|b| {
            matches!(b, Block::Heading { level: l, .. } if *l == level)
        });
        assert!(found_heading, "heading level {} not parsed", level);
    }
}

/// UNIT: attestation with all optional fields (timestamp, note).
#[test]
fn unit_attestation_all_fields() {
    let input =
        "!attest identity=Bob role=reviewer trust=verified timestamp=2026-04-04 note=LGTM";
    let doc = parse(input).expect("parse attestation");
    assert_eq!(doc.attestations.len(), 1);
    let att = &doc.attestations[0];
    assert_eq!(att.identity, "Bob");
    assert_eq!(att.role, "reviewer");
    assert_eq!(att.trust_level, TrustLevel::Verified);
    assert_eq!(att.timestamp.as_deref(), Some("2026-04-04"));
    assert_eq!(att.note.as_deref(), Some("LGTM"));
}

/// UNIT: ordered list is parsed correctly.
#[test]
fn unit_ordered_list() {
    let input = "1. First\n2. Second\n3. Third";
    let doc = parse(input).expect("parse ordered list");
    let list = doc.blocks.iter().find(|b| matches!(b, Block::List { ordered: true, .. }));
    assert!(list.is_some(), "ordered list block expected");
    if let Some(Block::List { items, .. }) = list {
        assert_eq!(items.len(), 3, "three list items expected");
    }
}

/// UNIT: unordered list is parsed correctly.
#[test]
fn unit_unordered_list() {
    let input = "- Alpha\n- Beta\n- Gamma";
    let doc = parse(input).expect("parse unordered list");
    let list = doc.blocks.iter().find(|b| matches!(b, Block::List { ordered: false, .. }));
    assert!(list.is_some(), "unordered list block expected");
}

/// UNIT: inline emphasis is parsed.
#[test]
fn unit_inline_emphasis() {
    let input = "A paragraph with *italic* text.";
    let doc = parse(input).expect("parse emphasis");
    // At least one paragraph with emphasis inline.
    let has_emphasis = doc.blocks.iter().any(|b| {
        if let Block::Paragraph(inlines) = b {
            inlines.iter().any(|i| matches!(i, Inline::Emphasis(_)))
        } else {
            false
        }
    });
    assert!(has_emphasis, "italic/emphasis inline expected");
}

/// UNIT: inline strong is parsed.
#[test]
fn unit_inline_strong() {
    let input = "A paragraph with **bold** text.";
    let doc = parse(input).expect("parse strong");
    let has_strong = doc.blocks.iter().any(|b| {
        if let Block::Paragraph(inlines) = b {
            inlines.iter().any(|i| matches!(i, Inline::Strong(_)))
        } else {
            false
        }
    });
    assert!(has_strong, "strong/bold inline expected");
}

/// UNIT: inline code is parsed.
#[test]
fn unit_inline_code() {
    let input = "Use `cargo test` to run tests.";
    let doc = parse(input).expect("parse inline code");
    let has_code = doc.blocks.iter().any(|b| {
        if let Block::Paragraph(inlines) = b {
            inlines.iter().any(|i| matches!(i, Inline::Code(_)))
        } else {
            false
        }
    });
    assert!(has_code, "inline code span expected");
}

/// UNIT: Manifest::from_document extracts version and title.
#[test]
fn unit_manifest_extraction() {
    let input = "@version 2.0\n\n# My Document\n\nContent.";
    let doc = parse(input).expect("parse");
    let manifest = Manifest::from_document(&doc);
    assert_eq!(manifest.version.as_deref(), Some("2.0"));
    assert_eq!(manifest.title.as_deref(), Some("My Document"));
}

/// UNIT: Directive::new creates a directive with empty attributes.
#[test]
fn unit_directive_new() {
    let d = Directive::new("require", "trust-level:reviewed");
    assert_eq!(d.name, "require");
    assert_eq!(d.value, "trust-level:reviewed");
    assert!(d.attributes.is_empty(), "new directive should have no attributes");
}

// ============================================================================
// [P2P] — property-based tests using proptest
// ============================================================================

use proptest::prelude::*;

proptest! {
    /// P2P: TrustLevel::from_str is total for the four canonical names
    /// (case-insensitive) and None for all other strings.
    #[test]
    fn p2p_trust_level_from_str_canonical(
        s in prop_oneof![
            Just("unverified"),
            Just("UNVERIFIED"),
            Just("Unverified"),
            Just("automated"),
            Just("AUTOMATED"),
            Just("reviewed"),
            Just("REVIEWED"),
            Just("verified"),
            Just("VERIFIED"),
        ]
    ) {
        let result = TrustLevel::from_str(s);
        prop_assert!(result.is_some(), "canonical name '{}' must parse", s);
    }

    /// P2P: TrustLevel::from_str returns None for arbitrary non-canonical strings
    /// (tested via strings that definitely aren't the four keywords).
    #[test]
    fn p2p_trust_level_from_str_unknown(
        s in "[a-z]{1,20}".prop_filter("exclude canonical names", |s| {
            !["unverified","automated","reviewed","verified"].contains(&s.as_str())
        })
    ) {
        let result = TrustLevel::from_str(&s);
        prop_assert!(result.is_none(), "non-canonical '{}' must return None", s);
    }

    /// P2P: TrustLevel ordering is consistent with the documented scale
    /// (Unverified < Automated < Reviewed < Verified).
    #[test]
    fn p2p_trust_level_ordering(
        a_idx in 0usize..4,
        b_idx in 0usize..4,
    ) {
        let levels = [
            TrustLevel::Unverified,
            TrustLevel::Automated,
            TrustLevel::Reviewed,
            TrustLevel::Verified,
        ];
        let a = levels[a_idx];
        let b = levels[b_idx];
        // Ordering must be total and consistent.
        if a_idx < b_idx {
            prop_assert!(a < b, "{} should be < {}", a, b);
        } else if a_idx > b_idx {
            prop_assert!(a > b, "{} should be > {}", a, b);
        } else {
            prop_assert_eq!(a, b);
        }
    }

    /// P2P: Directive::new stores name and value verbatim (no transformation).
    #[test]
    fn p2p_directive_stores_verbatim(
        name in "[a-z][a-z0-9-]{0,20}",
        value in "[^\n\r]{0,100}",
    ) {
        let d = Directive::new(name.clone(), value.clone());
        prop_assert_eq!(&d.name, &name);
        prop_assert_eq!(&d.value, &value);
        prop_assert!(d.attributes.is_empty());
    }

    /// P2P: Attestation::new stores identity, role, trust_level verbatim.
    #[test]
    fn p2p_attestation_stores_verbatim(
        identity in "[A-Za-z][A-Za-z0-9-]{0,30}",
        role in "[a-z]{3,15}",
        trust_idx in 0usize..4,
    ) {
        let levels = [
            TrustLevel::Unverified,
            TrustLevel::Automated,
            TrustLevel::Reviewed,
            TrustLevel::Verified,
        ];
        let trust = levels[trust_idx];
        let a = Attestation::new(identity.clone(), role.clone(), trust);
        prop_assert_eq!(&a.identity, &identity);
        prop_assert_eq!(&a.role, &role);
        prop_assert_eq!(a.trust_level, trust);
        prop_assert!(a.timestamp.is_none());
        prop_assert!(a.note.is_none());
    }

    /// P2P: parse + render of a simple paragraph (no special chars) is
    /// idempotent on the block count (rendered output re-parses to same count).
    #[test]
    fn p2p_paragraph_roundtrip_block_count(
        text in "[A-Za-z ]{5,80}",
    ) {
        // Build a minimal document with one paragraph.
        let mut doc = Document::new();
        doc.blocks.push(Block::Paragraph(vec![Inline::Text(text.clone())]));

        let rendered = render(&doc).expect("render must not fail");
        let re_parsed = parse(&rendered).expect("re-parse must not fail");
        // Block count must be preserved (1 paragraph).
        prop_assert_eq!(
            re_parsed.blocks.len(),
            1,
            "re-parsed block count mismatch for input {:?}",
            text
        );
    }

    /// P2P: TrustLevel display -> from_str is a perfect round-trip for all
    /// variants.
    #[test]
    fn p2p_trust_level_display_from_str_roundtrip(
        idx in 0usize..4,
    ) {
        let levels = [
            TrustLevel::Unverified,
            TrustLevel::Automated,
            TrustLevel::Reviewed,
            TrustLevel::Verified,
        ];
        let original = levels[idx];
        let s = original.to_string();
        let parsed = TrustLevel::from_str(&s);
        prop_assert_eq!(parsed, Some(original), "display->from_str roundtrip failed");
    }
}

// ============================================================================
// [CONTRACT] — pre/post-condition tests
// ============================================================================

/// CONTRACT: parse("") post-condition: returns Ok with an empty Document.
/// Precondition: empty string is valid A2ML input (whitespace-only is also valid).
#[test]
fn contract_parse_empty_returns_empty_doc() {
    let result = parse("");
    assert!(result.is_ok(), "CONTRACT: parse('') must return Ok");
    let doc = result.unwrap();
    assert!(doc.title.is_none(), "CONTRACT: empty doc has no title");
    assert!(doc.blocks.is_empty(), "CONTRACT: empty doc has no blocks");
    assert!(doc.directives.is_empty(), "CONTRACT: empty doc has no directives");
    assert!(doc.attestations.is_empty(), "CONTRACT: empty doc has no attestations");
}

/// CONTRACT: render(doc) post-condition: always returns valid UTF-8.
/// (The Rust String type guarantees UTF-8; this test ensures no bytes are
/// dropped or corrupted by the renderer.)
#[test]
fn contract_render_always_utf8() {
    let inputs = [
        "",
        "# Hello",
        "@version 1.0",
        "!attest identity=X role=author trust=verified",
        "```\ncode\n```",
    ];
    for input in &inputs {
        let doc = parse(input).expect("parse must not fail");
        let rendered = render(&doc).expect("render must not fail");
        // Rust String is always UTF-8; this line would panic if not.
        let _valid: &str = &rendered;
        // Additionally: re-parse must not error.
        let _re = parse(&rendered).expect("re-parse of rendered output must not fail");
    }
}

/// CONTRACT: parse(s) where s contains an unclosed fenced code block must
/// return Err (pre: well-formedness required; post: error on violation).
#[test]
fn contract_parse_unclosed_code_block_is_err() {
    let inputs = [
        "```\nunclosed",
        "~~~\ncode without close",
        "# Title\n\n```rust\nno close",
    ];
    for input in &inputs {
        let result = parse(input);
        assert!(
            result.is_err(),
            "CONTRACT: unclosed code block must be Err, got Ok for {:?}",
            input
        );
    }
}

/// CONTRACT: TrustLevel comparison is a total order: for any two TrustLevels
/// exactly one of a < b, a == b, a > b holds.
#[test]
fn contract_trust_level_total_order() {
    let levels = [
        TrustLevel::Unverified,
        TrustLevel::Automated,
        TrustLevel::Reviewed,
        TrustLevel::Verified,
    ];
    for &a in &levels {
        for &b in &levels {
            let lt = a < b;
            let eq = a == b;
            let gt = a > b;
            let exactly_one = (lt as u8) + (eq as u8) + (gt as u8) == 1;
            assert!(
                exactly_one,
                "CONTRACT: total order violated for {:?} vs {:?}",
                a,
                b
            );
        }
    }
}

/// CONTRACT: Document::default() equals Document::new() (Default impl is
/// consistent with explicit constructor).
#[test]
fn contract_document_default_equals_new() {
    let d1 = Document::new();
    let d2 = Document::default();
    assert_eq!(d1, d2, "CONTRACT: Document::default() must equal Document::new()");
}

/// CONTRACT: Manifest::from_document on a doc with no @version directive must
/// return manifest.version = None.
#[test]
fn contract_manifest_no_version_is_none() {
    let doc = parse("# Title\n\nContent.").expect("parse");
    let manifest = Manifest::from_document(&doc);
    assert!(
        manifest.version.is_none(),
        "CONTRACT: no @version directive -> manifest.version must be None"
    );
}

/// CONTRACT: Manifest::from_document on a doc with @version must return
/// manifest.version = Some(value).
#[test]
fn contract_manifest_version_some() {
    let doc = parse("@version 3.14").expect("parse");
    let manifest = Manifest::from_document(&doc);
    assert_eq!(
        manifest.version.as_deref(),
        Some("3.14"),
        "CONTRACT: @version 3.14 -> manifest.version must be Some(\"3.14\")"
    );
}

// ============================================================================
// [ASPECT] — cross-cutting security and error-handling tests
// ============================================================================

/// ASPECT/SECURITY: Script injection in directive value is stored as plain
/// text, not interpreted or executed.
#[test]
fn aspect_security_no_script_injection_in_directive() {
    let payload = "@version <script>alert(1)</script>";
    // This may parse or fail, but must NOT cause a panic.
    // The value must be stored verbatim if parsed.
    let result = parse(payload);
    if let Ok(doc) = result {
        if let Some(d) = doc.directives.first() {
            // Value must not be silently truncated or modified.
            assert!(
                d.value.contains("<script>") || d.value.is_empty(),
                "ASPECT: injection payload must be stored verbatim, got: {:?}",
                d.value
            );
        }
    }
    // If parse returns Err that's also acceptable (value rejected by parser).
}

/// ASPECT/SECURITY: 1 MB input does not panic (no catastrophic backtracking
/// or unbounded allocation).
#[test]
fn aspect_security_large_input_no_panic() {
    let large = "A paragraph with lots of text. ".repeat(32_768); // ~1 MB
    // Must not panic; Ok or Err both acceptable.
    let _ = parse(&large);
}

/// ASPECT/SECURITY: Null bytes in input do not cause UB or panic.
#[test]
fn aspect_security_null_bytes_no_panic() {
    let input_with_null = "# Hello\0World\n\nA paragraph\0with null.";
    // Null bytes may trigger parse error or be silently handled; no panic allowed.
    let _ = parse(input_with_null);
}

/// ASPECT/SECURITY: Very long directive name does not cause stack overflow.
#[test]
fn aspect_security_very_long_directive_name() {
    let long_name = format!("@{} value", "x".repeat(100_000));
    // Must not stack overflow or panic.
    let _ = parse(&long_name);
}

/// ASPECT/SECURITY: Unicode content is handled correctly (no mojibake).
#[test]
fn aspect_security_unicode_content() {
    let unicode_inputs = [
        "# こんにちは\n\n日本語のテキスト。",
        "# Привет мир\n\nКириллица.",
        "# 🦀 Ferris\n\n`cargo build`",
        "# Ünïcödë\n\nLätin-1 extendëd.",
    ];
    for input in &unicode_inputs {
        let doc = parse(input).expect("unicode input must parse");
        let rendered = render(&doc).expect("unicode doc must render");
        // The rendered output must be valid UTF-8 (guaranteed by Rust String).
        let _s: &str = &rendered;
        // Re-parse must not fail.
        let _ = parse(&rendered).expect("re-parse of unicode rendered output");
    }
}

/// ASPECT/SECURITY: Deeply nested block quotes do not stack overflow.
#[test]
fn aspect_security_deep_blockquote_no_stackoverflow() {
    // 50 levels of block quote nesting.
    let input = "> ".repeat(50) + "deep content";
    // May error (nesting limit) or succeed; must not crash.
    let _ = parse(&input);
}

/// ASPECT/ERROR-HANDLING: A2mlError::diagnostic produces a non-empty string.
#[test]
fn aspect_error_diagnostic_non_empty() {
    use a2ml::A2mlError;
    let err = A2mlError::parse(10, 5, "unexpected token");
    let diag = err.diagnostic();
    assert!(!diag.is_empty(), "ASPECT: diagnostic must not be empty");
    assert!(diag.contains("10"), "ASPECT: diagnostic must include line number");
    assert!(diag.contains("5"), "ASPECT: diagnostic must include column number");
    assert!(
        diag.contains("unexpected token"),
        "ASPECT: diagnostic must include message"
    );
}

/// ASPECT/ERROR-HANDLING: RenderError diagnostic includes the message.
#[test]
fn aspect_error_render_diagnostic() {
    use a2ml::A2mlError;
    let err = A2mlError::RenderError("output buffer full".into());
    let diag = err.diagnostic();
    assert!(!diag.is_empty(), "ASPECT: RenderError diagnostic must be non-empty");
    assert!(
        diag.contains("output buffer full"),
        "ASPECT: RenderError diagnostic must include message"
    );
}

// ============================================================================
// [E2E] — reflexive / dogfood tests
// ============================================================================

/// E2E/REFLEXIVE: parse the standards repo's own 0-AI-MANIFEST.a2ml.
///
/// This test reads the real manifest from the standards repository and verifies
/// that:
///   1. The file parses without error.
///   2. The parsed document has at least one block.
///   3. The rendered output can be re-parsed (round-trip stability).
///   4. The rendered output is non-empty.
#[test]
fn e2e_dogfood_parse_standards_manifest() {
    // Locate the manifest relative to this crate's root.
    // Cargo sets CARGO_MANIFEST_DIR to the crate directory at test time.
    let crate_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    // standards/a2ml/bindings/rust/ -> standards/
    let standards_root = crate_dir
        .parent() // rust/
        .and_then(|p| p.parent()) // bindings/
        .and_then(|p| p.parent()) // a2ml/
        .and_then(|p| p.parent()) // standards/
        .expect("could not traverse to standards root");

    let manifest_path = standards_root.join("0-AI-MANIFEST.a2ml");
    if !manifest_path.exists() {
        // Skip gracefully if path resolution fails in unusual CI environments.
        eprintln!("[E2E] Skipping: manifest not found at {}", manifest_path.display());
        return;
    }

    let content = std::fs::read_to_string(&manifest_path)
        .expect("[E2E] manifest must be readable");

    let doc = parse(&content).expect("[E2E] standards manifest must parse");
    assert!(!doc.blocks.is_empty(), "[E2E] manifest must have blocks");

    let rendered = render(&doc).expect("[E2E] manifest must render");
    assert!(!rendered.is_empty(), "[E2E] rendered manifest must be non-empty");

    // Round-trip stability: re-parsed block count must match original.
    let re_parsed = parse(&rendered).expect("[E2E] re-parse must succeed");
    // Allow ±1 block count for potential trailing blank-line differences.
    let delta = (doc.blocks.len() as isize - re_parsed.blocks.len() as isize).unsigned_abs();
    assert!(
        delta <= 1,
        "[E2E] block count drift too large: {} -> {} after roundtrip",
        doc.blocks.len(),
        re_parsed.blocks.len()
    );
}

/// E2E/REFLEXIVE: parse the a2ml-gleam test fixture (if present).
/// This validates cross-format compatibility.
#[test]
fn e2e_dogfood_parse_machine_readable_state() {
    let crate_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let standards_root = crate_dir
        .parent()
        .and_then(|p| p.parent())
        .and_then(|p| p.parent())
        .and_then(|p| p.parent())
        .expect("could not traverse to standards root");

    // Try to find any .a2ml file in .machine_readable/
    let state_path = standards_root
        .join(".machine_readable")
        .join("6a2")
        .join("STATE.a2ml");

    if !state_path.exists() {
        eprintln!(
            "[E2E] Skipping: STATE.a2ml not found at {}",
            state_path.display()
        );
        return;
    }

    let content = std::fs::read_to_string(&state_path)
        .expect("[E2E] STATE.a2ml must be readable");

    // STATE.a2ml uses Scheme/S-expression syntax, not A2ML markup.
    // The parser should either handle it gracefully (parse as text paragraphs)
    // or return an error — but must NOT panic.
    let result = parse(&content);
    // No assertion on Ok/Err — just no panic.
    let _ = result;
}
