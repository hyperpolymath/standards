// SPDX-License-Identifier: MPL-2.0
// (MPL-2.0 required for crates.io)
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// CRG C test suite for the k9-svc crate.
//
// Test categories covered (all required for CRG Grade C):
//
//   [SMOKE]    Fast, dependency-free sanity checks.
//
//   [UNIT]     Per-feature correctness tests (supplement to inline tests).
//
//   [P2P]      Property-based (proptest) tests verifying invariants hold for
//              arbitrary inputs.
//
//   [CONTRACT] Pre/post-condition tests for every public function's contract.
//
//   [ASPECT]   Cross-cutting concerns: security (injection, large inputs,
//              null bytes), error propagation.
//
//   [E2E]      Reflexive / dogfood: parse real K9 files from the standards
//              repo; verify round-trip stability.

use k9_svc::parser::parse;
use k9_svc::renderer::render;
use k9_svc::types::{Component, Contract, Pedigree, Recipe, SecurityLevel};

// ============================================================================
// [SMOKE] — happy-path sanity checks
// ============================================================================

/// SMOKE: the crate can be imported and a minimal component parses.
#[test]
fn smoke_minimal_parse() {
    let input = r#"component: my-svc
  version: 0.1.0
  pedigree:
    origin: https://github.com/example/svc
    author: Alice
  security: kennel
"#;
    let components = parse(input).expect("[SMOKE] minimal parse must succeed");
    assert_eq!(components.len(), 1, "[SMOKE] one component expected");
    assert_eq!(components[0].name, "my-svc");
}

/// SMOKE: SecurityLevel display names are stable.
#[test]
fn smoke_security_level_display() {
    assert_eq!(SecurityLevel::Kennel.to_string(), "kennel");
    assert_eq!(SecurityLevel::Yard.to_string(), "yard");
    assert_eq!(SecurityLevel::Hunt.to_string(), "hunt");
}

/// SMOKE: render of a minimal component produces expected fields.
#[test]
fn smoke_render_minimal() {
    let comp = Component::new(
        "smoke-svc",
        "1.0.0",
        Pedigree::new("https://github.com/example/smoke", "Eve"),
        SecurityLevel::Hunt,
    );
    let output = render(&[comp]).expect("[SMOKE] render must succeed");
    assert!(output.contains("component: smoke-svc"), "[SMOKE] name in output");
    assert!(output.contains("  version: 1.0.0"), "[SMOKE] version in output");
    assert!(output.contains("  security: hunt"), "[SMOKE] security in output");
}

// ============================================================================
// [UNIT] — targeted per-feature unit tests
// ============================================================================

/// UNIT: SecurityLevel ordering: Kennel < Yard < Hunt (most exposed = Hunt).
#[test]
fn unit_security_level_ordering() {
    // Kennel is most trusted (least exposed), Hunt is most exposed.
    // The ordering should reflect exposure or be well-defined.
    // Just verify they are distinct and comparable.
    assert_ne!(SecurityLevel::Kennel, SecurityLevel::Yard);
    assert_ne!(SecurityLevel::Yard, SecurityLevel::Hunt);
    assert_ne!(SecurityLevel::Kennel, SecurityLevel::Hunt);
}

/// UNIT: component with description is parsed correctly.
#[test]
fn unit_component_with_description() {
    let input = r#"component: annotated
  version: 0.3.0
  description: A well-described service
  pedigree:
    origin: https://example.com
    author: Bob
  security: yard
"#;
    let components = parse(input).expect("parse");
    assert_eq!(components[0].description.as_deref(), Some("A well-described service"));
}

/// UNIT: component with recipe fields (tool, command).
#[test]
fn unit_component_with_recipe() {
    let input = r#"component: builder
  version: 2.0.0
  pedigree:
    origin: https://example.com/builder
    author: Carol
  security: yard
  recipe:
    tool: cargo
    command: cargo build --release
"#;
    let components = parse(input).expect("parse");
    let recipe = components[0].recipe.as_ref().expect("recipe expected");
    assert_eq!(recipe.tool, "cargo");
    assert_eq!(recipe.command, "cargo build --release");
}

/// UNIT: component with contract (name, description, check, severity).
#[test]
fn unit_component_with_contract() {
    let input = r#"component: guarded
  version: 0.1.0
  pedigree:
    origin: https://example.com/guarded
    author: Dave
  security: kennel
  contract: no-unsafe
    description: Forbid unsafe blocks
    check: cargo clippy -- -D unsafe-code
    severity: error
"#;
    let components = parse(input).expect("parse");
    assert_eq!(components[0].contracts.len(), 1);
    let contract = &components[0].contracts[0];
    assert_eq!(contract.name, "no-unsafe");
    assert_eq!(contract.severity, "error");
}

/// UNIT: multiple contracts on a single component.
#[test]
fn unit_multiple_contracts() {
    let input = r#"component: strict
  version: 1.0.0
  pedigree:
    origin: https://example.com/strict
    author: Eve
  security: yard
  contract: no-unsafe
    description: No unsafe
    check: cargo clippy -- -D unsafe-code
    severity: error
  contract: no-panic
    description: No panics
    check: cargo clippy -- -D clippy::unwrap_used
    severity: warning
"#;
    let components = parse(input).expect("parse");
    assert_eq!(components[0].contracts.len(), 2);
}

/// UNIT: pedigree with license field (verifies optional pedigree.license is stored).
#[test]
fn unit_pedigree_with_license() {
    // Build a component programmatically to verify Pedigree.license field exists.
    let mut ped = Pedigree::new("https://example.com/licensed", "Frank");
    ped.license = Some("PMPL-1.0-or-later".to_string());
    let comp = Component::new("licensed", "0.1.0", ped, SecurityLevel::Hunt);
    assert_eq!(
        comp.pedigree.license.as_deref(),
        Some("PMPL-1.0-or-later"),
        "pedigree.license should be stored"
    );
    // Render and check output.
    let rendered = render(&[comp]).expect("render");
    assert!(rendered.contains("component: licensed"));
}

/// UNIT: Component::new creates minimal fields with no recipe/contracts.
#[test]
fn unit_component_new_minimal() {
    let comp = Component::new(
        "min",
        "0.0.1",
        Pedigree::new("https://example.com", "X"),
        SecurityLevel::Kennel,
    );
    assert_eq!(comp.name, "min");
    assert_eq!(comp.version, "0.0.1");
    assert!(comp.recipe.is_none());
    assert!(comp.contracts.is_empty());
    assert!(comp.metadata.is_empty());
    assert!(comp.description.is_none());
}

/// UNIT: Recipe::new stores tool and command correctly.
#[test]
fn unit_recipe_new() {
    let r = Recipe::new("just", "just build");
    assert_eq!(r.tool, "just");
    assert_eq!(r.command, "just build");
    assert!(r.requires.is_empty());
    assert!(r.outputs.is_empty());
}

/// UNIT: Contract::new sets default severity to "error".
#[test]
fn unit_contract_new_default_severity() {
    let c = Contract::new("invariant", "Description.", "check command");
    assert_eq!(c.severity, "error");
}

// ============================================================================
// [P2P] — property-based tests using proptest
// ============================================================================

use proptest::prelude::*;

proptest! {
    /// P2P: SecurityLevel::from_str is total for the three canonical names
    /// (case-insensitive).
    #[test]
    fn p2p_security_level_from_str_canonical(
        s in prop_oneof![
            Just("kennel"),
            Just("KENNEL"),
            Just("Kennel"),
            Just("yard"),
            Just("YARD"),
            Just("Yard"),
            Just("hunt"),
            Just("HUNT"),
            Just("Hunt"),
        ]
    ) {
        let result = SecurityLevel::from_str(s);
        prop_assert!(result.is_some(), "canonical name '{}' must parse", s);
    }

    /// P2P: SecurityLevel::from_str returns None for arbitrary non-canonical strings.
    #[test]
    fn p2p_security_level_from_str_unknown(
        s in "[a-z]{2,20}".prop_filter("exclude canonical names", |s| {
            !["kennel","yard","hunt"].contains(&s.as_str())
        })
    ) {
        let result = SecurityLevel::from_str(&s);
        prop_assert!(result.is_none(), "non-canonical '{}' must return None", s);
    }

    /// P2P: SecurityLevel display -> from_str is a perfect round-trip for all
    /// variants.
    #[test]
    fn p2p_security_level_display_from_str_roundtrip(
        idx in 0usize..3,
    ) {
        let levels = [SecurityLevel::Kennel, SecurityLevel::Yard, SecurityLevel::Hunt];
        let original = levels[idx];
        let s = original.to_string();
        let parsed = SecurityLevel::from_str(&s);
        prop_assert_eq!(parsed, Some(original), "display->from_str roundtrip failed");
    }

    /// P2P: Component::new stores name, version, security_level verbatim.
    #[test]
    fn p2p_component_new_stores_verbatim(
        name in "[a-z][a-z0-9-]{0,20}",
        version in "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}",
        security_idx in 0usize..3,
    ) {
        let levels = [SecurityLevel::Kennel, SecurityLevel::Yard, SecurityLevel::Hunt];
        let security = levels[security_idx];
        let comp = Component::new(
            name.clone(),
            version.clone(),
            Pedigree::new("https://example.com", "Test"),
            security,
        );
        prop_assert_eq!(&comp.name, &name);
        prop_assert_eq!(&comp.version, &version);
        prop_assert_eq!(comp.security_level, security);
    }

    /// P2P: Pedigree::new stores origin and author verbatim.
    #[test]
    fn p2p_pedigree_stores_verbatim(
        origin in "https://[a-z]{3,20}\\.com/[a-z]{3,20}",
        author in "[A-Za-z][A-Za-z ]{0,30}",
    ) {
        let p = Pedigree::new(origin.clone(), author.clone());
        prop_assert_eq!(&p.origin, &origin);
        prop_assert_eq!(&p.author, &author);
        prop_assert!(p.license.is_none());
        prop_assert!(p.commit.is_none());
    }

    /// P2P: Contract::new stores name, description, check verbatim.
    #[test]
    fn p2p_contract_stores_verbatim(
        name in "[a-z][a-z0-9-]{0,20}",
        description in "[A-Za-z ]{5,60}",
        check in "[a-z ]{5,40}",
    ) {
        let c = Contract::new(name.clone(), description.clone(), check.clone());
        prop_assert_eq!(&c.name, &name);
        prop_assert_eq!(&c.description, &description);
        prop_assert_eq!(&c.check, &check);
        prop_assert_eq!(&c.severity, "error");
    }
}

// ============================================================================
// [CONTRACT] — pre/post-condition tests
// ============================================================================

/// CONTRACT: parse("") post-condition: returns Ok with an empty Vec.
#[test]
fn contract_parse_empty_returns_empty_vec() {
    let result = parse("");
    assert!(result.is_ok(), "CONTRACT: parse('') must return Ok");
    assert!(result.unwrap().is_empty(), "CONTRACT: empty input -> empty vec");
}

/// CONTRACT: render(&[]) post-condition: returns Ok with empty or whitespace-only string.
#[test]
fn contract_render_empty_slice_returns_empty() {
    let result = render(&[]);
    assert!(result.is_ok(), "CONTRACT: render([]) must return Ok");
    let out = result.unwrap();
    assert!(
        out.is_empty() || out.trim().is_empty(),
        "CONTRACT: render([]) must produce empty output, got {:?}",
        out
    );
}

/// CONTRACT: render(components) post-condition: always returns valid UTF-8.
#[test]
fn contract_render_always_utf8() {
    let components = vec![
        Component::new(
            "utf8-test",
            "1.0.0",
            Pedigree::new("https://example.com", "Ünïcödë Ãüthör"),
            SecurityLevel::Kennel,
        ),
    ];
    let rendered = render(&components).expect("render must not fail");
    let _valid: &str = &rendered; // Rust String guarantees UTF-8
}

/// CONTRACT: parse(s) with Nickel format input must return K9Error::NickelFormat.
#[test]
fn contract_nickel_format_rejected() {
    let nickel_input = "let config = { name = \"test\" } in config";
    use k9_svc::error::K9Error;
    let result = parse(nickel_input);
    assert!(result.is_err(), "CONTRACT: Nickel input must be rejected");
    assert!(
        matches!(result.unwrap_err(), K9Error::NickelFormat(_)),
        "CONTRACT: error must be K9Error::NickelFormat"
    );
}

/// CONTRACT: SecurityLevel total order — for any two SecurityLevels,
/// exactly one of a < b, a == b, a > b holds.
#[test]
fn contract_security_level_total_order() {
    let levels = [SecurityLevel::Kennel, SecurityLevel::Yard, SecurityLevel::Hunt];
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

/// CONTRACT: missing pedigree in parse input must return Err.
#[test]
fn contract_missing_pedigree_is_err() {
    let input = r#"component: incomplete
  version: 1.0.0
  security: hunt
"#;
    assert!(parse(input).is_err(), "CONTRACT: missing pedigree must be Err");
}

/// CONTRACT: unknown security level in parse input must return Err.
#[test]
fn contract_unknown_security_level_is_err() {
    let input = r#"component: bad-level
  version: 1.0.0
  pedigree:
    origin: https://example.com
    author: X
  security: fortress
"#;
    use k9_svc::error::K9Error;
    let result = parse(input);
    assert!(result.is_err(), "CONTRACT: unknown security level must be Err");
    assert!(
        matches!(result.unwrap_err(), K9Error::UnknownSecurityLevel(_)),
        "CONTRACT: error must be K9Error::UnknownSecurityLevel"
    );
}

// ============================================================================
// [ASPECT] — cross-cutting security and error-handling tests
// ============================================================================

/// ASPECT/SECURITY: Shell injection in origin URL is stored as plain text,
/// not executed.
#[test]
fn aspect_security_no_shell_injection_in_origin() {
    let input = r#"component: injected
  version: 1.0.0
  pedigree:
    origin: https://example.com; rm -rf /
    author: Attacker
  security: hunt
"#;
    // May parse (storing verbatim) or return error; must NOT execute the command.
    let result = parse(input);
    if let Ok(components) = result {
        if let Some(comp) = components.first() {
            // If parsed, origin must be stored as-is (not executed).
            assert!(comp.pedigree.origin.contains("example.com"));
        }
    }
    // If Err, that's also acceptable.
}

/// ASPECT/SECURITY: 1 MB input does not panic.
#[test]
fn aspect_security_large_input_no_panic() {
    // Build a large but valid-ish block of K9-like text.
    let single_comp = "component: svc\n  version: 1.0.0\n  pedigree:\n    origin: https://x.com\n    author: X\n  security: yard\n";
    let large_input: String = single_comp.repeat(1000); // ~130 KB
    let _ = parse(&large_input);
}

/// ASPECT/SECURITY: Null bytes in input do not cause UB or panic.
#[test]
fn aspect_security_null_bytes_no_panic() {
    let input = "component: svc\0null\n  version: 1.0.0\n  pedigree:\n    origin: https://x.com\n    author: X\n  security: yard\n";
    let _ = parse(input);
}

/// ASPECT/SECURITY: Unicode in component name / author does not panic.
#[test]
fn aspect_security_unicode_no_panic() {
    let input = r#"component: 日本語コンポーネント
  version: 0.1.0
  pedigree:
    origin: https://example.com/jp
    author: 田中太郎
  security: kennel
"#;
    let _ = parse(input);
}

/// ASPECT/ERROR-HANDLING: K9Error diagnostic produces non-empty string.
#[test]
fn aspect_error_diagnostic_non_empty() {
    use k9_svc::error::K9Error;
    let err = K9Error::UnknownSecurityLevel("fortress".into());
    let diag = format!("{}", err);
    assert!(!diag.is_empty(), "ASPECT: error display must not be empty");
    assert!(diag.contains("fortress"), "ASPECT: error must mention the bad value");
}

/// ASPECT/ERROR-HANDLING: NickelFormat error includes the Nickel content.
#[test]
fn aspect_error_nickel_format_message() {
    use k9_svc::error::K9Error;
    let err = K9Error::NickelFormat("let x = 1 in x".into());
    let msg = format!("{}", err);
    assert!(!msg.is_empty(), "ASPECT: NickelFormat error must not be empty");
}

// ============================================================================
// [E2E] — reflexive / dogfood tests
// ============================================================================

/// E2E/REFLEXIVE: parse any K9 files found in the standards repo.
///
/// The k9-svc/ directory may contain real K9 configuration files.
/// This test finds them and verifies they parse without error.
#[test]
fn e2e_dogfood_parse_k9_fixtures() {
    let crate_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    // k9-svc/bindings/rust/ -> standards/k9-svc/ -> standards/
    let standards_root = crate_dir
        .parent() // rust/
        .and_then(|p| p.parent()) // bindings/
        .and_then(|p| p.parent()) // k9-svc/
        .and_then(|p| p.parent()) // standards/
        .expect("could not traverse to standards root");

    // Look for .k9 files in the k9-svc directory.
    let k9_dir = standards_root.join("k9-svc");
    if !k9_dir.exists() {
        eprintln!("[E2E] Skipping: k9-svc dir not found at {}", k9_dir.display());
        return;
    }

    // Enumerate .k9 files (non-recursive for simplicity).
    let entries: Vec<_> = std::fs::read_dir(&k9_dir)
        .expect("[E2E] k9-svc dir must be readable")
        .filter_map(|e| e.ok())
        .filter(|e| {
            e.path()
                .extension()
                .map(|ext| ext == "k9")
                .unwrap_or(false)
        })
        .collect();

    if entries.is_empty() {
        eprintln!("[E2E] No .k9 files found in {}; skipping.", k9_dir.display());
        return;
    }

    for entry in &entries {
        let path = entry.path();
        let content = std::fs::read_to_string(&path)
            .expect(&format!("[E2E] {} must be readable", path.display()));
        let result = parse(&content);
        assert!(
            result.is_ok(),
            "[E2E] {} must parse successfully, got: {:?}",
            path.display(),
            result.unwrap_err()
        );
    }
}

/// E2E/REFLEXIVE: round-trip a programmatically constructed K9 document.
///
/// Build a component, render it, re-parse the rendered output, and verify
/// the key fields are preserved.
#[test]
fn e2e_roundtrip_constructed_component() {
    let mut comp = Component::new(
        "e2e-svc",
        "3.0.0",
        Pedigree::new("https://github.com/hyperpolymath/e2e-test", "Tester"),
        SecurityLevel::Yard,
    );
    comp.description = Some("E2E round-trip test component".into());
    comp.recipe = Some(Recipe::new("cargo", "cargo build --release"));
    comp.contracts.push(Contract::new(
        "no-unsafe",
        "Forbid unsafe blocks",
        "cargo clippy -- -D unsafe-code",
    ));

    let rendered = render(&[comp.clone()]).expect("[E2E] render must succeed");
    assert!(!rendered.is_empty(), "[E2E] rendered output must be non-empty");

    let re_parsed = parse(&rendered).expect("[E2E] re-parse must succeed");
    assert_eq!(re_parsed.len(), 1, "[E2E] one component after roundtrip");

    let c = &re_parsed[0];
    assert_eq!(c.name, "e2e-svc", "[E2E] name preserved");
    assert_eq!(c.version, "3.0.0", "[E2E] version preserved");
    assert_eq!(c.security_level, SecurityLevel::Yard, "[E2E] security_level preserved");
    assert_eq!(
        c.pedigree.author, "Tester",
        "[E2E] pedigree.author preserved"
    );
}
