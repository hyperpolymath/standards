// SPDX-License-Identifier: MPL-2.0
//
// a2ml-lsp — Hover provider for A2ML documents.
//
// Returns Markdown documentation when the cursor hovers over:
//   1. Directive names (`@abstract`, `@opaque`, …)
//   2. Section headers (`[metadata]`, `[dependencies]`, …)
//
// Author: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>

use std::sync::LazyLock;
use regex::Regex;
use tower_lsp::lsp_types::*;

// ── Directive documentation ──────────────────────────────────────────

/// Documentation entry for a single A2ML directive.
struct DirectiveDoc {
    /// Directive name without the leading `@`.
    name: &'static str,
    /// Markdown documentation shown on hover.
    markdown: &'static str,
}

/// Full directive catalogue with documentation drawn from the A2ML spec.
const DIRECTIVE_DOCS: &[DirectiveDoc] = &[
    DirectiveDoc {
        name: "abstract",
        markdown: "## @abstract\n\n\
            Declares the **abstract / summary section** of the document.\n\n\
            ```a2ml\n@abstract:\nYour summary text here.\n@end\n```\n\n\
            The abstract is typically rendered as a preamble by output formatters.\n\n\
            *A2ML v0 core directive*",
    },
    DirectiveDoc {
        name: "opaque",
        markdown: "## @opaque\n\n\
            **Byte-for-byte preserved payload block.**\n\n\
            Content inside `@opaque` MUST be preserved verbatim through every \
            processing stage. Renderers MAY transform only when the target format \
            requires it, and must report transformations.\n\n\
            ```a2ml\n@opaque(lang=\"ebnf\", id=\"grammar\"):\n... raw content ...\n@end\n```\n\n\
            **Attributes:**\n\
            - `lang` — language hint for syntax highlighting\n\
            - `id` — referenceable identifier for `@ref()`\n\n\
            *A2ML v0 core directive — non-negotiable preservation guarantee*",
    },
    DirectiveDoc {
        name: "fig",
        markdown: "## @fig\n\n\
            **Figure block** with caption and optional ID.\n\n\
            ```a2ml\n@fig(id=\"fig-arch\"):\nArchitecture diagram description.\n@end\n```\n\n\
            **Attributes:**\n\
            - `id` — referenceable identifier\n\
            - `caption` — figure caption text\n\n\
            *A2ML v0 core directive*",
    },
    DirectiveDoc {
        name: "table",
        markdown: "## @table\n\n\
            **Table block** with caption and optional ID.\n\n\
            ```a2ml\n@table(id=\"tbl-results\"):\n| Col A | Col B |\n| 1     | 2     |\n@end\n```\n\n\
            **Attributes:**\n\
            - `id` — referenceable identifier\n\
            - `caption` — table caption text\n\n\
            *A2ML v0 core directive*",
    },
    DirectiveDoc {
        name: "refs",
        markdown: "## @refs\n\n\
            **References / bibliography section.**\n\n\
            ```a2ml\n@refs:\n[1] First reference\n[2] Second reference\n@end\n```\n\n\
            Each non-blank line is treated as a single reference entry.\n\n\
            *A2ML v0 core directive*",
    },
    DirectiveDoc {
        name: "requires",
        markdown: "## @requires\n\n\
            **Structural dependencies** for attested mode.\n\n\
            Declares what sections or IDs this block depends on. Used by the \
            Idris2 typed core to build the dependency graph and enforce proof \
            obligations.\n\n\
            ```a2ml\n@requires:\nsection-a\nsection-b\n@end\n```\n\n\
            *A2ML v0 core directive*",
    },
    DirectiveDoc {
        name: "include",
        markdown: "## @include\n\n\
            **Include content from another A2ML file.**\n\n\
            ```a2ml\n@include(\"path/to/other.a2ml\")\n```\n\n\
            The included file is parsed and its blocks are spliced into the \
            current document at the inclusion point.\n\n\
            *A2ML v1 directive*",
    },
    DirectiveDoc {
        name: "ref",
        markdown: "## @ref()\n\n\
            **Inline reference** to a declared ID.\n\n\
            ```a2ml\nSee @ref(my-id) for details.\n```\n\n\
            In checked and attested modes, the target ID **must** exist in the \
            document; in lax mode, unresolved references produce warnings.\n\n\
            *A2ML v0 inline directive*",
    },
    DirectiveDoc {
        name: "meta",
        markdown: "## @meta\n\n\
            **Metadata directive block.**\n\n\
            ```a2ml\n@meta:\nkey = \"value\"\n@end\n```\n\n\
            *A2ML v1 directive*",
    },
    DirectiveDoc {
        name: "note",
        markdown: "## @note\n\n\
            **Admonition: informational note.**\n\n\
            ```a2ml\n@note:\nImportant information here.\n@end\n```\n\n\
            *A2ML v1 directive*",
    },
    DirectiveDoc {
        name: "warning",
        markdown: "## @warning\n\n\
            **Admonition: warning callout.**\n\n\
            ```a2ml\n@warning:\nCritical caveat here.\n@end\n```\n\n\
            *A2ML v1 directive*",
    },
    DirectiveDoc {
        name: "example",
        markdown: "## @example\n\n\
            **Example block.**\n\n\
            ```a2ml\n@example:\nDemonstration content.\n@end\n```\n\n\
            *A2ML v1 directive*",
    },
    DirectiveDoc {
        name: "quote",
        markdown: "## @quote\n\n\
            **Block quotation.**\n\n\
            ```a2ml\n@quote:\nQuoted passage.\n@end\n```\n\n\
            *A2ML v1 directive*",
    },
    DirectiveDoc {
        name: "end",
        markdown: "## @end\n\n\
            **Closes the current directive block.**\n\n\
            Every `@directive:` that opens a block must be closed by a matching \
            `@end` on its own line.\n\n\
            *A2ML v0 core keyword*",
    },
];

// ── Section documentation ────────────────────────────────────────────

/// Documentation entry for a known [section] header.
struct SectionDoc {
    name: &'static str,
    markdown: &'static str,
}

/// Known section headers and their schema documentation.
const SECTION_DOCS: &[SectionDoc] = &[
    SectionDoc {
        name: "metadata",
        markdown: "## [metadata]\n\n\
            **Document metadata block.**\n\n\
            Required fields:\n\
            - `project` — project name\n\
            - `version` — semantic version\n\
            - `status`  — active | paused | archived | …\n\n\
            Optional fields:\n\
            - `author`, `license`, `created`, `updated`, `description`",
    },
    SectionDoc {
        name: "project-context",
        markdown: "## [project-context]\n\n\
            **Project background and goals.**\n\n\
            Free-form text describing what this project is, why it exists, \
            and what success looks like.",
    },
    SectionDoc {
        name: "dependencies",
        markdown: "## [dependencies]\n\n\
            **External and internal dependency list.**\n\n\
            Each dependency is a key = value pair:\n\
            ```\nsome-lib = \">=1.2.0\"\n```",
    },
    SectionDoc {
        name: "current-position",
        markdown: "## [current-position]\n\n\
            **Snapshot of where the project is right now.**\n\n\
            Fields: `completion`, `milestone`, `last-session-date`",
    },
    SectionDoc {
        name: "route-to-mvp",
        markdown: "## [route-to-mvp]\n\n\
            **Steps remaining to reach Minimum Viable Product.**\n\n\
            Ordered list of milestones with estimated effort.",
    },
    SectionDoc {
        name: "blockers-and-issues",
        markdown: "## [blockers-and-issues]\n\n\
            **Active blockers and known issues.**\n\n\
            Each blocker should have a severity and description.",
    },
    SectionDoc {
        name: "critical-next-actions",
        markdown: "## [critical-next-actions]\n\n\
            **Immediate action items.**\n\n\
            The most important things to do in the next work session.",
    },
    SectionDoc {
        name: "session-history",
        markdown: "## [session-history]\n\n\
            **Timestamped log of work sessions.**\n\n\
            Each entry should include date, what was done, and outcomes.",
    },
    SectionDoc {
        name: "architecture-decisions",
        markdown: "## [architecture-decisions]\n\n\
            **Architecture Decision Records (ADRs).**\n\n\
            Status values: proposed, accepted, deprecated, superseded, rejected.",
    },
    SectionDoc {
        name: "development-practices",
        markdown: "## [development-practices]\n\n\
            **Coding standards, workflow conventions, and team norms.**",
    },
    SectionDoc {
        name: "design-rationale",
        markdown: "## [design-rationale]\n\n\
            **Why decisions were made the way they were.**\n\n\
            Links design choices to requirements and constraints.",
    },
    SectionDoc {
        name: "ecosystem",
        markdown: "## [ecosystem]\n\n\
            **Ecosystem position and relationships.**\n\n\
            Fields: `type`, `purpose`, `position-in-ecosystem`.",
    },
    SectionDoc {
        name: "related-projects",
        markdown: "## [related-projects]\n\n\
            **Sibling, upstream, and downstream projects.**\n\n\
            Relationship types: sibling-standard, potential-consumer, inspiration.",
    },
    SectionDoc {
        name: "position-in-ecosystem",
        markdown: "## [position-in-ecosystem]\n\n\
            **Where this project fits in the larger picture.**",
    },
    SectionDoc {
        name: "agentic",
        markdown: "## [agentic]\n\n\
            **AI agent interaction patterns.**\n\n\
            Defines how AI assistants should interact with this project.",
    },
    SectionDoc {
        name: "neurosym",
        markdown: "## [neurosym]\n\n\
            **Neurosymbolic integration configuration.**\n\n\
            Settings for Hypatia and related neurosymbolic CI/CD tools.",
    },
    SectionDoc {
        name: "playbook",
        markdown: "## [playbook]\n\n\
            **Operational runbook and procedures.**\n\n\
            Step-by-step guides for common operations.",
    },
];

// ── Public entry point ───────────────────────────────────────────────

/// Produce a hover result for the token under the cursor.
///
/// Returns `Some(Hover)` with Markdown content when the cursor is on a
/// recognised directive or section header, `None` otherwise.
pub fn provide_hover(text: &str, position: Position) -> Option<Hover> {
    let lines: Vec<&str> = text.lines().collect();
    let line_idx = position.line as usize;

    if line_idx >= lines.len() {
        return None;
    }

    let line = lines[line_idx];

    // Try directive hover first, then section hover.
    if let Some(hover) = directive_hover(line, position) {
        return Some(hover);
    }

    section_hover(line, position)
}

// ── Sub-providers ────────────────────────────────────────────────────

// Literal patterns, compiled once.
//
// These were `Regex::new(<literal>).unwrap()` inside the functions below,
// which recompiled the pattern on every hover request and panicked the LSP
// task if the literal were ever malformed. Hoisting to a `LazyLock` compiles
// each pattern once, and `expect` states the invariant being relied on: the
// pattern is a compile-time constant, so a failure here is a programming
// error, not a runtime condition.
static DIRECTIVE_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"@([A-Za-z][A-Za-z0-9_-]*)").expect("DIRECTIVE_RE is a valid literal pattern")
});

static SECTION_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"^\[([A-Za-z][A-Za-z0-9_-]*)\]\s*$")
        .expect("SECTION_RE is a valid literal pattern")
});

/// Check whether the cursor is on an `@directive` name and return its docs.
fn directive_hover(line: &str, position: Position) -> Option<Hover> {
    // Match `@name` anywhere on the line.  We check whether the cursor column
    // falls within the match span.
    let re = &*DIRECTIVE_RE;
    let col = position.character as usize;

    for mat in re.find_iter(line) {
        if col >= mat.start() && col <= mat.end() {
            // Extract just the name part (skip the `@`).
            let name = &mat.as_str()[1..];

            if let Some(doc) = DIRECTIVE_DOCS.iter().find(|d| d.name == name) {
                return Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: doc.markdown.to_string(),
                    }),
                    range: Some(Range::new(
                        Position::new(position.line, mat.start() as u32),
                        Position::new(position.line, mat.end() as u32),
                    )),
                });
            }
        }
    }

    None
}

/// Check whether the cursor is on a `[section]` header and return its docs.
fn section_hover(line: &str, position: Position) -> Option<Hover> {
    let re = &*SECTION_RE;
    let trimmed = line.trim();

    if let Some(caps) = re.captures(trimmed) {
        // `?`, not unwrap: a panicking hover handler takes the editor's
        // language features down for the rest of the session.
        let name = caps.get(1)?.as_str();

        if let Some(doc) = SECTION_DOCS.iter().find(|s| s.name == name) {
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: doc.markdown.to_string(),
                }),
                range: Some(Range::new(
                    Position::new(position.line, 0),
                    Position::new(position.line, line.len() as u32),
                )),
            });
        }
    }

    None
}
