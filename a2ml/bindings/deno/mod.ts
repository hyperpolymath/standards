// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
//
// mod.ts — Deno entry point for the @hyperpolymath/a2ml library.
//
// Re-exports the compiled ReScript modules for use in Deno projects.
// All types and functions are available through this single entry point.

// @ts-nocheck — ReScript-generated ES modules do not ship .d.ts files

export {
  parse,
  parseFile,
  render,
  renderBlock,
  renderInline,
  emptyDocument,
  makeDirective,
  makeAttestation,
  manifestFromDocument,
  parseErrorToString,
  trustLevelFromString,
  trustLevelToString,
} from "./src/A2ML.res.mjs";

export {
  parseA2ML,
  parseA2MLFile,
  parseInlines,
} from "./src/A2ML_Parser.res.mjs";

export {
  renderA2ML,
  renderInline as renderInlineElement,
  renderInlines,
  renderDirective,
  renderAttestation,
} from "./src/A2ML_Renderer.res.mjs";

export type {
  trustLevel,
  inline,
  directive,
  attestation,
  block,
  document,
  manifest,
  parseError,
} from "./src/A2ML_Types.res.mjs";
