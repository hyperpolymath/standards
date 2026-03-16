// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
//
// mod.ts — Deno entry point for the @hyperpolymath/k9 library.
//
// Re-exports the compiled ReScript modules for use in Deno projects.
// All types and functions are available through this single entry point.

// @ts-nocheck — ReScript-generated ES modules do not ship .d.ts files

export {
  parse,
  parseFile,
  render,
  renderSecurityLevel,
  detectFormat,
  makeComponent,
  makePedigree,
  defaultSecurityPolicy,
  emptyRecipes,
  securityLevelFromString,
  securityLevelToString,
  parseErrorToString,
} from "./src/K9.res.mjs";

export {
  parseK9,
  parseK9File,
} from "./src/K9_Parser.res.mjs";

export {
  renderK9,
  renderSecurityLevel as renderSecurityLevelStr,
} from "./src/K9_Renderer.res.mjs";

export type {
  securityLevel,
  pedigree,
  securityPolicy,
  target,
  recipes,
  validation,
  contractClause,
  contract,
  component,
  parseError,
} from "./src/K9_Types.res.mjs";
