// SPDX-License-Identifier: MIT
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// Runtime entry point — what VS Code loads via package.json `main`.
//
// Pipeline:
//   src/extension.affine   ──affinescript compile──>   out/extension.cjs
//   src/index.cjs          ──this file──>             exports.{activate,deactivate}
//
// Wires the vendored affine-vscode adapter into the wasm shim's
// `extraImports` hook before activation, so AffineScript extern fns
// declared in stdlib/Vscode.affine + stdlib/VscodeLanguageClient.affine
// resolve to live vscode / vscode-languageclient API calls.

"use strict";

const shim = require("../out/extension.cjs");
const makeVscodeBindings = require("./affine-vscode-adapter.cjs");

shim.extraImports = function extraImports() {
  return makeVscodeBindings(
    require("vscode"),
    require("vscode-languageclient/node"),
    shim
  );
};

exports.activate = shim.activate;
exports.deactivate = shim.deactivate;
