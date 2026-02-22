// SPDX-License-Identifier: PMPL-1.0-or-later

// A2ML WASM prototype using rescript-wasm-runtime.
// This is a minimal demo intended for local testing only.

// Assumes rescript-wasm-runtime is available locally.
// You can add it to a workspace or compile this in a repo where
// rescript-wasm-runtime's compiled JS is on the module path.

let buildWasm = async () => {
  let ok = await Wasm.compileToWasm("fixtures/add.wat", "build/add.wasm", ())
  if !ok {
    Js.log("WASM compile failed")
  }
}

let runWasm = async () => {
  let instance = await Wasm.loadModule("build/add.wasm", ())
  let exports: Js.t<{. add: (int, int) => int}> = %raw(`instance.exports`)
  let result = exports##add(2, 3)
  Js.log(`add(2,3) = ${Int.toString(result)}`)
}

let _ = {
  let _ = buildWasm()
  let _ = runWasm()
}
