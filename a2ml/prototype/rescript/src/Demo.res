// SPDX-License-Identifier: PMPL-1.0-or-later

@val external consoleLog: 'a => unit = "console.log"

let demoDoc = "# A2ML Overview\n\n@abstract:\nA2ML is a typed, attested markup format.\n@end\n\n## Claims\n- Required sections must exist.\n- References must resolve.\n"

let _ = {
  let parsed = A2ml.parse(demoDoc)
  let html = A2ml.renderHtml(parsed)
  let errors = A2ml.validateChecked(parsed)
  // Replace with DOM updates in a real web integration.
  consoleLog(html)
  consoleLog(errors)
}
