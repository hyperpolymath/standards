// SPDX-License-Identifier: PMPL-1.0-or-later

open Compat

// Simple JSON encoding without external dependencies
let escapeString = (s: string): string => {
  // Basic escaping for JSON strings
  let s1 = replace(s, "\\", "\\\\")
  let s2 = replace(s1, "\"", "\\\"")
  let s3 = replace(s2, "\n", "\\n")
  s3
}

let inlineToJson = (part: A2ml.inline): string => {
  switch part {
  | Text(t) =>
      `{"type":"text","text":"${escapeString(t)}"}`
  | Emph(t) =>
      `{"type":"emph","text":"${escapeString(t)}"}`
  | Strong(t) =>
      `{"type":"strong","text":"${escapeString(t)}"}`
  | Link(label, url) =>
      `{"type":"link","label":"${escapeString(label)}","url":"${escapeString(url)}"}`
  }
}

let rec blockToJson = (block: A2ml.block): string => {
  switch block {
  | Heading(level, text) =>
      `{"type":"heading","level":${intToString(level)},"text":"${escapeString(text)}"}`
  | Paragraph(parts) =>
      let items = parts->arrayMap(inlineToJson)->arrayJoin(",")
      `{"type":"paragraph","content":[${items}]}`
  | List(items) =>
      let itemsJson = items->arrayMap(parts => {
        let content = parts->arrayMap(inlineToJson)->arrayJoin(",")
        `[${content}]`
      })->arrayJoin(",")
      `{"type":"list","items":[${itemsJson}]}`
  | Directive(name, attrs, body) =>
      let attrsJson = attrs->arrayMap(((k, v)) => `"${escapeString(k)}":"${escapeString(v)}"`)->arrayJoin(",")
      let bodyJson = body->arrayMap(blockToJson)->arrayJoin(",")
      `{"type":"directive","name":"${escapeString(name)}","attrs":{${attrsJson}},"body":[${bodyJson}]}`
  }
}

let docToJson = (doc: A2ml.doc): string => {
  let blocks = doc->arrayMap(blockToJson)->arrayJoin(",")
  `{"blocks":[${blocks}]}`
}
