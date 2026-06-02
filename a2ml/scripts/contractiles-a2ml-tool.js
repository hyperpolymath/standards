#!/usr/bin/env -S deno run --allow-read --allow-write
// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// Contractiles A2ML validator/emitter (Deno port of the former Python tool;
// Python is banned estate-wide). JSON output is byte-compatible with the
// previous `json.dumps(obj, indent=2, sort_keys=True) + "\n"` form.

const SPEC_VERSION = "1.0.0";

const REQUIRED = {
  mustfile: {
    filename: "Mustfile.a2ml",
    sections: ["Parameters", "Checks"],
    parameters: ["gateway_port", "schema_version"],
    item_fields: ["description", "run"],
  },
  trustfile: {
    filename: "Trustfile.a2ml",
    sections: ["Inputs", "Verifications"],
    inputs: [
      "policy_path",
      "policy_hash_path",
      "schema_path",
      "schema_sig_path",
      "schema_pub_path",
      "driver_paths",
      "migrations_path",
      "migrations_sig_path",
      "migrations_pub_path",
    ],
    item_fields: ["description", "command"],
  },
  dustfile: {
    filename: "Dustfile.a2ml",
    sections: ["Logs", "Policy", "Gateway", "Dust-Events"],
    fields_by_section: {
      "Logs": ["path", "reversible", "handler"],
      "Policy": ["path", "rollback"],
      "Gateway": ["event", "undo"],
      "Dust-Events": ["source", "transform"],
    },
  },
  intentfile: {
    filename: "Intentfile.a2ml",
    sections: ["Trust-Engine", "Control-Plane", "Pipeline", "Introspection"],
  },
};

const NAME_TO_TYPE = Object.fromEntries(
  Object.entries(REQUIRED).map(([k, v]) => [v.filename, k]),
);

function baseName(p) {
  const parts = p.split("/");
  return parts[parts.length - 1];
}

function parseA2ml(path) {
  let section = null;
  let item = null;
  const data = { sections: {}, items: {}, bullets: {} };

  const flushItem = () => {
    if (item === null) return;
    (data.items[section] ??= []).push(item);
    item = null;
  };

  const text = Deno.readTextFileSync(path);
  for (const raw of text.split("\n")) {
    const line = raw.replace(/\s+$/, "");
    if (line.startsWith("## ")) {
      flushItem();
      section = line.slice(3).trim();
      if (!(section in data.sections)) data.sections[section] = true;
      continue;
    }
    if (line.startsWith("### ")) {
      flushItem();
      item = { name: line.slice(4).trim() };
      continue;
    }
    if (!line.startsWith("- ")) continue;

    const content = line.slice(2).trim();
    if (content.includes(":")) {
      const idx = content.indexOf(":");
      const key = content.slice(0, idx).trim();
      const value = content.slice(idx + 1).trim();
      if (item !== null) {
        item[key] = value;
      } else {
        (data.bullets[section] ??= []).push({ [key]: value });
      }
    } else {
      (data.bullets[section] ??= []).push(content);
    }
  }

  flushItem();
  return data;
}

function resolveType(path, explicit) {
  if (explicit) {
    if (!(explicit in REQUIRED)) return null;
    return explicit;
  }
  return NAME_TO_TYPE[baseName(path)] ?? null;
}

function validate(path, explicitType) {
  const docType = resolveType(path, explicitType);
  if (!docType) return [`Unsupported file: ${baseName(path)}`];

  const req = REQUIRED[docType];
  const parsed = parseA2ml(path);
  const errors = [];

  for (const section of req.sections ?? []) {
    if (!(section in parsed.sections)) errors.push(`Missing section: ${section}`);
  }

  if (docType === "mustfile") {
    const params = {};
    for (const entry of parsed.bullets["Parameters"] ?? []) {
      if (entry && typeof entry === "object") Object.assign(params, entry);
    }
    for (const key of req.parameters ?? []) {
      if (!(key in params) || params[key] === "") errors.push(`Missing parameter: ${key}`);
    }

    const checks = parsed.items["Checks"] ?? [];
    const seen = new Set();
    for (const check of checks) {
      const cname = check.name ?? "";
      if (!cname) {
        errors.push("Check missing name");
        continue;
      }
      if (seen.has(cname)) errors.push(`Duplicate check name: ${cname}`);
      seen.add(cname);
      for (const field of req.item_fields ?? []) {
        if (!(field in check) || check[field] === "") {
          errors.push(`Check '${cname}' missing field: ${field}`);
        }
      }
    }
  } else if (docType === "trustfile") {
    const inputs = {};
    for (const entry of parsed.bullets["Inputs"] ?? []) {
      if (entry && typeof entry === "object") Object.assign(inputs, entry);
    }
    for (const key of req.inputs ?? []) {
      if (!(key in inputs) || inputs[key] === "") errors.push(`Missing input: ${key}`);
    }

    const verifications = parsed.items["Verifications"] ?? [];
    for (const verification of verifications) {
      const vname = verification.name ?? "";
      if (!vname) {
        errors.push("Verification missing name");
        continue;
      }
      for (const field of req.item_fields ?? []) {
        if (!(field in verification) || verification[field] === "") {
          errors.push(`Verification '${vname}' missing field: ${field}`);
        }
      }
    }
  } else if (docType === "dustfile") {
    for (const [section, fields] of Object.entries(req.fields_by_section ?? {})) {
      const items = parsed.items[section] ?? [];
      if (items.length === 0) {
        errors.push(`Section '${section}' has no entries`);
        continue;
      }
      for (const entry of items) {
        const ename = entry.name ?? "";
        if (!ename) {
          errors.push(`${section} entry missing name`);
          continue;
        }
        for (const field of fields) {
          if (!(field in entry) || entry[field] === "") {
            errors.push(`${section} '${ename}' missing field: ${field}`);
          }
        }
      }
    }
  } else if (docType === "intentfile") {
    for (const section of req.sections ?? []) {
      const bullets = parsed.bullets[section] ?? [];
      if (bullets.length === 0) {
        errors.push(`Section '${section}' must include at least one item`);
      }
    }
  }

  return errors;
}

function emit(path, explicitType) {
  const docType = resolveType(path, explicitType);
  if (!docType) {
    console.error(`Unsupported file: ${baseName(path)}`);
    Deno.exit(1);
  }

  const parsed = parseA2ml(path);

  if (docType === "mustfile") {
    const parameters = {};
    for (const entry of parsed.bullets["Parameters"] ?? []) {
      if (entry && typeof entry === "object") Object.assign(parameters, entry);
    }
    const checks = parsed.items["Checks"] ?? [];
    return { type: "mustfile", spec_version: SPEC_VERSION, parameters, checks };
  }

  if (docType === "trustfile") {
    const inputs = {};
    for (const entry of parsed.bullets["Inputs"] ?? []) {
      if (entry && typeof entry === "object") Object.assign(inputs, entry);
    }
    const verifications = parsed.items["Verifications"] ?? [];
    return { type: "trustfile", spec_version: SPEC_VERSION, inputs, verifications };
  }

  if (docType === "dustfile") {
    const sections = {};
    for (const section of REQUIRED[docType].sections) {
      sections[section] = parsed.items[section] ?? [];
    }
    return { type: "dustfile", spec_version: SPEC_VERSION, sections };
  }

  if (docType === "intentfile") {
    const future = {};
    for (const section of REQUIRED[docType].sections) {
      const items = parsed.bullets[section] ?? [];
      future[section] = items.map((it) =>
        typeof it === "string" ? it : Object.values(it)[0]
      );
    }
    return { type: "intentfile", spec_version: SPEC_VERSION, future };
  }

  console.error(`Unsupported file: ${baseName(path)}`);
  Deno.exit(1);
}

// Recursively sort object keys, then serialise to match Python's
// json.dumps(obj, indent=2, sort_keys=True) including ensure_ascii=True.
function sortDeep(value) {
  if (Array.isArray(value)) return value.map(sortDeep);
  if (value && typeof value === "object") {
    const out = {};
    for (const k of Object.keys(value).sort()) out[k] = sortDeep(value[k]);
    return out;
  }
  return value;
}

function asciiEscape(s) {
  let out = "";
  for (const ch of s) {
    const cp = ch.codePointAt(0);
    if (cp > 0x7f) {
      if (cp > 0xffff) {
        const u = cp - 0x10000;
        const hi = 0xd800 + (u >> 10);
        const lo = 0xdc00 + (u & 0x3ff);
        out += "\\u" + hi.toString(16).padStart(4, "0") +
          "\\u" + lo.toString(16).padStart(4, "0");
      } else {
        out += "\\u" + cp.toString(16).padStart(4, "0");
      }
    } else {
      out += ch;
    }
  }
  return out;
}

function pyJson(obj) {
  return asciiEscape(JSON.stringify(sortDeep(obj), null, 2)) + "\n";
}

function parseArgs(argv) {
  const cmd = argv[0];
  if (cmd === "validate") {
    const files = [];
    let type = null;
    for (let i = 1; i < argv.length; i++) {
      if (argv[i] === "--type") {
        type = argv[++i];
      } else {
        files.push(argv[i]);
      }
    }
    if (files.length === 0) {
      console.error("usage: contractiles-a2ml-tool validate FILES... [--type TYPE]");
      Deno.exit(2);
    }
    return { cmd, files, type };
  }
  if (cmd === "emit") {
    const positional = [];
    let type = null;
    for (let i = 1; i < argv.length; i++) {
      if (argv[i] === "--type") {
        type = argv[++i];
      } else {
        positional.push(argv[i]);
      }
    }
    if (positional.length < 2) {
      console.error("usage: contractiles-a2ml-tool emit INPUT OUTPUT [--type TYPE]");
      Deno.exit(2);
    }
    return { cmd, input: positional[0], output: positional[1], type };
  }
  console.error("usage: contractiles-a2ml-tool {validate,emit} ...");
  Deno.exit(2);
}

function main() {
  const args = parseArgs(Deno.args);

  if (args.cmd === "validate") {
    const allErrors = [];
    for (const f of args.files) {
      for (const err of validate(f, args.type)) {
        allErrors.push(`${baseName(f)}: ${err}`);
      }
    }
    if (allErrors.length > 0) {
      for (const err of allErrors) console.error(err);
      return 1;
    }
    return 0;
  }

  if (args.cmd === "emit") {
    const output = emit(args.input, args.type);
    Deno.writeTextFileSync(args.output, pyJson(output));
    return 0;
  }

  return 1;
}

Deno.exit(main());
