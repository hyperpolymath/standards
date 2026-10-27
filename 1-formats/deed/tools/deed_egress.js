#!/usr/bin/env bun
// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
const DOC = String.raw`deed-egress — profile check for the (egress …) clause of a repo deed
(spec/abnf/clauses/egress.abnf v1.0.0; prose: spec/EGRESS-CLAUSE-SPEC.adoc).

The file must first pass deed_lint (deed.abnf); this checks only what the
clause profile adds. A deed with no (egress …) clause passes.

Usage:
  deed_egress.js FILE.deed ...
  deed_egress.js --fixtures DIR   # DIR/valid/*.deed pass, DIR/invalid/*.deed fail
Exit: 0 all pass, 1 any failure, 2 usage.`;

import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { validate, LintError } from "./deed_lint.js";

/** A profile violation. The parsed AST carries no positions, so it names the field instead of a line. */
export class EgressError extends LintError {
  constructor(msg) {
    super(msg, null);
    this.name = "EgressError";
  }

  toString() {
    return this.msg;
  }
}

export const ENFORCEMENT = ["none", "harden-runner-audit", "harden-runner-block"];

const WORKFLOW = /^\.github\/workflows\/[A-Za-z0-9._-]+\.ya?ml$/;
const JOB_ID = /^[A-Za-z_][A-Za-z0-9_-]*$/;
const SECRET = /^[A-Za-z_][A-Za-z0-9_]*$/;
const LABEL = "[A-Za-z0-9][A-Za-z0-9-]*";
const HOST_PORT = new RegExp(`^(\\*\\.)?${LABEL}(\\.${LABEL})*(:([0-9]{1,5}))?$`);
const REQUIRED = ["workflow", "job", "secrets", "endpoints", "enforcement"];
const ALLOWED = new Set([...REQUIRED, "reason"]);

/** Collect string items of a list value, or throw naming the field. */
function stringList(val, field) {
  if (val[0] !== "list") throw new EgressError(`:${field} must be a list of strings`);
  return val[1].map((v) => {
    if (v[0] !== "string") throw new EgressError(`:${field} items must be strings, got ${v[0]}`);
    return v[1];
  });
}

/** Check one (job …) clause; returns {workflow, job, secrets, endpoints, enforcement}. */
function checkJob(items) {
  const seen = new Map();
  for (const it of items) {
    if (it[0] !== "field") throw new EgressError(`(job …) takes fields only, not a nested (${it[1]} …)`);
    if (!ALLOWED.has(it[1])) throw new EgressError(`(job …) has no field :${it[1]}`);
    if (seen.has(it[1])) throw new EgressError(`(job …) repeats :${it[1]}`);
    seen.set(it[1], it[2]);
  }
  for (const f of REQUIRED) if (!seen.has(f)) throw new EgressError(`(job …) is missing :${f}`);

  const str = (f) => {
    const v = seen.get(f);
    if (v[0] !== "string") throw new EgressError(`:${f} must be a string`);
    return v[1];
  };
  const workflow = str("workflow");
  if (!WORKFLOW.test(workflow)) throw new EgressError(`:workflow ${JSON.stringify(workflow)} is not .github/workflows/<name>.yml`);
  const job = str("job");
  if (!JOB_ID.test(job)) throw new EgressError(`:job ${JSON.stringify(job)} is not a GitHub job id`);
  if (seen.has("reason")) str("reason");

  const secrets = stringList(seen.get("secrets"), "secrets");
  if (secrets.length === 0) throw new EgressError(":secrets must not be empty");
  for (const s of secrets) {
    if (!SECRET.test(s)) throw new EgressError(`secret name ${JSON.stringify(s)} is malformed`);
    if (s === "GITHUB_TOKEN") throw new EgressError("GITHUB_TOKEN is not listed: it is the job token, bounded by permissions:");
  }
  if (new Set(secrets).size !== secrets.length) throw new EgressError(":secrets has a duplicate");

  const endpoints = stringList(seen.get("endpoints"), "endpoints");
  for (const e of endpoints) {
    if (e.startsWith("secret:")) {
      const name = e.slice("secret:".length);
      if (!SECRET.test(name)) throw new EgressError(`endpoint ${JSON.stringify(e)} names a malformed secret`);
      if (!secrets.includes(name)) throw new EgressError(`endpoint ${JSON.stringify(e)} names a secret not in :secrets`);
      continue;
    }
    if (e.startsWith("var:")) {
      const name = e.slice("var:".length);
      if (!SECRET.test(name)) throw new EgressError(`endpoint ${JSON.stringify(e)} names a malformed variable`);
      continue;
    }
    const m = HOST_PORT.exec(e);
    if (!m) throw new EgressError(`endpoint ${JSON.stringify(e)} is not host[:port], secret:NAME or var:NAME`);
    if (m[4] !== undefined && (Number(m[4]) < 1 || Number(m[4]) > 65535)) {
      throw new EgressError(`endpoint ${JSON.stringify(e)} has a port outside 1..65535`);
    }
  }

  const enf = seen.get("enforcement");
  if (enf[0] !== "symbol" || !ENFORCEMENT.includes(enf[1])) {
    throw new EgressError(`:enforcement must be one of the symbols ${ENFORCEMENT.join(", ")}`);
  }
  return { workflow, job, secrets, endpoints, enforcement: enf[1] };
}

/**
 * Check the (egress …) clause of deed source text. Returns the declared jobs
 * ([] when the deed has no egress clause); throws LintError on a grammar or
 * profile violation.
 */
export function checkEgress(text, filename = null) {
  const [head, items] = validate(text, filename);
  const clauses = items.filter((it) => it[0] === "clause" && it[1] === "egress");
  if (clauses.length === 0) return [];
  if (head !== "repo-deed") throw new EgressError(`(egress …) belongs in a repo-deed, not a ${head}`);
  if (clauses.length > 1) throw new EgressError("a repo deed carries at most one (egress …) clause");

  const jobs = [];
  let policies = 0;
  for (const it of clauses[0][2]) {
    if (it[0] === "field" && it[1] === "policy") {
      if (it[2][0] !== "string") throw new EgressError(":policy must be a string");
      policies += 1;
    } else if (it[0] === "clause" && it[1] === "job") {
      jobs.push(checkJob(it[2]));
    } else {
      throw new EgressError(`(egress …) takes :policy and (job …) only, not ${it[0] === "field" ? ":" : "("}${it[1]}`);
    }
  }
  if (policies > 1) throw new EgressError("(egress …) repeats :policy");
  const keys = jobs.map((j) => `${j.workflow}#${j.job}`);
  if (new Set(keys).size !== keys.length) throw new EgressError("(egress …) lists the same workflow job twice");
  return jobs;
}

/** valid/ must pass; invalid/ must fail. Returns exit code. */
function fixtures(d) {
  let bad = 0;
  for (const [sub, expect] of [["valid", true], ["invalid", false]]) {
    let names = [];
    try {
      names = readdirSync(join(d, sub)).filter((n) => n.endsWith(".deed")).sort();
    } catch {
      // missing directory: nothing to check
    }
    for (const n of names) {
      const f = join(d, sub, n);
      let got = true;
      let err = "";
      try {
        checkEgress(readFileSync(f, "utf8"), f);
      } catch (e) {
        if (!(e instanceof LintError)) throw e;
        got = false;
        err = e.message;
      }
      if (got !== expect) bad += 1;
      console.log(`${got === expect ? "PASS " : "FAIL "}${f}` + (got === expect ? "" : `  (unexpected: ${err || "valid"})`));
    }
  }
  return bad ? 1 : 0;
}

function main(argv) {
  if (argv.includes("--fixtures")) {
    const d = argv[argv.indexOf("--fixtures") + 1];
    if (d === undefined) {
      console.log("--fixtures requires a DIR argument");
      return 2;
    }
    const rc = fixtures(d);
    console.log("FIXTURES " + (rc === 0 ? "OK" : "FAILED"));
    return rc;
  }
  const files = argv.filter((a) => !a.startsWith("-"));
  if (files.length === 0) {
    console.log(DOC);
    return 2;
  }
  let bad = 0;
  for (const f of files) {
    try {
      const jobs = checkEgress(readFileSync(f, "utf8"), f);
      console.log(`OK   ${f} (${jobs.length} egress job${jobs.length === 1 ? "" : "s"})`);
    } catch (e) {
      if (!(e instanceof LintError)) throw e;
      bad += 1;
      console.log(`FAIL ${f}: ${e.message}`);
    }
  }
  return bad ? 1 : 0;
}

if (import.meta.main) process.exit(main(process.argv.slice(2)));
