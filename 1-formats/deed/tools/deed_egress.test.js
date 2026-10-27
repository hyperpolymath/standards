// SPDX-License-Identifier: MPL-2.0
// bun test for deed_egress.js. Run: bun test 1-formats/deed/tools/
import { describe, expect, test } from "bun:test";
import { readdirSync, readFileSync } from "node:fs";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { checkEgress, EgressError } from "./deed_egress.js";
import { validate } from "./deed_lint.js";

const HERE = dirname(fileURLToPath(import.meta.url));
const FIX = join(HERE, "fixtures", "egress");
const deeds = (sub) => readdirSync(join(FIX, sub)).filter((n) => n.endsWith(".deed")).sort();

describe("egress fixtures", () => {
  for (const n of deeds("valid")) {
    test(`valid/${n} passes`, () => {
      const f = join(FIX, "valid", n);
      expect(() => checkEgress(readFileSync(f, "utf8"), f)).not.toThrow();
    });
  }
  for (const n of deeds("invalid")) {
    // Every invalid fixture is a grammatical deed: it must fail on the
    // profile, never on deed.abnf, or it tests the wrong layer.
    test(`invalid/${n} is a valid deed but fails the profile`, () => {
      const f = join(FIX, "invalid", n);
      const src = readFileSync(f, "utf8");
      expect(() => validate(src, f)).not.toThrow();
      expect(() => checkEgress(src, f)).toThrow(EgressError);
    });
  }
});

describe("checkEgress result", () => {
  test("returns the declared jobs", () => {
    const f = join(FIX, "valid", "template-pair_chora.deed");
    const jobs = checkEgress(readFileSync(f, "utf8"), f);
    expect(jobs).toEqual([
      {
        workflow: ".github/workflows/push-email-notify.yml",
        job: "notify",
        secrets: ["SMTP_HOST", "SMTP_PORT", "SMTP_USER", "SMTP_PASS"],
        endpoints: ["secret:SMTP_HOST"],
        enforcement: "none",
      },
      {
        workflow: ".github/workflows/sonarqube.yml",
        job: "sonarqube",
        secrets: ["SONAR_TOKEN"],
        endpoints: ["sonarcloud.io", "*.sonarcloud.io:443"],
        enforcement: "harden-runner-block",
      },
    ]);
  });

  test("a deed without the clause declares nothing", () => {
    const f = join(FIX, "valid", "no-egress_chora.deed");
    expect(checkEgress(readFileSync(f, "utf8"), f)).toEqual([]);
  });

  test("the clause is refused outside a repo-deed", () => {
    const src = ';; SPDX-License-Identifier: CC-BY-SA-4.0\n(praxis-deed :schema-version "1.0.0" (egress))\n';
    expect(() => checkEgress(src, "tool_praxis.deed")).toThrow(EgressError);
  });
});
