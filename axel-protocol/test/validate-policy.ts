// SPDX-License-Identifier: PMPL-1.0-or-later
// AXEL Protocol - Policy JSON Schema Validator
// Validates /.well-known/axel-policy examples against the JSON Schema.

import Ajv from "npm:ajv@8.17.1";
import addFormats from "npm:ajv-formats@3.0.1";
import { join, dirname, fromFileUrl } from "jsr:@std/path@1";

const ROOT = join(dirname(fromFileUrl(import.meta.url)), "..");

const schemaPath = join(ROOT, "schemas", "axel-policy.schema.json");
const policyPaths = [
  join(ROOT, "public", ".well-known", "axel-policy"),
  join(ROOT, "public", ".well-known", "axel-policy.example-adult.json"),
];

const schema = JSON.parse(await Deno.readTextFile(schemaPath));

const ajv = new Ajv({ allErrors: true, strict: false, validateSchema: false });
addFormats(ajv);
const validate = ajv.compile(schema);

let allPassed = true;

for (const policyPath of policyPaths) {
  const label = policyPath.replace(ROOT + "/", "");
  try {
    const policyText = await Deno.readTextFile(policyPath);
    const policy = JSON.parse(policyText);
    const valid = validate(policy);

    if (valid) {
      console.log(`PASS: ${label}`);
    } else {
      console.error(`FAIL: ${label}`);
      for (const err of validate.errors ?? []) {
        console.error(`  - ${err.instancePath || "/"}: ${err.message}`);
      }
      allPassed = false;
    }
  } catch (e) {
    console.error(`ERROR: ${label} - ${(e as Error).message}`);
    allPassed = false;
  }
}

if (allPassed) {
  console.log("\nAll policies valid.");
  Deno.exit(0);
} else {
  console.error("\nValidation failed.");
  Deno.exit(1);
}
