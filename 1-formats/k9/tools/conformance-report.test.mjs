// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
// hypatia: allow cicd_rules/javascript_detected -- Bun reporting boundary regression tests.
import { test } from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { counts, state, checkedAssertions, corpusItems, render, delta, baseline } from './conformance-report.mjs';
const row = (status, advisory = false) => ({ id: 'x', status, advisory, reason: '' });
const json = (file, verdict = 'pass', findings = []) => JSON.stringify({ file, verdict, findings });
test('known blocking failure remains red when other evidence is missing', () => {
  assert.equal(state([row('fail'), row('indeterminate')]), 'red');
  assert.equal(state([row('fail', true)]), 'yellow');
  assert.equal(state([row('fail', true), row('indeterminate')]), 'orange');
  assert.equal(state([row('not_applicable')]), 'grey');
  assert.equal(state([row('pass'), row('not_applicable')]), 'green');
  assert.equal(state([]), 'orange');
});
test('empty, partial and interrupted assertion streams never become a pass', () => {
  assert.equal(state(checkedAssertions('', 0)), 'orange');
  assert.equal(state(checkedAssertions('ok   one', 0, 2)), 'orange');
  assert.equal(state(checkedAssertions('ok   one', 1)), 'orange');
  assert.equal(state(checkedAssertions('FAIL one', 0)), 'red');
  assert.equal(state(checkedAssertions('ok   one\nSKIP two', 0)), 'orange');
});
test('missing or malformed per-file evidence is indeterminate', () => {
  assert.equal(state(corpusItems('', ['a'], new Set(), new Set())), 'orange');
  assert.equal(state(corpusItems('not json', ['a'], new Set(), new Set())), 'orange');
  assert.equal(state(corpusItems(json('a') + '\n' + json('a'), ['a'], new Set(), new Set())), 'orange');
  assert.equal(state(corpusItems(json('b'), ['a'], new Set(), new Set())), 'orange');
});
test('missing semantic verifier is not a conformance pass', () => {
  const result = corpusItems(json('a', 'pass', [{ severity: 'skipped', rule: 'K9-N000', message: 'no nickel' }]), ['a'], new Set(), new Set());
  assert.equal(state(result), 'orange');
  assert.equal(counts(result).indeterminate, 1);
});
test('debt is advisory only while untouched, and conforming exemptions fail', () => {
  const data = json('a', 'fail', [{ severity: 'error', rule: 'K9-E001', message: 'invalid' }]);
  assert.equal(state(corpusItems(data, ['a'], new Set(['a']), new Set())), 'yellow');
  assert.equal(state(corpusItems(data, ['a'], new Set(['a']), new Set(['a']))), 'red');
  assert.equal(state(corpusItems(json('a'), ['a'], new Set(['a']), new Set())), 'red');
});
test('routed dialects are excluded rather than counted as validated contracts', () => {
  const items = corpusItems(json('coordination.k9', 'routed'), ['coordination.k9'], new Set(), new Set());
  assert.equal(counts(items).passed, 0);
  assert.equal(counts(items).not_applicable, 1);
});
test('comparisons require the same scope/profile and a different revision', () => {
  const c = { id: 'corpus', counts: counts([row('pass')]) };
  const current = { schema: 1, profile: 'p', pr: '12', sha: 'new', categories: [c] };
  const previous = { ...current, sha: 'old', categories: [{ ...c, counts: counts([row('fail')]) }] };
  assert.match(delta(current, previous, c), /improved/);
  assert.match(delta(current, { ...previous, profile: 'other' }, c), /no comparable/);
  assert.match(delta(current, current, c), /no comparable/);
  assert.match(delta(current, { ...previous, categories: [{ ...c, counts: counts([row('indeterminate')]) }] }, c), /indeterminate/);
});
test('report escapes untrusted evidence and preserves a small valid baseline', () => {
  const items = [{ id: '</details><script>x</script>', status: 'indeterminate', reason: 'bad | value\nnext' }];
  const r = { schema: 1, state: 'orange', sha: 'abc', profile: 'p', pr: '1', run: 'local', url: '', categories: [{ id: 'corpus', title: 'Corpus', items, counts: counts(items), state: 'orange' }] };
  const body = render(r);
  assert.ok(!body.includes('<script>'));
  assert.ok(body.includes('&#124;'));
  assert.equal(baseline(body).sha, 'abc');
  assert.equal(baseline('<!-- k9-baseline:xxx -->'), null);
});
test('the actual corpus shell gate rejects empty output and unattributed failure', () => {
  const tmp = fs.mkdtempSync(path.join(os.tmpdir(), 'k9-report-'));
  try {
    for (const dir of ['.githooks', '1-formats/k9/tools']) fs.mkdirSync(path.join(tmp, dir), { recursive: true });
    fs.copyFileSync(new URL('../../../.githooks/validate-k9.sh', import.meta.url), path.join(tmp, '.githooks/validate-k9.sh'));
    fs.writeFileSync(path.join(tmp, 'a.k9.ncl'), 'K9!\n');
    spawnSync('git', ['init', '-q'], { cwd: tmp });
    spawnSync('git', ['add', 'a.k9.ncl'], { cwd: tmp });
    const validator = path.join(tmp, '1-formats/k9/tools/k9-validate.sh');
    for (const body of ['exit 0', 'exit 1', `echo '${json('a.k9.ncl')}'\nexit 1`]) {
      fs.writeFileSync(validator, '#!/bin/bash\n' + body + '\n');
      const r = spawnSync('bash', ['.githooks/validate-k9.sh'], { cwd: tmp, encoding: 'utf8' });
      assert.notEqual(r.status, 0, r.stdout + r.stderr);
    }
  } finally { fs.rmSync(tmp, { recursive: true, force: true }); }
});
