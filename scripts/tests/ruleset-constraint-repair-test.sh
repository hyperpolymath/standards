#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
set -euo pipefail
ruby - "$(dirname "$0")/../plan-ruleset-constraint-repair.rb" <<'RUBY'
require File.expand_path(ARGV.fetch(0))
retained = [
  { 'type' => 'required_signatures' },
  { 'type' => 'pull_request', 'parameters' => { 'required_review_thread_resolution' => true } },
  { 'type' => 'required_status_checks', 'parameters' => { 'required_status_checks' => [
    { 'context' => 'scan / gitleaks', 'integration_id' => 15368 }] } },
  { 'type' => 'code_scanning', 'parameters' => { 'code_scanning_tools' => ['CodeQL'] } }
]
fixture = {
  'id' => 123, 'name' => 'Existing name', 'target' => 'branch', 'enforcement' => 'active',
  'conditions' => { 'ref_name' => { 'include' => ['~DEFAULT_BRANCH'], 'exclude' => [] } },
  'bypass_actors' => [], 'rules' => retained + RulesetConstraintRepair::RETIRED.map { |type| { 'type' => type } }
}
result = RulesetConstraintRepair.plan(fixture)
raise 'Changed active protections' unless result.fetch('rules') == retained
raise 'Changed bypass actors' unless result.fetch('bypass_actors') == []
raise 'Mutated input' unless fixture.fetch('rules').length == 8
raise 'Not idempotent' unless RulesetConstraintRepair.plan(result) == result
actors = [
  { 'actor_type' => 'Integration', 'actor_id' => 288115, 'bypass_mode' => 'always' },
  { 'actor_type' => 'RepositoryRole', 'actor_id' => 288115, 'bypass_mode' => 'pull_request' },
  { 'actor_type' => 'Integration', 'actor_id' => 15368, 'bypass_mode' => 'always' }
]
with_actors = fixture.merge('bypass_actors' => actors)
raise 'Changed actors without explicit selection' unless RulesetConstraintRepair.plan(with_actors)['bypass_actors'] == actors
without_stale = RulesetConstraintRepair.plan(with_actors, uninstalled_app_id: 288115)
raise 'Removed an unrelated bypass' unless without_stale['bypass_actors'] == actors.drop(1)
begin
  RulesetConstraintRepair.plan(with_actors, uninstalled_app_id: 999)
  raise 'Accepted an unknown app'
rescue ArgumentError
  # An explicit, present integration ID is required.
end
['tag', nil].each do |bad_target|
  begin
    RulesetConstraintRepair.plan(fixture.merge('target' => bad_target))
    raise 'Accepted unsupported scope'
  rescue ArgumentError
    # Expected: malformed or unsupported scopes are never rewritten.
  end
end
puts 'PASS: only four retired constraints removed; CI, signatures, reviews, scans, scope, and bypass preserved'
RUBY
