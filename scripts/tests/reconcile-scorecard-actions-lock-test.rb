#!/usr/bin/env ruby
# SPDX-License-Identifier: MPL-2.0
# Prove native-lock reconciliation is narrow and verifier failures stay failures.
require 'tmpdir'
require 'fileutils'
require 'minitest/autorun'
require 'open3'
require 'rbconfig'
require_relative '../reconcile-scorecard-actions-lock'

class ScorecardActionsLockTest < Minitest::Test
  # Build a temporary repository root with one workflow, a placeholder lock and
  # a stub `gh` on PATH that records invocations and replays canned verifier
  # output (shared, or per workflow from responses/).
  def setup
    @root = Dir.mktmpdir('scorecard-lock-')
    @old_path = ENV.fetch('PATH')
    FileUtils.mkdir_p(File.join(@root, '.github/workflows'))
    File.write(File.join(@root, '.github/workflows/ci.yml'), "jobs:\n  check:\n    steps:\n      - uses: actions/checkout@v7\n      - run: \"echo 'uses: untrusted/action@main'\"\n")
    File.write(File.join(@root, '.github/workflows/actions.lock'), 'fixture; semantics belong to gh actions-lock')
    File.write(File.join(@root, 'gh'), <<~SH)
      #!/bin/sh
      printf '%s\\n' "$*" >> invocation
      case "$*" in
        actions-lock\\ .github/workflows/*.yml\\ --verify\\ --no-interactive\\ --json=valid,findings) ;;
        *) exit 97 ;;
      esac
      # Per-workflow responses: responses/<basename>.json (+ .exit) override
      # the shared TEST_LOCK_JSON / TEST_LOCK_EXIT, so one run can mix a
      # verifying workflow with a failing one.
      name=$(basename "$2")
      if [ -f "responses/$name.json" ]; then
        cat "responses/$name.json"
        exit "$(cat "responses/$name.exit" 2>/dev/null || echo 0)"
      fi
      printf '%s' "$TEST_LOCK_JSON"
      exit "${TEST_LOCK_EXIT:-0}"
    SH
    File.chmod(0o755, File.join(@root, 'gh'))
    ENV['PATH'] = "#{@root}:#{@old_path}"
    ENV['TEST_LOCK_JSON'] = '{"valid":true,"findings":[]}'
  end

  def teardown
    ENV['PATH'] = @old_path
    ENV.delete('TEST_LOCK_JSON')
    ENV.delete('TEST_LOCK_EXIT')
    FileUtils.remove_entry(@root)
  end

  def finding(rule: 'PinnedDependenciesID', message: "score is 3: GitHub-owned GitHubAction not pinned by hash\nRemediation", path: '.github/workflows/ci.yml', line: 4)
    { 'ruleId' => rule, 'message' => { 'text' => message }, 'locations' => [
      { 'physicalLocation' => { 'artifactLocation' => { 'uri' => path }, 'region' => { 'startLine' => line } } }
    ] }
  end

  def document(*results, tool: 'Scorecard')
    { 'version' => '2.1.0', 'runs' => [{ 'tool' => { 'driver' => { 'name' => tool } }, 'results' => results }] }
  end

  def test_only_verified_action_pin_findings_are_removed
    other = [finding(rule: 'TokenPermissionsID'), finding(message: "score is 3: container not pinned by hash\n"),
             finding(line: 5), finding(path: '../outside.yml'), finding(path: '.github/workflows/missing.yml')]
    result, audit = ScorecardActionsLock.reconcile(document(finding, finding, *other), @root)
    assert_equal other, result['runs'][0]['results']
    assert_equal 2, audit.length
    assert_equal 1, File.readlines(File.join(@root, 'invocation')).length
  end

  def test_missing_lock_and_foreign_tools_are_not_filtered
    File.unlink(File.join(@root, '.github/workflows/actions.lock'))
    original = document(finding)
    result, audit = ScorecardActionsLock.reconcile(original, @root)
    assert_equal original, result
    assert_empty audit
    _, audit = ScorecardActionsLock.reconcile(document(finding, tool: 'CodeQL'), @root)
    assert_empty audit
    refute File.exist?(File.join(@root, 'invocation'))
  end

  # A verifier result that is malformed, invalid or paired with a non-zero exit
  # never removes a finding: the finding is kept, no audit record is written,
  # and the workflow is reported as a failure.
  def test_invalid_verification_never_becomes_a_clean_result
    ['{}', '{"valid":false,"findings":[]}', '{"valid":true}', '[]', 'not JSON'].each do |invalid|
      ENV['TEST_LOCK_JSON'] = invalid
      assert_verification_failed(document(finding), '.github/workflows/ci.yml')
    end
    ENV['TEST_LOCK_JSON'] = '{"valid":true,"findings":[]}'
    ENV['TEST_LOCK_EXIT'] = '1'
    assert_verification_failed(document(finding), '.github/workflows/ci.yml')
  end

  # Assert that reconciling +doc+ removes nothing, records no audit entry and
  # reports +relative+ as a failed workflow.
  def assert_verification_failed(doc, relative)
    original = JSON.parse(JSON.generate(doc))
    result, audit, failures = ScorecardActionsLock.reconcile(doc, @root)
    assert_equal original, result
    assert_empty audit
    assert_equal [relative], failures.keys
  end

  def test_workflow_symlinks_are_not_filtered
    File.rename(File.join(@root, '.github/workflows/ci.yml'), File.join(@root, 'actual.yml'))
    File.symlink('../../actual.yml', File.join(@root, '.github/workflows/ci.yml'))
    _, audit = ScorecardActionsLock.reconcile(document(finding), @root)
    assert_empty audit
    refute File.exist?(File.join(@root, 'invocation'))
  end

  def test_mixed_workflow_with_job_level_reusable_ref_is_not_rejected_as_stale
    slsa_sha = 'f7dd8c54c2067bafc12ca7a55595d5ee9b75204a'
    checkout_sha = '3d3c42e5aac5ba805825da76410c181273ba90b1'
    File.write(File.join(@root, '.github/workflows/release.yml'), <<~YAML)
      name: Release
      on: [push]
      jobs:
        build:
          runs-on: ubuntu-latest
          steps:
            - uses: actions/checkout@#{checkout_sha}
        provenance:
          uses: slsa-framework/slsa-github-generator/.github/workflows/generator_generic_slsa3.yml@#{slsa_sha} # v2.1.0
    YAML
    File.write(File.join(@root, '.github/workflows/actions.lock'), <<~YAML)
      version: 'v0.0.2'
      workflows:
        '.github/workflows/release.yml':
          - 'actions/checkout@#{checkout_sha}'
          - 'slsa-framework/slsa-github-generator@#{slsa_sha}'
      dependencies:
        'actions/checkout@#{checkout_sha}':
          ref: 'v7.0.1'
          commit: 'sha1-#{checkout_sha}'
        'slsa-framework/slsa-github-generator@#{slsa_sha}':
          ref: 'v2.1.0'
          commit: 'sha1-#{slsa_sha}'
    YAML
    ENV['TEST_LOCK_JSON'] = JSON.generate(
      'valid' => false,
      'findings' => [
        {
          'category' => 'stale',
          'severity' => 'warning',
          'workflow' => '.github/workflows/release.yml',
          'dependency' => "slsa-framework/slsa-github-generator@#{slsa_sha}"
        }
      ]
    )
    ENV['TEST_LOCK_EXIT'] = '1'

    result, audit = ScorecardActionsLock.reconcile(
      document(finding(path: '.github/workflows/release.yml', line: 7)),
      @root
    )
    assert_empty result['runs'][0]['results']
    assert_equal 1, audit.length
  end

  # Arm D of standards#1036: a job-level reusable ref absent from actions.lock
  # is a verification failure even though gh actions-lock passes it vacuously;
  # the finding is kept and the workflow reported, never silently dropped.
  def test_arm_d_job_level_reusable_ref_absent_from_lock_is_kept_and_reported
    missing_sha = 'deadbeefdeadbeefdeadbeefdeadbeefdeadbeef'
    File.write(File.join(@root, '.github/workflows/scorecard.yml'), <<~YAML)
      name: Scorecard
      on: [push]
      jobs:
        analysis:
          uses: hyperpolymath/standards/.github/workflows/scorecard-reusable.yml@#{missing_sha}
    YAML
    File.write(File.join(@root, '.github/workflows/actions.lock'), <<~YAML)
      version: 'v0.0.2'
      workflows:
        '.github/workflows/scorecard.yml': []
      dependencies: {}
    YAML
    ENV['TEST_LOCK_JSON'] = '{"valid":true,"findings":[]}'
    ENV['TEST_LOCK_EXIT'] = '0'

    assert_verification_failed(document(finding(path: '.github/workflows/scorecard.yml', line: 5)),
                               '.github/workflows/scorecard.yml')
  end
  # Write a second workflow and a per-file verifier response that FAILS for it
  # (a real lock desync, as on metadatastician/ZenodoDeposits.jl quality.yml).
  def add_failing_workflow(name = 'quality.yml')
    File.write(File.join(@root, ".github/workflows/#{name}"),
               "jobs:\n  lint:\n    steps:\n      - uses: actions/checkout@v7.0.0\n")
    FileUtils.mkdir_p(File.join(@root, 'responses'))
    File.write(File.join(@root, "responses/#{name}.json"), JSON.generate(
      'valid' => false,
      'findings' => [{ 'category' => 'ref-changed', 'severity' => 'error',
                       'dependency' => 'actions/checkout@v7.0.0' }]
    ))
    File.write(File.join(@root, "responses/#{name}.exit"), '1')
    ".github/workflows/#{name}"
  end

  # The regression for the whole-document abort: one workflow failing native
  # verification must not stop reconciliation of any other workflow. The
  # failing file comes FIRST so an abort (or a "stop filtering after the first
  # failure" variant) leaves the verified file's findings in place.
  def test_one_failing_workflow_does_not_unreconcile_the_others
    bad = add_failing_workflow
    bad_findings = [finding(path: bad, line: 4), finding(path: bad, line: 4)]
    good_findings = [finding, finding]
    result, audit, failures = ScorecardActionsLock.reconcile(document(*bad_findings, *good_findings), @root)

    assert_equal bad_findings, result['runs'][0]['results'], 'failing workflow keeps every finding; verified one loses its'
    assert_equal ['.github/workflows/ci.yml'] * 2, audit.map { |a| a['file'] }
    assert_equal [bad], failures.keys
    assert_match(/Native action-lock verification failed for #{Regexp.escape(bad)}/, failures[bad])
    invocations = File.readlines(File.join(@root, 'invocation'))
    assert_equal 2, invocations.length, 'gh runs once per workflow, failures included'
  end

  # A workflow whose YAML cannot be parsed is a failure of that workflow only.
  def test_unparseable_workflow_is_confined_to_itself
    File.write(File.join(@root, '.github/workflows/broken.yml'), "jobs: [unclosed\n")
    broken = finding(path: '.github/workflows/broken.yml', line: 1)
    result, audit, failures = ScorecardActionsLock.reconcile(document(broken, finding), @root)
    assert_equal [broken], result['runs'][0]['results']
    assert_equal 1, audit.length
    assert_equal ['.github/workflows/broken.yml'], failures.keys
  end

  # Run the script as scorecard-reusable.yml does and return [status, stderr].
  def run_cli(input_doc, out, audit)
    input = File.join(@root, 'results.sarif')
    File.write(input, input_doc.is_a?(String) ? input_doc : JSON.generate(input_doc))
    script = File.expand_path('../reconcile-scorecard-actions-lock.rb', __dir__)
    _, stderr, status = Open3.capture3(RbConfig.ruby, script, input, out, audit, @root)
    [status, stderr]
  end

  # Workflow contract: on a partial failure the reconciled SARIF IS written (so
  # the select step uploads it, not the raw SARIF) and the exit is non-zero (so
  # the run still fails by design).
  def test_cli_partial_failure_writes_output_and_exits_nonzero
    bad = add_failing_workflow
    out = File.join(@root, 'out.sarif')
    audit = File.join(@root, 'audit.json')
    status, stderr = run_cli(document(finding(path: bad, line: 4), finding), out, audit)
    assert_equal 3, status.exitstatus
    assert File.size?(out), 'reconciled SARIF must be written on a partial failure'
    kept = JSON.parse(File.read(out))['runs'][0]['results']
    assert_equal [bad], kept.map { |r| r.dig('locations', 0, 'physicalLocation', 'artifactLocation', 'uri') }
    assert_equal 1, JSON.parse(File.read(audit)).length
    assert_match(/kept findings for #{Regexp.escape(bad)}/, stderr)
  end

  # Workflow contract: success exits 0; a structural error exits 2 and writes
  # no output, so the workflow falls back to the raw SARIF and fails.
  def test_cli_success_and_structural_error_exit_codes
    out = File.join(@root, 'out.sarif')
    audit = File.join(@root, 'audit.json')
    status, = run_cli(document(finding), out, audit)
    assert_equal 0, status.exitstatus
    assert_empty JSON.parse(File.read(out))['runs'][0]['results']

    File.unlink(out)
    status, stderr = run_cli('{"version":"2.0.0","runs":[]}', out, audit)
    assert_equal 2, status.exitstatus
    refute File.exist?(out), 'no reconciled SARIF on a structural error'
    assert_match(/Scorecard reconciliation failed/, stderr)
  end
end
