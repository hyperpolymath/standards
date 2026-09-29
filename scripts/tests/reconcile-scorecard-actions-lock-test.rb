#!/usr/bin/env ruby
# SPDX-License-Identifier: MPL-2.0
# Prove native-lock reconciliation is narrow and verifier failures stay failures.
require 'tmpdir'
require 'fileutils'
require 'minitest/autorun'
require_relative '../reconcile-scorecard-actions-lock'

class ScorecardActionsLockTest < Minitest::Test
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

  def test_invalid_verification_never_becomes_a_clean_result
    ['{}', '{"valid":false,"findings":[]}', '{"valid":true}', '[]', 'not JSON'].each do |invalid|
      ENV['TEST_LOCK_JSON'] = invalid
      assert_raises(StandardError) { ScorecardActionsLock.reconcile(document(finding), @root) }
    end
    ENV['TEST_LOCK_JSON'] = '{"valid":true,"findings":[]}'
    ENV['TEST_LOCK_EXIT'] = '1'
    assert_raises(StandardError) { ScorecardActionsLock.reconcile(document(finding), @root) }
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

  def test_arm_d_job_level_reusable_ref_absent_from_lock_raises
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

    assert_raises(StandardError) do
      ScorecardActionsLock.reconcile(
        document(finding(path: '.github/workflows/scorecard.yml', line: 5)),
        @root
      )
    end
  end
end
