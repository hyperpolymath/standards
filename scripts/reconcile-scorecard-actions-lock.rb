#!/usr/bin/env ruby
# SPDX-License-Identifier: MPL-2.0
# Reconcile only Scorecard's inline action-pin findings with native lock coverage.
# No lock semantics are reimplemented: gh actions-lock verifies each workflow.
# Usage: ruby reconcile-scorecard-actions-lock.rb INPUT OUTPUT AUDIT_JSON [ROOT]
# Exit codes:
#   0  every eligible workflow verified; OUTPUT and AUDIT_JSON written.
#   3  OUTPUT and AUDIT_JSON written, but at least one workflow failed native
#      verification. Only that workflow's findings are retained; findings in
#      verified workflows are still removed. Callers must treat this as failure.
#   2  usage or SARIF structure error; OUTPUT is NOT written.
require 'json'
require 'open3'
require 'yaml'

module ScorecardActionsLock
  PIN_MESSAGE = /\Ascore is \d+: (?:GitHub-owned |third-party )?GitHubAction not pinned by hash\n/
  REUSABLE_REF = %r{\A([A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+)/\.github/workflows/[^@\s]+\.ya?ml@([^\s#]+)\z}
  SHA_COMMIT = /\Asha1-([0-9a-f]{40})\z/i
  HEX_SHA = /\A[0-9a-f]{40}\z/i

  # Return the one-based line numbers of remote GitHub Action +uses+ entries in
  # the workflow. Local actions, containers and text containing +uses+ are
  # excluded.
  def self.action_lines(path)
    lines = []
    visit = lambda do |node|
      if node.is_a?(Psych::Nodes::Mapping)
        node.children.each_slice(2) do |key, value|
          if key.is_a?(Psych::Nodes::Scalar) && key.value == 'uses' && value.is_a?(Psych::Nodes::Scalar)
            # Only remote GitHub actions, never containers or local paths.
            lines << key.start_line + 1 if value.value.match?(%r{\A[A-Za-z0-9_.-]+/[A-Za-z0-9_./-]+@[^\s]+\z})
          end
        end
      end
      Array(node.children).each { |child| visit.call(child) } if node.respond_to?(:children)
    end
    visit.call(Psych.parse_stream(File.read(path)))
    lines
  end

  # Return normalised `owner/repo@ref` strings for every remote reusable-workflow
  # `uses: owner/repo/.github/workflows/<file>.y(a)ml@<ref>` in +path+.
  # `gh actions-lock --verify` (v0.1.6) ignores job-level `uses:` in both
  # directions (standards#1036): it falsely flags present lock entries as
  # `stale`, and vacuously passes workflows whose job-level ref is absent from
  # `actions.lock` (Arm D).
  def self.reusable_workflow_deps(path)
    deps = []
    visit = lambda do |node|
      if node.is_a?(Psych::Nodes::Mapping)
        node.children.each_slice(2) do |key, value|
          if key.is_a?(Psych::Nodes::Scalar) && key.value == 'uses' && value.is_a?(Psych::Nodes::Scalar)
            match = value.value.strip.match(REUSABLE_REF)
            deps << "#{match[1]}@#{match[2]}" if match
          end
        end
      end
      Array(node.children).each { |child| visit.call(child) } if node.respond_to?(:children)
    end
    visit.call(Psych.parse_stream(File.read(path)))
    deps.uniq
  end

  # Verify that every `owner/repo@ref` in +reusable_deps+ is recorded under
  # `workflows[relative]` and `dependencies` in `actions.lock` with a valid
  # immutable commit hash (and matching SHA when +ref+ is a 40-hex SHA).
  def self.lock_covers_reusable_deps?(lock_path, relative, reusable_deps)
    return true if reusable_deps.empty?

    lock = YAML.safe_load(File.read(lock_path))
    return false unless lock.is_a?(Hash)

    wf_entries = lock.dig('workflows', relative)
    deps_map = lock['dependencies']
    return false unless wf_entries.is_a?(Array) && deps_map.is_a?(Hash)

    reusable_deps.all? do |dep|
      next false unless wf_entries.include?(dep)
      entry = deps_map[dep]
      next false unless entry.is_a?(Hash)
      commit_match = entry['commit'].to_s.match(SHA_COMMIT)
      next false unless commit_match
      ref = dep.split('@', 2).last
      !ref.match?(HEX_SHA) || commit_match[1].casecmp?(ref)
    end
  end

  # Decide whether one workflow's `gh actions-lock --verify` result is clean
  # enough to suppress its Scorecard pin findings. Accepts a valid result whose
  # findings are empty (with a zero exit) or only `sha-as-ref`, and an invalid
  # result whose only non-`sha-as-ref` findings are `stale` entries for
  # job-level reusable refs the verifier cannot see (standards#1036).
  def self.verification_accepted?(verification, status, reusable_deps)
    return false unless verification.is_a?(Hash) && verification['findings'].is_a?(Array)
    findings = verification['findings']

    if verification['valid'] == true
      return status.success? if findings.empty?
      return findings.all? { |f| f.is_a?(Hash) && f['category'] == 'sha-as-ref' }
    end

    return false unless verification['valid'] == false && !findings.empty?

    accepted_stale = 0
    findings.each do |f|
      return false unless f.is_a?(Hash)
      if f['category'] == 'stale' && reusable_deps.include?(f['dependency'])
        accepted_stale += 1
      elsif f['category'] != 'sha-as-ref'
        return false
      end
    end
    accepted_stale.positive?
  end

  # Verify one workflow (+relative+ to +root+) against `actions.lock`: job-level
  # reusable refs must be recorded in the lock, and `gh actions-lock --verify`
  # must report an accepted result. Returns the verifier's JSON on success.
  # Raises with the reason on any failure, including unparseable workflow YAML,
  # a missing `gh`, or non-JSON verifier output, so the caller can confine the
  # failure to this one file.
  def self.verify_workflow(root, relative)
    path = File.join(root, relative)
    lock_path = File.join(root, '.github/workflows/actions.lock')
    reusable_deps = reusable_workflow_deps(path)
    unless lock_covers_reusable_deps?(lock_path, relative, reusable_deps)
      raise "Native action-lock verification failed for #{relative}: job-level reusable ref absent from actions.lock"
    end

    stdout, stderr, status = Open3.capture3('gh', 'actions-lock', relative,
      '--verify', '--no-interactive', '--json=valid,findings', chdir: root)
    warn stderr unless stderr.empty?
    verification = JSON.parse(stdout)
    unless verification_accepted?(verification, status, reusable_deps)
      raise "Native action-lock verification failed for #{relative}"
    end
    verification
  end

  # Remove Scorecard action-pin findings only when they identify a remote action
  # entry in a regular workflow below +root+ and native action-lock verification
  # of THAT workflow succeeds. Verification is per file: a workflow that fails
  # keeps all of its findings and is recorded in the returned failures map
  # (relative path => reason), while findings in other, verified workflows are
  # still removed. The supplied SARIF document is updated in place and returned
  # as [document, audit, failures], where audit lists the removed findings.
  #
  # Invokes +gh actions-lock+ at most once per eligible workflow. Raises only
  # when the SARIF structure itself is invalid.
  def self.reconcile(document, root)
    raise 'Expected a SARIF 2.1.0 document with runs' unless document.is_a?(Hash) &&
      document['version'] == '2.1.0' && document['runs'].is_a?(Array) && !document['runs'].empty?
    document['runs'].each do |run|
      raise 'Expected a results array' unless run.is_a?(Hash) && run['results'].is_a?(Array)
    end

    root = File.realpath(root)
    verified = {}
    failures = {}
    audit = []
    document['runs'].each do |run|
      next unless run.dig('tool', 'driver', 'name') == 'Scorecard'

      run['results'] = run['results'].reject do |result|
        next false unless result['ruleId'] == 'PinnedDependenciesID' &&
          result.dig('message', 'text').to_s.match?(PIN_MESSAGE)
        locations = result['locations']
        next false unless locations.is_a?(Array) && locations.length == 1
        location = locations[0]['physicalLocation'] || {}
        relative = location.dig('artifactLocation', 'uri')
        line = location.dig('region', 'startLine')
        next false unless relative.is_a?(String) && relative.match?(%r{\A\.github/workflows/[^/]+\.ya?ml\z}) &&
          line.is_a?(Integer) && line.positive?

        path = File.join(root, relative)
        next false unless File.file?(path) && !File.symlink?(path) &&
          File.realpath(path).start_with?(root + '/') && File.file?(File.join(root, '.github/workflows/actions.lock'))
        # A failure in one workflow is confined to that workflow: its findings
        # are kept and every other workflow is still reconciled on its own.
        next false if failures.key?(relative)

        begin
          lines = action_lines(path)
        rescue StandardError => error
          failures[relative] = "Unparseable workflow #{relative}: #{error.message}"
          next false
        end
        next false unless lines.include?(line)

        unless verified.key?(relative)
          begin
            verified[relative] = verify_workflow(root, relative)
          rescue StandardError => error
            failures[relative] = error.message
            next false
          end
        end
        audit << { 'file' => relative, 'line' => line, 'rule' => result['ruleId'],
          'reason' => 'False positive: native direct and transitive pins verified by gh actions-lock --verify',
          'verification' => verified.fetch(relative) }
        true
      end
    end
    [document, audit, failures]
  end
end

if $PROGRAM_NAME == __FILE__
  begin
    input, output, audit_path, root = ARGV
    raise 'Usage: INPUT OUTPUT AUDIT_JSON [ROOT]' unless input && output && audit_path && ARGV.length <= 4
    raise 'Keep the original SARIF as a separate artifact' if File.expand_path(input) == File.expand_path(output)
    document, audit, failures = ScorecardActionsLock.reconcile(JSON.parse(File.read(input)), root || Dir.pwd)
    File.write(output, JSON.pretty_generate(document) + "\n")
    File.write(audit_path, JSON.pretty_generate(audit) + "\n")
    puts "Reconciled #{audit.length} verified native action-pin false positives; all other findings retained."
    unless failures.empty?
      failures.each { |relative, reason| warn "Scorecard reconciliation kept findings for #{relative}: #{reason}" }
      warn "Scorecard reconciliation incomplete: #{failures.length} workflow(s) failed native action-lock " \
           'verification; their findings are retained in the reconciled SARIF.'
      exit 3
    end
  rescue StandardError => error
    warn "Scorecard reconciliation failed: #{error.message}"
    exit 2
  end
end
