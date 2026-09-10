#!/usr/bin/env ruby
# SPDX-License-Identifier: MPL-2.0
# Reconcile only Scorecard's inline action-pin findings with native lock coverage.
# No lock semantics are reimplemented: gh actions-lock verifies each workflow.
# Usage: ruby reconcile-scorecard-actions-lock.rb INPUT OUTPUT AUDIT_JSON [ROOT]
require 'json'
require 'open3'
require 'yaml'

module ScorecardActionsLock
  PIN_MESSAGE = /\Ascore is \d+: (?:GitHub-owned |third-party )?GitHubAction not pinned by hash\n/

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

  def self.reconcile(document, root)
    raise 'Expected a SARIF 2.1.0 document with runs' unless document.is_a?(Hash) &&
      document['version'] == '2.1.0' && document['runs'].is_a?(Array) && !document['runs'].empty?

    root = File.realpath(root)
    verified = {}
    audit = []
    document['runs'].each do |run|
      raise 'Expected a results array' unless run['results'].is_a?(Array)
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
        next false unless action_lines(path).include?(line)

        unless verified.key?(relative)
          stdout, stderr, status = Open3.capture3('gh', 'actions-lock', relative,
            '--verify', '--no-interactive', '--json=valid,findings', chdir: root)
          warn stderr unless stderr.empty?
          verification = JSON.parse(stdout)
          raise "Native action-lock verification failed for #{relative}" unless status.success? &&
            verification.is_a?(Hash) && verification['valid'] == true && verification['findings'].is_a?(Array)
          verified[relative] = verification
        end
        audit << { 'file' => relative, 'line' => line, 'rule' => result['ruleId'],
          'reason' => 'False positive: native direct and transitive pins verified by gh actions-lock --verify',
          'verification' => verified.fetch(relative) }
        true
      end
    end
    [document, audit]
  end
end

if $PROGRAM_NAME == __FILE__
  begin
    input, output, audit_path, root = ARGV
    raise 'Usage: INPUT OUTPUT AUDIT_JSON [ROOT]' unless input && output && audit_path && ARGV.length <= 4
    raise 'Keep the original SARIF as a separate artifact' if File.expand_path(input) == File.expand_path(output)
    document, audit = ScorecardActionsLock.reconcile(JSON.parse(File.read(input)), root || Dir.pwd)
    File.write(output, JSON.pretty_generate(document) + "\n")
    File.write(audit_path, JSON.pretty_generate(audit) + "\n")
    puts "Reconciled #{audit.length} verified native action-pin false positives; all other findings retained."
  rescue StandardError => error
    warn "Scorecard reconciliation failed: #{error.message}"
    exit 2
  end
end
