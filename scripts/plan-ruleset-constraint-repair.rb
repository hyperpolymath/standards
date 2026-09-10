#!/usr/bin/env ruby
# SPDX-License-Identifier: MPL-2.0
# Produce a reviewable PUT body for the four retired constraints documented
# in config/README.adoc. This script never calls GitHub or changes a ruleset.
require 'json'

module RulesetConstraintRepair
  RETIRED = %w[update required_deployments code_quality code_coverage].freeze
  WRITABLE = %w[name target enforcement conditions bypass_actors rules].freeze

  # Build a new ruleset update body with the four retired constraints removed,
  # preserving the other writable fields and leaving +source+ unchanged.
  #
  # When +uninstalled_app_id+ is supplied, also remove only that integration's
  # bypass. Supply an ID only after GitHub rejects it as no longer installed;
  # never infer app availability from its name.
  #
  # Raises ArgumentError unless the source is an active default-branch ruleset
  # with pull-request protection, or if the requested integration ID is invalid
  # or absent. A supplied ID also requires +source+ to contain +bypass_actors+.
  def self.plan(source, uninstalled_app_id: nil)
    unless source.is_a?(Hash) && source['target'] == 'branch' && source['enforcement'] == 'active' &&
           source.dig('conditions', 'ref_name', 'include') == ['~DEFAULT_BRANCH'] &&
           source.dig('conditions', 'ref_name', 'exclude') == [] && source['rules'].is_a?(Array)
      raise ArgumentError, 'Expected an active default-branch ruleset with no exclusions'
    end
    rules = source.fetch('rules')
    unless rules.all? { |rule| rule.is_a?(Hash) && rule['type'].is_a?(String) }
      raise ArgumentError, 'Malformed rule data; refusing a partial plan'
    end
    raise ArgumentError, 'Missing pull-request protection' unless rules.any? { |r| r['type'] == 'pull_request' }

    result = source.select { |key, _value| WRITABLE.include?(key) }
    result['rules'] = rules.reject { |rule| RETIRED.include?(rule['type']) }
    if uninstalled_app_id
      actors = source.fetch('bypass_actors')
      unless uninstalled_app_id.is_a?(Integer) && uninstalled_app_id.positive? &&
             actors.is_a?(Array) && actors.any? { |actor| actor['actor_type'] == 'Integration' && actor['actor_id'] == uninstalled_app_id }
        raise ArgumentError, 'Expected the exact ID of an existing integration bypass rejected by GitHub'
      end
      result['bypass_actors'] = actors.reject { |actor| actor['actor_type'] == 'Integration' && actor['actor_id'] == uninstalled_app_id }
    end
    result
  end
end

if $PROGRAM_NAME == __FILE__
  unless ARGV.length == 1 || (ARGV.length == 3 && ARGV[1] == '--remove-uninstalled-app')
    abort 'Usage: plan-ruleset-constraint-repair.rb <live-ruleset.json> [--remove-uninstalled-app <verified-id>]'
  end
  app_id = ARGV[2] && Integer(ARGV[2], 10)
  puts JSON.pretty_generate(RulesetConstraintRepair.plan(JSON.parse(File.read(ARGV[0])), uninstalled_app_id: app_id))
end
