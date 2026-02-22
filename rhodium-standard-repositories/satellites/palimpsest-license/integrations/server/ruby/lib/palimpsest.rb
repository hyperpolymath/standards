# frozen_string_literal: true

require_relative 'palimpsest/version'
require_relative 'palimpsest/config'
require_relative 'palimpsest/middleware'
require_relative 'palimpsest/generator'

# Palimpsest License integration for Ruby/Rails
#
# This module provides utilities for integrating the Palimpsest License
# into Ruby web applications, with first-class support for Rails.
#
# @example Basic usage with Rack
#   use Palimpsest::Middleware, {
#     work_title: 'My Work',
#     author_name: 'Jane Doe'
#   }
#
# @example Rails configuration
#   # config/application.rb
#   config.middleware.use Palimpsest::Middleware, {
#     work_title: 'My Rails App',
#     author_name: 'Jane Doe',
#     emotional_lineage: 'A story of resilience'
#   }
module Palimpsest
  class Error < StandardError; end

  # Generate JSON-LD metadata
  #
  # @param config [Hash] Configuration options
  # @return [String] JSON-LD metadata
  def self.generate_jsonld(config)
    Generator.new(Config.new(config)).generate_jsonld
  end

  # Generate HTML meta tags
  #
  # @param config [Hash] Configuration options
  # @return [String] HTML meta tags
  def self.generate_html_meta(config)
    Generator.new(Config.new(config)).generate_html_meta
  end

  # Generate license widget
  #
  # @param config [Hash] Configuration options
  # @param theme [String] Theme ('light' or 'dark')
  # @return [String] HTML widget
  def self.generate_widget(config, theme: 'light')
    Generator.new(Config.new(config)).generate_widget(theme)
  end

  # Validate metadata
  #
  # @param metadata [Hash] Metadata to validate
  # @return [Hash] Validation result with :valid and :errors keys
  def self.validate_metadata(metadata)
    errors = []

    errors << 'Missing required field: license' unless metadata[:license] || metadata['license']

    author = metadata[:author] || metadata['author']
    author_name = author&.dig(:name) || author&.dig('name')
    errors << 'Missing required field: author.name' unless author_name

    type = metadata[:@type] || metadata['@type']
    errors << 'Invalid or missing @type field (must be "CreativeWork")' unless type == 'CreativeWork'

    license = metadata[:license] || metadata['license']
    if license && !license.to_s.downcase.include?('palimpsest')
      errors << 'License URL does not reference Palimpsest License'
    end

    { valid: errors.empty?, errors: errors }
  end
end
