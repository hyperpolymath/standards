# frozen_string_literal: true

module Palimpsest
  # Configuration class for Palimpsest License
  class Config
    attr_accessor :work_title, :author_name, :author_url, :emotional_lineage,
                  :version, :license_url, :agi_consent_required,
                  :inject_headers, :inject_html_meta, :inject_jsonld, :language

    # Initialize configuration with options
    #
    # @param options [Hash] Configuration options
    def initialize(options = {})
      @work_title = options[:work_title]
      @author_name = options[:author_name]
      @author_url = options[:author_url]
      @emotional_lineage = options[:emotional_lineage]
      @version = options[:version] || '0.4'
      @license_url = options[:license_url] || "https://palimpsestlicense.org/v#{@version}"
      @agi_consent_required = options.fetch(:agi_consent_required, true)
      @inject_headers = options.fetch(:inject_headers, true)
      @inject_html_meta = options.fetch(:inject_html_meta, true)
      @inject_jsonld = options.fetch(:inject_jsonld, true)
      @language = options[:language] || 'en'
    end

    # Convert configuration to hash
    #
    # @return [Hash] Configuration as hash
    def to_h
      {
        work_title: @work_title,
        author_name: @author_name,
        author_url: @author_url,
        emotional_lineage: @emotional_lineage,
        version: @version,
        license_url: @license_url,
        agi_consent_required: @agi_consent_required,
        inject_headers: @inject_headers,
        inject_html_meta: @inject_html_meta,
        inject_jsonld: @inject_jsonld,
        language: @language
      }
    end
  end
end
