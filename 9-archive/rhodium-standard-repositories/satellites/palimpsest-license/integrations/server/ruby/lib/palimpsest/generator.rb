# frozen_string_literal: true

require 'json'

module Palimpsest
  # Generator for Palimpsest License metadata
  class Generator
    # @param config [Config] Configuration object
    def initialize(config)
      @config = config
    end

    # Generate JSON-LD metadata
    #
    # @return [String] JSON-LD string
    def generate_jsonld
      metadata = {
        '@context': 'https://schema.org',
        '@type': 'CreativeWork',
        name: @config.work_title || '[Work Title]',
        author: {
          '@type': 'Person',
          name: @config.author_name || '[Author Name]'
        },
        license: @config.license_url,
        usageInfo: 'https://palimpsestlicense.org',
        additionalProperty: [
          {
            '@type': 'PropertyValue',
            name: 'Palimpsest:Version',
            value: @config.version
          },
          {
            '@type': 'PropertyValue',
            name: 'Palimpsest:AGIConsent',
            value: @config.agi_consent_required ?
              'Explicit consent required for AI training. See license for details.' :
              'Consent granted with attribution. See license for details.'
          },
          {
            '@type': 'PropertyValue',
            name: 'Palimpsest:MetadataPreservation',
            value: 'Mandatory. Removal or modification constitutes license breach.'
          }
        ]
      }

      metadata[:author][:url] = @config.author_url if @config.author_url

      if @config.emotional_lineage
        metadata[:additionalProperty].insert(1, {
          '@type': 'PropertyValue',
          name: 'Palimpsest:EmotionalLineage',
          value: @config.emotional_lineage
        })
      end

      JSON.pretty_generate(metadata)
    end

    # Generate HTML meta tags
    #
    # @return [String] HTML meta tags
    def generate_html_meta
      license_description = @config.language == 'nl' ?
        "Dit werk is beschermd onder de Palimpsest Licentie v#{@config.version}" :
        "This work is protected under the Palimpsest License v#{@config.version}"

      tags = [
        %(<meta name="license" content="#{html_escape(@config.license_url)}">),
        %(<meta name="license-type" content="Palimpsest-#{html_escape(@config.version)}">),
        %(<meta property="og:license" content="#{html_escape(@config.license_url)}">),
        %(<meta name="dcterms.license" content="#{html_escape(@config.license_url)}">),
        %(<meta name="dcterms.rights" content="#{html_escape(license_description)}">),
        %(<meta name="palimpsest:version" content="#{html_escape(@config.version)}">),
        %(<meta name="palimpsest:agi-consent" content="#{@config.agi_consent_required}">)
      ]

      if @config.author_name
        tags << %(<meta name="author" content="#{html_escape(@config.author_name)}">)
        tags << %(<meta name="dcterms.creator" content="#{html_escape(@config.author_name)}">)
      end

      if @config.emotional_lineage
        tags << %(<meta name="palimpsest:emotional-lineage" content="#{html_escape(@config.emotional_lineage)}">)
      end

      tags.join("\n    ")
    end

    # Generate license widget HTML
    #
    # @param theme [String] Theme ('light' or 'dark')
    # @return [String] HTML widget
    def generate_widget(theme = 'light')
      text = @config.language == 'nl' ? {
        main: "Dit werk is beschermd onder de <strong>Palimpsest Licentie v#{@config.version}</strong>.",
        requirement: 'Afgeleiden moeten de emotionele en culturele integriteit van het origineel behouden.',
        link: 'Lees de volledige licentie'
      } : {
        main: "This work is protected under the <strong>Palimpsest License v#{@config.version}</strong>.",
        requirement: "Derivatives must preserve the original's emotional and cultural integrity.",
        link: 'Read the full license'
      }

      styles = theme == 'dark' ? {
        border_color: '#30363d',
        font_color: '#c9d1d9',
        link_color: '#58a6ff',
        bg_color: '#0d1117'
      } : {
        border_color: '#e1e4e8',
        font_color: '#24292e',
        link_color: '#0366d6',
        bg_color: '#f6f8fa'
      }

      <<~HTML
        <div class="palimpsest-notice" style="border: 1px solid #{styles[:border_color]}; border-radius: 6px; padding: 16px; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; font-size: 14px; line-height: 1.5; color: #{styles[:font_color]}; background-color: #{styles[:bg_color]}; margin: 20px 0;">
          <div style="display: flex; align-items: center;">
            <svg style="width: 24px; height: 24px; margin-right: 12px; flex-shrink: 0;" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
              <path d="M12 2L2 7L12 12L22 7L12 2Z" stroke="#{styles[:link_color]}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
              <path d="M2 17L12 22L22 17" stroke="#{styles[:link_color]}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
              <path d="M2 12L12 17L22 12" stroke="#{styles[:link_color]}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            </svg>
            <div>
              <p style="margin: 0 0 8px 0;">#{text[:main]}</p>
              <p style="margin: 0 0 8px 0; font-size: 13px; opacity: 0.8;">#{text[:requirement]}</p>
              <a href="#{@config.license_url}" style="color: #{styles[:link_color]}; text-decoration: none; font-weight: 500;">#{text[:link]} →</a>
            </div>
          </div>
        </div>
      HTML
    end

    # Get HTTP headers
    #
    # @return [Hash] HTTP headers
    def http_headers
      headers = {
        'X-License' => "Palimpsest-#{@config.version}",
        'X-License-Url' => @config.license_url,
        'X-Palimpsest-Version' => @config.version,
        'X-Palimpsest-AGI-Consent' => @config.agi_consent_required.to_s,
        'Link' => "<#{@config.license_url}>; rel=\"license\""
      }

      headers['X-Author'] = @config.author_name if @config.author_name

      headers
    end

    private

    # Simple HTML escape
    def html_escape(str)
      str.to_s.gsub('&', '&amp;')
             .gsub('<', '&lt;')
             .gsub('>', '&gt;')
             .gsub('"', '&quot;')
             .gsub("'", '&#39;')
    end
  end
end
