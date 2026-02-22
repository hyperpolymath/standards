<?php

namespace Palimpsest;

/**
 * Main Palimpsest License class for PHP integration
 *
 * Provides utilities for generating and injecting Palimpsest License
 * metadata into PHP applications.
 */
class PalimpsestLicense
{
    private PalimpsestConfig $config;

    /**
     * Create a new Palimpsest License instance
     *
     * @param PalimpsestConfig $config Configuration object
     */
    public function __construct(PalimpsestConfig $config)
    {
        $this->config = $config;
    }

    /**
     * Generate JSON-LD metadata
     *
     * @return string JSON-LD metadata as a string
     */
    public function generateJsonLd(): string
    {
        $metadata = [
            '@context' => 'https://schema.org',
            '@type' => 'CreativeWork',
            'name' => $this->config->workTitle ?? '[Work Title]',
            'author' => [
                '@type' => 'Person',
                'name' => $this->config->authorName ?? '[Author Name]',
            ],
            'license' => $this->config->licenseUrl,
            'usageInfo' => 'https://palimpsestlicense.org',
            'additionalProperty' => [
                [
                    '@type' => 'PropertyValue',
                    'name' => 'Palimpsest:Version',
                    'value' => $this->config->version,
                ],
                [
                    '@type' => 'PropertyValue',
                    'name' => 'Palimpsest:AGIConsent',
                    'value' => $this->config->agiConsentRequired
                        ? 'Explicit consent required for AI training. See license for details.'
                        : 'Consent granted with attribution. See license for details.',
                ],
                [
                    '@type' => 'PropertyValue',
                    'name' => 'Palimpsest:MetadataPreservation',
                    'value' => 'Mandatory. Removal or modification constitutes license breach.',
                ],
            ],
        ];

        if ($this->config->authorUrl) {
            $metadata['author']['url'] = $this->config->authorUrl;
        }

        if ($this->config->emotionalLineage) {
            array_splice($metadata['additionalProperty'], 1, 0, [[
                '@type' => 'PropertyValue',
                'name' => 'Palimpsest:EmotionalLineage',
                'value' => $this->config->emotionalLineage,
            ]]);
        }

        return json_encode($metadata, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE);
    }

    /**
     * Generate HTML meta tags
     *
     * @return string HTML meta tags
     */
    public function generateHtmlMeta(): string
    {
        $licenseDescription = $this->config->language === 'nl'
            ? "Dit werk is beschermd onder de Palimpsest Licentie v{$this->config->version}"
            : "This work is protected under the Palimpsest License v{$this->config->version}";

        $tags = [
            '<meta name="license" content="' . htmlspecialchars($this->config->licenseUrl) . '">',
            '<meta name="license-type" content="Palimpsest-' . htmlspecialchars($this->config->version) . '">',
            '<meta property="og:license" content="' . htmlspecialchars($this->config->licenseUrl) . '">',
            '<meta name="dcterms.license" content="' . htmlspecialchars($this->config->licenseUrl) . '">',
            '<meta name="dcterms.rights" content="' . htmlspecialchars($licenseDescription) . '">',
            '<meta name="palimpsest:version" content="' . htmlspecialchars($this->config->version) . '">',
            '<meta name="palimpsest:agi-consent" content="' . ($this->config->agiConsentRequired ? 'true' : 'false') . '">',
        ];

        if ($this->config->authorName) {
            $tags[] = '<meta name="author" content="' . htmlspecialchars($this->config->authorName) . '">';
            $tags[] = '<meta name="dcterms.creator" content="' . htmlspecialchars($this->config->authorName) . '">';
        }

        if ($this->config->emotionalLineage) {
            $tags[] = '<meta name="palimpsest:emotional-lineage" content="' . htmlspecialchars($this->config->emotionalLineage) . '">';
        }

        return implode("\n    ", $tags);
    }

    /**
     * Get HTTP headers for license information
     *
     * @return array<string, string> Associative array of headers
     */
    public function getHttpHeaders(): array
    {
        $headers = [
            'X-License' => 'Palimpsest-' . $this->config->version,
            'X-License-Url' => $this->config->licenseUrl,
            'X-Palimpsest-Version' => $this->config->version,
            'X-Palimpsest-AGI-Consent' => $this->config->agiConsentRequired ? 'true' : 'false',
            'Link' => '<' . $this->config->licenseUrl . '>; rel="license"',
        ];

        if ($this->config->authorName) {
            $headers['X-Author'] = $this->config->authorName;
        }

        return $headers;
    }

    /**
     * Inject HTTP headers
     *
     * @return void
     */
    public function injectHeaders(): void
    {
        if (!headers_sent()) {
            foreach ($this->getHttpHeaders() as $name => $value) {
                header("$name: $value");
            }
        }
    }

    /**
     * Generate license widget HTML
     *
     * @param string $theme Theme ('light' or 'dark')
     * @return string HTML widget
     */
    public function generateLicenseWidget(string $theme = 'light'): string
    {
        $text = $this->config->language === 'nl' ? [
            'main' => "Dit werk is beschermd onder de <strong>Palimpsest Licentie v{$this->config->version}</strong>.",
            'requirement' => 'Afgeleiden moeten de emotionele en culturele integriteit van het origineel behouden.',
            'link' => 'Lees de volledige licentie',
        ] : [
            'main' => "This work is protected under the <strong>Palimpsest License v{$this->config->version}</strong>.",
            'requirement' => "Derivatives must preserve the original's emotional and cultural integrity.",
            'link' => 'Read the full license',
        ];

        $styles = $theme === 'dark' ? [
            'borderColor' => '#30363d',
            'fontColor' => '#c9d1d9',
            'linkColor' => '#58a6ff',
            'bgColor' => '#0d1117',
        ] : [
            'borderColor' => '#e1e4e8',
            'fontColor' => '#24292e',
            'linkColor' => '#0366d6',
            'bgColor' => '#f6f8fa',
        ];

        return <<<HTML
<div class="palimpsest-notice" style="border: 1px solid {$styles['borderColor']}; border-radius: 6px; padding: 16px; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; font-size: 14px; line-height: 1.5; color: {$styles['fontColor']}; background-color: {$styles['bgColor']}; margin: 20px 0;">
  <div style="display: flex; align-items: center;">
    <svg style="width: 24px; height: 24px; margin-right: 12px; flex-shrink: 0;" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
      <path d="M12 2L2 7L12 12L22 7L12 2Z" stroke="{$styles['linkColor']}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
      <path d="M2 17L12 22L22 17" stroke="{$styles['linkColor']}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
      <path d="M2 12L12 17L22 12" stroke="{$styles['linkColor']}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
    </svg>
    <div>
      <p style="margin: 0 0 8px 0;">{$text['main']}</p>
      <p style="margin: 0 0 8px 0; font-size: 13px; opacity: 0.8;">{$text['requirement']}</p>
      <a href="{$this->config->licenseUrl}" style="color: {$styles['linkColor']}; text-decoration: none; font-weight: 500;">{$text['link']} →</a>
    </div>
  </div>
</div>
HTML;
    }

    /**
     * Inject HTML content with license metadata
     *
     * @param string $html Original HTML content
     * @return string Modified HTML with license metadata
     */
    public function injectHtml(string $html): string
    {
        // Inject meta tags in <head>
        if ($this->config->injectHtmlMeta && str_contains($html, '</head>')) {
            $metaTags = $this->generateHtmlMeta();
            $html = str_replace('</head>', "    $metaTags\n  </head>", $html);
        }

        // Inject JSON-LD before </body>
        if ($this->config->injectJsonLd && str_contains($html, '</body>')) {
            $jsonLd = $this->generateJsonLd();
            $scriptTag = "\n    <script type=\"application/ld+json\">\n$jsonLd\n    </script>";
            $html = str_replace('</body>', "$scriptTag\n  </body>", $html);
        }

        return $html;
    }

    /**
     * Get license information as an array
     *
     * @return array<string, mixed>
     */
    public function getLicenseInfo(): array
    {
        return [
            'license' => [
                'name' => "Palimpsest License v{$this->config->version}",
                'version' => $this->config->version,
                'url' => $this->config->licenseUrl,
                'identifier' => "Palimpsest-{$this->config->version}",
            ],
            'work' => [
                'title' => $this->config->workTitle,
                'author' => $this->config->authorName,
                'author_url' => $this->config->authorUrl,
            ],
            'protections' => [
                'emotional_lineage' => $this->config->emotionalLineage,
                'agi_consent_required' => $this->config->agiConsentRequired,
                'metadata_preservation_required' => true,
                'quantum_proof_traceability' => true,
            ],
            'compliance' => [
                'guide_url' => 'https://palimpsestlicense.org/guides/compliance',
                'audit_template_url' => 'https://palimpsestlicense.org/toolkit/audit',
            ],
        ];
    }

    /**
     * Validate metadata array
     *
     * @param array<string, mixed> $metadata Metadata to validate
     * @return array{valid: bool, errors: array<string>}
     */
    public static function validateMetadata(array $metadata): array
    {
        $errors = [];

        if (!isset($metadata['license'])) {
            $errors[] = 'Missing required field: license';
        }

        if (!isset($metadata['author']['name'])) {
            $errors[] = 'Missing required field: author.name';
        }

        if (!isset($metadata['@type']) || $metadata['@type'] !== 'CreativeWork') {
            $errors[] = 'Invalid or missing @type field (must be "CreativeWork")';
        }

        if (isset($metadata['license']) && !str_contains(strtolower($metadata['license']), 'palimpsest')) {
            $errors[] = 'License URL does not reference Palimpsest License';
        }

        return [
            'valid' => empty($errors),
            'errors' => $errors,
        ];
    }
}
