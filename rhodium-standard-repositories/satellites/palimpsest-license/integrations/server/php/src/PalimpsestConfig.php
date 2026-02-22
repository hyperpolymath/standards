<?php

namespace Palimpsest;

/**
 * Configuration class for Palimpsest License integration
 */
class PalimpsestConfig
{
    public ?string $workTitle = null;
    public ?string $authorName = null;
    public ?string $authorUrl = null;
    public ?string $emotionalLineage = null;
    public string $version = '0.4';
    public string $licenseUrl = 'https://palimpsestlicense.org/v0.4';
    public bool $agiConsentRequired = true;
    public bool $injectHeaders = true;
    public bool $injectHtmlMeta = true;
    public bool $injectJsonLd = true;
    public string $language = 'en';

    /**
     * Create a new configuration instance
     *
     * @param array<string, mixed> $options Configuration options
     */
    public function __construct(array $options = [])
    {
        foreach ($options as $key => $value) {
            if (property_exists($this, $key)) {
                $this->$key = $value;
            }
        }
    }

    /**
     * Convert configuration to array
     *
     * @return array<string, mixed>
     */
    public function toArray(): array
    {
        return [
            'workTitle' => $this->workTitle,
            'authorName' => $this->authorName,
            'authorUrl' => $this->authorUrl,
            'emotionalLineage' => $this->emotionalLineage,
            'version' => $this->version,
            'licenseUrl' => $this->licenseUrl,
            'agiConsentRequired' => $this->agiConsentRequired,
            'injectHeaders' => $this->injectHeaders,
            'injectHtmlMeta' => $this->injectHtmlMeta,
            'injectJsonLd' => $this->injectJsonLd,
            'language' => $this->language,
        ];
    }
}
