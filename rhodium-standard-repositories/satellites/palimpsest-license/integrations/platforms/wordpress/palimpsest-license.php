<?php
/**
 * Plugin Name: Palimpsest License
 * Plugin URI: https://palimpsestlicense.org
 * Description: Add Palimpsest License metadata to your WordPress site
 * Version: 0.4.0
 * Author: Palimpsest License Contributors
 * Author URI: https://palimpsestlicense.org
 * License: Palimpsest-0.4
 * Text Domain: palimpsest-license
 */

// Prevent direct access
if (!defined('ABSPATH')) {
    exit;
}

// Plugin constants
define('PALIMPSEST_VERSION', '0.4.0');
define('PALIMPSEST_PLUGIN_DIR', plugin_dir_path(__FILE__));
define('PALIMPSEST_PLUGIN_URL', plugin_dir_url(__FILE__));

/**
 * Main plugin class
 */
class Palimpsest_License_Plugin {

    private static $instance = null;
    private $options;

    /**
     * Get singleton instance
     */
    public static function get_instance() {
        if (self::$instance === null) {
            self::$instance = new self();
        }
        return self::$instance;
    }

    /**
     * Constructor
     */
    private function __construct() {
        $this->options = get_option('palimpsest_license_options', []);
        $this->init_hooks();
    }

    /**
     * Initialise WordPress hooks
     */
    private function init_hooks() {
        // Add meta tags to header
        add_action('wp_head', [$this, 'add_meta_tags'], 1);

        // Add JSON-LD to footer
        add_action('wp_footer', [$this, 'add_jsonld'], 1);

        // Add HTTP headers
        add_action('send_headers', [$this, 'add_http_headers']);

        // Admin menu
        add_action('admin_menu', [$this, 'add_admin_menu']);
        add_action('admin_init', [$this, 'register_settings']);

        // Shortcodes
        add_shortcode('palimpsest_badge', [$this, 'badge_shortcode']);
        add_shortcode('palimpsest_notice', [$this, 'notice_shortcode']);
    }

    /**
     * Add meta tags to page header
     */
    public function add_meta_tags() {
        $version = $this->get_option('version', '0.4');
        $license_url = $this->get_option('license_url', "https://palimpsestlicense.org/v{$version}");
        $author = $this->get_option('author_name', get_bloginfo('name'));
        $agi_consent = $this->get_option('agi_consent_required', true);

        echo "\n<!-- Palimpsest License Metadata -->\n";
        echo '<meta name="license" content="' . esc_attr($license_url) . '">' . "\n";
        echo '<meta name="license-type" content="Palimpsest-' . esc_attr($version) . '">' . "\n";
        echo '<meta property="og:license" content="' . esc_attr($license_url) . '">' . "\n";
        echo '<meta name="dcterms.license" content="' . esc_attr($license_url) . '">' . "\n";
        echo '<meta name="palimpsest:version" content="' . esc_attr($version) . '">' . "\n";
        echo '<meta name="palimpsest:agi-consent" content="' . ($agi_consent ? 'true' : 'false') . '">' . "\n";

        if ($author) {
            echo '<meta name="author" content="' . esc_attr($author) . '">' . "\n";
            echo '<meta name="dcterms.creator" content="' . esc_attr($author) . '">' . "\n";
        }

        $emotional_lineage = $this->get_option('emotional_lineage');
        if ($emotional_lineage) {
            echo '<meta name="palimpsest:emotional-lineage" content="' . esc_attr($emotional_lineage) . '">' . "\n";
        }

        echo "<!-- End Palimpsest License Metadata -->\n\n";
    }

    /**
     * Add JSON-LD to page footer
     */
    public function add_jsonld() {
        $version = $this->get_option('version', '0.4');
        $license_url = $this->get_option('license_url', "https://palimpsestlicense.org/v{$version}");
        $work_title = $this->get_option('work_title', get_bloginfo('name'));
        $author = $this->get_option('author_name', get_bloginfo('name'));
        $agi_consent = $this->get_option('agi_consent_required', true);

        $metadata = [
            '@context' => 'https://schema.org',
            '@type' => 'CreativeWork',
            'name' => $work_title,
            'author' => [
                '@type' => 'Person',
                'name' => $author,
            ],
            'license' => $license_url,
            'usageInfo' => 'https://palimpsestlicense.org',
            'additionalProperty' => [
                [
                    '@type' => 'PropertyValue',
                    'name' => 'Palimpsest:Version',
                    'value' => $version,
                ],
                [
                    '@type' => 'PropertyValue',
                    'name' => 'Palimpsest:AGIConsent',
                    'value' => $agi_consent
                        ? 'Explicit consent required for AI training. See license for details.'
                        : 'Consent granted with attribution. See license for details.',
                ],
            ],
        ];

        $emotional_lineage = $this->get_option('emotional_lineage');
        if ($emotional_lineage) {
            $metadata['additionalProperty'][] = [
                '@type' => 'PropertyValue',
                'name' => 'Palimpsest:EmotionalLineage',
                'value' => $emotional_lineage,
            ];
        }

        echo "\n<script type=\"application/ld+json\">\n";
        echo wp_json_encode($metadata, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES);
        echo "\n</script>\n";
    }

    /**
     * Add HTTP headers
     */
    public function add_http_headers() {
        $version = $this->get_option('version', '0.4');
        $license_url = $this->get_option('license_url', "https://palimpsestlicense.org/v{$version}");
        $agi_consent = $this->get_option('agi_consent_required', true);

        header("X-License: Palimpsest-{$version}");
        header("X-License-Url: {$license_url}");
        header("X-Palimpsest-Version: {$version}");
        header("X-Palimpsest-AGI-Consent: " . ($agi_consent ? 'true' : 'false'));
        header("Link: <{$license_url}>; rel=\"license\"");
    }

    /**
     * Badge shortcode
     */
    public function badge_shortcode($atts) {
        $atts = shortcode_atts([
            'theme' => 'light',
            'language' => 'en',
        ], $atts);

        $version = $this->get_option('version', '0.4');
        $license_url = $this->get_option('license_url', "https://palimpsestlicense.org/v{$version}");

        $text = $atts['language'] === 'nl'
            ? "Palimpsest Licentie v{$version}"
            : "Palimpsest License v{$version}";

        $styles = $atts['theme'] === 'dark'
            ? 'background: #0d1117; border-color: #30363d; color: #c9d1d9;'
            : 'background: #f6f8fa; border-color: #e1e4e8; color: #24292e;';

        return sprintf(
            '<a href="%s" target="_blank" rel="license noopener" style="display: inline-flex; align-items: center; padding: 6px 12px; border: 1px solid; border-radius: 6px; font-size: 12px; font-weight: 500; text-decoration: none; %s">%s</a>',
            esc_url($license_url),
            $styles,
            esc_html($text)
        );
    }

    /**
     * Notice shortcode
     */
    public function notice_shortcode($atts) {
        $atts = shortcode_atts([
            'theme' => 'light',
            'language' => 'en',
        ], $atts);

        // Implementation similar to badge but with full notice
        // ... (truncated for brevity)

        return '<div class="palimpsest-notice">License notice here</div>';
    }

    /**
     * Add admin menu
     */
    public function add_admin_menu() {
        add_options_page(
            'Palimpsest License Settings',
            'Palimpsest License',
            'manage_options',
            'palimpsest-license',
            [$this, 'render_admin_page']
        );
    }

    /**
     * Register settings
     */
    public function register_settings() {
        register_setting('palimpsest_license_options', 'palimpsest_license_options');
    }

    /**
     * Render admin page
     */
    public function render_admin_page() {
        require_once PALIMPSEST_PLUGIN_DIR . 'admin/settings-page.php';
    }

    /**
     * Get option value
     */
    private function get_option($key, $default = null) {
        return isset($this->options[$key]) ? $this->options[$key] : $default;
    }
}

// Initialise plugin
Palimpsest_License_Plugin::get_instance();
