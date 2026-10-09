<?php

declare(strict_types=1);

namespace PhpAegis;

/**
 * Input sanitization utilities
 */
class Sanitizer
{
    /**
     * Sanitize HTML to prevent XSS
     */
    public function html(string $input): string
    {
        return htmlspecialchars($input, ENT_QUOTES | ENT_HTML5, 'UTF-8');
    }

    /**
     * Strip all tags from input
     */
    public function stripTags(string $input): string
    {
        return strip_tags($input);
    }
}
