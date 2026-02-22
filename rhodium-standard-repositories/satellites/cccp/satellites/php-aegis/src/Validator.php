<?php

declare(strict_types=1);

namespace PhpAegis;

/**
 * Input validation utilities
 */
class Validator
{
    /**
     * Validate email address
     */
    public function email(string $email): bool
    {
        return filter_var($email, FILTER_VALIDATE_EMAIL) !== false;
    }

    /**
     * Validate URL
     */
    public function url(string $url): bool
    {
        return filter_var($url, FILTER_VALIDATE_URL) !== false;
    }
}
