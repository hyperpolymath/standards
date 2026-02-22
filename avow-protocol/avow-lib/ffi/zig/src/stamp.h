/* SPDX-License-Identifier: AGPL-3.0-or-later */
/* Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk> */

/**
 * @file stamp.h
 * @brief STAMP Protocol FFI Header
 *
 * C-compatible header for the STAMP verification library.
 * Use this header to integrate STAMP with C, C++, Rust, Python, or any
 * language with C FFI support.
 */

#ifndef STAMP_H
#define STAMP_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Result codes for STAMP verification functions
 */
typedef enum {
    STAMP_SUCCESS = 0,
    STAMP_ERROR_INVALID_URL = -1,
    STAMP_ERROR_TIMEOUT = -2,
    STAMP_ERROR_INVALID_RESPONSE = -3,
    STAMP_ERROR_INVALID_SIGNATURE = -4,
    STAMP_ERROR_RATE_LIMIT_EXCEEDED = -5,
    STAMP_ERROR_CONSENT_INVALID = -6,
    STAMP_ERROR_NULL_POINTER = -7,
    STAMP_ERROR_INTERNAL = -99,
} StampResult;

/**
 * @brief Parameters for unsubscribe link verification
 */
typedef struct {
    /** URL to verify (null-terminated) */
    const char* url;

    /** When the URL was tested (Unix timestamp in milliseconds) */
    uint64_t tested_at;

    /** HTTP response code (200, 404, etc.) */
    uint16_t response_code;

    /** Response time in milliseconds */
    uint32_t response_time;

    /** Cryptographic token (null-terminated, 64 chars) */
    const char* token;

    /** Signature (null-terminated) */
    const char* signature;
} StampUnsubscribeParams;

/**
 * @brief Parameters for consent chain verification
 */
typedef struct {
    /** Initial opt-in request timestamp (Unix milliseconds) */
    uint64_t initial_request;

    /** Confirmation timestamp (Unix milliseconds) */
    uint64_t confirmation;

    /** IP address (null-terminated) */
    const char* ip_address;

    /** Confirmation token (null-terminated) */
    const char* token;
} StampConsentParams;

/**
 * @brief Parameters for rate limit verification
 */
typedef struct {
    /** Sender identifier (null-terminated) */
    const char* sender_id;

    /** Account creation timestamp (Unix milliseconds) */
    uint64_t account_created;

    /** Number of messages sent today */
    uint32_t messages_today;

    /** Daily limit for this account */
    uint32_t daily_limit;
} StampRateLimitParams;

/**
 * @brief Verification proof (JSON format)
 *
 * Must be freed with stamp_free_proof() after use.
 */
typedef struct {
    /** Proof data (JSON format, null-terminated) */
    char* data;

    /** Length of proof data */
    size_t length;

    /** Signature over proof data (null-terminated) */
    char* signature;
} StampProof;

/**
 * @brief Verify an unsubscribe link
 *
 * Checks that:
 * - URL format is valid (HTTPS)
 * - Test was recent (within last 60 seconds)
 * - HTTP response was 200 OK
 * - Response time was fast (< 200ms)
 * - Signature is valid
 *
 * @param params Verification parameters
 * @return STAMP_SUCCESS if valid, error code otherwise
 *
 * @example
 * ```c
 * StampUnsubscribeParams params = {
 *     .url = "https://example.com/unsub",
 *     .tested_at = 1706630400000,
 *     .response_code = 200,
 *     .response_time = 87,
 *     .token = "abc123...",
 *     .signature = "sig...",
 * };
 * int result = stamp_verify_unsubscribe(&params);
 * if (result == STAMP_SUCCESS) {
 *     printf("Valid unsubscribe link\n");
 * }
 * ```
 */
StampResult stamp_verify_unsubscribe(const StampUnsubscribeParams* params);

/**
 * @brief Verify a consent chain (double opt-in)
 *
 * Checks that:
 * - Confirmation happened AFTER initial request
 * - Confirmation was timely (within 24 hours)
 * - Token is cryptographically valid
 *
 * @param params Consent verification parameters
 * @return STAMP_SUCCESS if valid, error code otherwise
 *
 * @example
 * ```c
 * StampConsentParams params = {
 *     .initial_request = 1706630400000,
 *     .confirmation = 1706630500000, // 100 seconds later
 *     .ip_address = "192.168.1.100",
 *     .token = "token...",
 * };
 * int result = stamp_verify_consent(&params);
 * ```
 */
StampResult stamp_verify_consent(const StampConsentParams* params);

/**
 * @brief Verify rate limit compliance
 *
 * Checks that:
 * - Messages today doesn't exceed daily limit
 * - Daily limit is appropriate for account age
 *
 * Account age determines max daily limit:
 * - < 30 days: max 1,000/day
 * - 30-90 days: max 10,000/day
 * - 90+ days: max 100,000/day
 *
 * @param params Rate limit parameters
 * @return STAMP_SUCCESS if within limits, error code otherwise
 *
 * @example
 * ```c
 * StampRateLimitParams params = {
 *     .sender_id = "user123",
 *     .account_created = 1704067200000, // 30 days ago
 *     .messages_today = 50,
 *     .daily_limit = 1000,
 * };
 * int result = stamp_verify_rate_limit(&params);
 * ```
 */
StampResult stamp_verify_rate_limit(const StampRateLimitParams* params);

/**
 * @brief Generate a verification proof
 *
 * Creates a JSON proof that can be displayed to users or stored.
 * The proof includes all verification details and a cryptographic signature.
 *
 * @param params Verification parameters
 * @return Pointer to StampProof on success, NULL on error
 *
 * @warning Caller must free the returned proof with stamp_free_proof()
 *
 * @example
 * ```c
 * StampProof* proof = stamp_generate_proof(&params);
 * if (proof != NULL) {
 *     printf("Proof: %s\n", proof->data);
 *     stamp_free_proof(proof);
 * }
 * ```
 */
StampProof* stamp_generate_proof(const StampUnsubscribeParams* params);

/**
 * @brief Free a proof returned by stamp_generate_proof()
 *
 * @param proof Proof to free (can be NULL)
 *
 * @example
 * ```c
 * StampProof* proof = stamp_generate_proof(&params);
 * // ... use proof ...
 * stamp_free_proof(proof);
 * ```
 */
void stamp_free_proof(StampProof* proof);

/**
 * @brief Get library version string
 *
 * @return Version string (e.g., "libstamp v1.0.0")
 */
const char* stamp_version(void);

#ifdef __cplusplus
}
#endif

#endif /* STAMP_H */
