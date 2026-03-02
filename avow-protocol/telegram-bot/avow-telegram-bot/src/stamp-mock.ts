// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

/**
 * Mock STAMP verification library
 *
 * This is a temporary implementation for the MVP Telegram bot.
 * It implements the same interface as the real Zig FFI, but without
 * dependent type proofs. Good enough to demo the UX.
 *
 * TODO: Replace with real libstamp FFI in Week 2
 */

// ============================================================================
// Types
// ============================================================================

export interface UnsubscribeParams {
  url: string;
  tested_at: number;
  response_code: number;
  response_time: number;
  token: string;
  signature: string;
}

export interface ConsentParams {
  initial_request: number;
  confirmation: number;
  ip_address: string;
  token: string;
}

export interface RateLimitParams {
  sender_id: string;
  account_created: number;
  messages_today: number;
  daily_limit: number;
}

export interface Proof {
  type: string;
  data: Record<string, unknown>;
  timestamp: number;
  signature: string;
}

export enum VerificationResult {
  SUCCESS = 0,
  ERROR_INVALID_URL = -1,
  ERROR_TIMEOUT = -2,
  ERROR_INVALID_RESPONSE = -3,
  ERROR_INVALID_SIGNATURE = -4,
  ERROR_RATE_LIMIT_EXCEEDED = -5,
  ERROR_CONSENT_INVALID = -6,
  ERROR_NULL_POINTER = -7,
  ERROR_INTERNAL = -99,
}

// ============================================================================
// Mock Verification Functions
// ============================================================================

/**
 * Verify an unsubscribe link
 *
 * Mock implementation that checks basic properties without formal proofs.
 * Real version (libstamp) would use Idris2 dependent types to prove these.
 */
export function verifyUnsubscribe(params: UnsubscribeParams): VerificationResult {
  // Check URL format
  if (!params.url.startsWith('https://')) {
    return VerificationResult.ERROR_INVALID_URL;
  }

  // Check test was recent (within last 60 seconds)
  const now = Date.now();
  const age_ms = now - params.tested_at;
  if (age_ms > 60000 || age_ms < 0) {
    return VerificationResult.ERROR_TIMEOUT;
  }

  // Check HTTP response code
  if (params.response_code !== 200) {
    return VerificationResult.ERROR_INVALID_RESPONSE;
  }

  // Check response time (< 200ms)
  if (params.response_time >= 200) {
    return VerificationResult.ERROR_TIMEOUT;
  }

  // Check signature exists (real version would verify cryptographically)
  if (!params.signature || params.signature.length === 0) {
    return VerificationResult.ERROR_INVALID_SIGNATURE;
  }

  return VerificationResult.SUCCESS;
}

/**
 * Verify a consent chain (double opt-in)
 *
 * Mock implementation - real version would cryptographically verify the token.
 */
export function verifyConsent(params: ConsentParams): VerificationResult {
  // Check confirmation happened AFTER initial request
  if (params.confirmation <= params.initial_request) {
    return VerificationResult.ERROR_CONSENT_INVALID;
  }

  // Check confirmation was timely (within 24 hours)
  const time_diff = params.confirmation - params.initial_request;
  if (time_diff > 86400000) { // 24 hours in milliseconds
    return VerificationResult.ERROR_CONSENT_INVALID;
  }

  // Check token exists (real version would verify HMAC)
  if (!params.token || params.token.length === 0) {
    return VerificationResult.ERROR_INVALID_SIGNATURE;
  }

  return VerificationResult.SUCCESS;
}

/**
 * Verify rate limit compliance
 *
 * Mock implementation - real version would enforce at protocol level.
 */
export function verifyRateLimit(params: RateLimitParams): VerificationResult {
  // Check messages don't exceed limit
  if (params.messages_today >= params.daily_limit) {
    return VerificationResult.ERROR_RATE_LIMIT_EXCEEDED;
  }

  // Check daily limit is appropriate for account age
  const now = Date.now();
  const age_ms = now - params.account_created;
  const age_days = age_ms / (24 * 60 * 60 * 1000);

  let max_limit: number;
  if (age_days < 30) {
    max_limit = 1000;
  } else if (age_days < 90) {
    max_limit = 10000;
  } else {
    max_limit = 100000;
  }

  if (params.daily_limit > max_limit) {
    return VerificationResult.ERROR_RATE_LIMIT_EXCEEDED;
  }

  return VerificationResult.SUCCESS;
}

/**
 * Generate a verification proof (JSON format)
 *
 * Mock implementation - real version would include cryptographic signature.
 */
export function generateProof(
  type: 'unsubscribe' | 'consent' | 'rate_limit',
  params: UnsubscribeParams | ConsentParams | RateLimitParams,
): Proof {
  const timestamp = Date.now();

  // Generate mock signature (real version would use real crypto)
  const signature = `mock_sig_${timestamp}_${Math.random().toString(36).substring(7)}`;

  return {
    type: `${type}_verification`,
    data: params as Record<string, unknown>,
    timestamp,
    signature,
  };
}

/**
 * Format verification result as human-readable string
 */
export function resultToString(result: VerificationResult): string {
  const messages: Record<VerificationResult, string> = {
    [VerificationResult.SUCCESS]: '✓ SUCCESS',
    [VerificationResult.ERROR_INVALID_URL]: '✗ INVALID_URL',
    [VerificationResult.ERROR_TIMEOUT]: '✗ TIMEOUT',
    [VerificationResult.ERROR_INVALID_RESPONSE]: '✗ INVALID_RESPONSE',
    [VerificationResult.ERROR_INVALID_SIGNATURE]: '✗ INVALID_SIGNATURE',
    [VerificationResult.ERROR_RATE_LIMIT_EXCEEDED]: '✗ RATE_LIMIT_EXCEEDED',
    [VerificationResult.ERROR_CONSENT_INVALID]: '✗ CONSENT_INVALID',
    [VerificationResult.ERROR_NULL_POINTER]: '✗ NULL_POINTER',
    [VerificationResult.ERROR_INTERNAL]: '✗ INTERNAL_ERROR',
  };

  return messages[result] || '✗ UNKNOWN_ERROR';
}

/**
 * Format proof as pretty JSON
 */
export function formatProof(proof: Proof): string {
  return JSON.stringify(proof, null, 2);
}

// ============================================================================
// Helper Functions
// ============================================================================

/**
 * Generate a mock unsubscribe URL for testing
 */
export function generateUnsubscribeUrl(userId: number, token: string): string {
  return `https://stamp-bot.example.com/unsubscribe?user=${userId}&token=${token}`;
}

/**
 * Test an unsubscribe URL (mock HTTP request)
 *
 * Real version would actually make HTTP request and verify it works.
 */
export async function testUnsubscribeUrl(url: string): Promise<{
  response_code: number;
  response_time: number;
}> {
  // Mock: Simulate HTTP request
  const start = Date.now();

  // Simulate network delay
  await new Promise(resolve => setTimeout(resolve, 50 + Math.random() * 100));

  const response_time = Date.now() - start;

  // Mock: Valid URLs return 200, invalid ones return 404
  const response_code = url.startsWith('https://') ? 200 : 404;

  return { response_code, response_time };
}

/**
 * Generate a cryptographic token (mock)
 *
 * Real version would use proper HMAC with secret key.
 */
export function generateToken(userId: number): string {
  const random = Math.random().toString(36).substring(2, 15);
  const timestamp = Date.now().toString(36);
  return `${userId}_${timestamp}_${random}`;
}

/**
 * Generate a signature (mock)
 *
 * Real version would use Ed25519 or similar.
 */
export function generateSignature(data: string): string {
  // Mock: Just hash the data + timestamp
  const timestamp = Date.now();
  return `sig_${timestamp}_${data.length}_${Math.random().toString(36).substring(7)}`;
}
