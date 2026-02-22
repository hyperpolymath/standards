// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

//! Rust example using libstamp via FFI

use std::ffi::{CStr, CString};
use std::os::raw::{c_char, c_int};
use std::time::{SystemTime, UNIX_EPOCH};

// ============================================================================
// FFI Bindings
// ============================================================================

#[repr(C)]
#[derive(Debug, Copy, Clone, PartialEq)]
#[allow(dead_code)]
enum StampResult {
    Success = 0,
    ErrorInvalidUrl = -1,
    ErrorTimeout = -2,
    ErrorInvalidResponse = -3,
    ErrorInvalidSignature = -4,
    ErrorRateLimitExceeded = -5,
    ErrorConsentInvalid = -6,
    ErrorNullPointer = -7,
    ErrorInternal = -99,
}

impl From<c_int> for StampResult {
    fn from(value: c_int) -> Self {
        match value {
            0 => StampResult::Success,
            -1 => StampResult::ErrorInvalidUrl,
            -2 => StampResult::ErrorTimeout,
            -3 => StampResult::ErrorInvalidResponse,
            -4 => StampResult::ErrorInvalidSignature,
            -5 => StampResult::ErrorRateLimitExceeded,
            -6 => StampResult::ErrorConsentInvalid,
            -7 => StampResult::ErrorNullPointer,
            _ => StampResult::ErrorInternal,
        }
    }
}

#[repr(C)]
struct StampUnsubscribeParams {
    url: *const c_char,
    tested_at: u64,
    response_code: u16,
    response_time: u32,
    token: *const c_char,
    signature: *const c_char,
}

#[repr(C)]
struct StampConsentParams {
    initial_request: u64,
    confirmation: u64,
    ip_address: *const c_char,
    token: *const c_char,
}

#[repr(C)]
struct StampRateLimitParams {
    sender_id: *const c_char,
    account_created: u64,
    messages_today: u32,
    daily_limit: u32,
}

#[repr(C)]
struct StampProof {
    data: *mut c_char,
    length: usize,
    signature: *mut c_char,
}

extern "C" {
    fn stamp_verify_unsubscribe(params: *const StampUnsubscribeParams) -> c_int;
    fn stamp_verify_consent(params: *const StampConsentParams) -> c_int;
    fn stamp_verify_rate_limit(params: *const StampRateLimitParams) -> c_int;
    fn stamp_generate_proof(params: *const StampUnsubscribeParams) -> *mut StampProof;
    fn stamp_free_proof(proof: *mut StampProof);
    fn stamp_version() -> *const c_char;
}

// ============================================================================
// Safe Rust Wrappers
// ============================================================================

fn get_current_timestamp() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as u64
}

fn verify_unsubscribe(
    url: &str,
    tested_at: u64,
    response_code: u16,
    response_time: u32,
    token: &str,
    signature: &str,
) -> StampResult {
    let url_c = CString::new(url).unwrap();
    let token_c = CString::new(token).unwrap();
    let signature_c = CString::new(signature).unwrap();

    let params = StampUnsubscribeParams {
        url: url_c.as_ptr(),
        tested_at,
        response_code,
        response_time,
        token: token_c.as_ptr(),
        signature: signature_c.as_ptr(),
    };

    let result = unsafe { stamp_verify_unsubscribe(&params) };
    StampResult::from(result)
}

fn verify_consent(
    initial_request: u64,
    confirmation: u64,
    ip_address: &str,
    token: &str,
) -> StampResult {
    let ip_c = CString::new(ip_address).unwrap();
    let token_c = CString::new(token).unwrap();

    let params = StampConsentParams {
        initial_request,
        confirmation,
        ip_address: ip_c.as_ptr(),
        token: token_c.as_ptr(),
    };

    let result = unsafe { stamp_verify_consent(&params) };
    StampResult::from(result)
}

fn verify_rate_limit(
    sender_id: &str,
    account_created: u64,
    messages_today: u32,
    daily_limit: u32,
) -> StampResult {
    let sender_c = CString::new(sender_id).unwrap();

    let params = StampRateLimitParams {
        sender_id: sender_c.as_ptr(),
        account_created,
        messages_today,
        daily_limit,
    };

    let result = unsafe { stamp_verify_rate_limit(&params) };
    StampResult::from(result)
}

fn generate_proof(
    url: &str,
    tested_at: u64,
    response_code: u16,
    response_time: u32,
    token: &str,
    signature: &str,
) -> Option<String> {
    let url_c = CString::new(url).unwrap();
    let token_c = CString::new(token).unwrap();
    let signature_c = CString::new(signature).unwrap();

    let params = StampUnsubscribeParams {
        url: url_c.as_ptr(),
        tested_at,
        response_code,
        response_time,
        token: token_c.as_ptr(),
        signature: signature_c.as_ptr(),
    };

    let proof_ptr = unsafe { stamp_generate_proof(&params) };
    if proof_ptr.is_null() {
        return None;
    }

    unsafe {
        let proof = &*proof_ptr;
        let proof_str = CStr::from_ptr(proof.data).to_string_lossy().into_owned();
        stamp_free_proof(proof_ptr);
        Some(proof_str)
    }
}

fn get_version() -> String {
    unsafe {
        CStr::from_ptr(stamp_version())
            .to_string_lossy()
            .into_owned()
    }
}

// ============================================================================
// Examples
// ============================================================================

fn main() {
    println!("=== STAMP Rust Example ===\n");
    println!("Version: {}\n", get_version());

    // Example 1: Valid unsubscribe link
    println!("Example 1: Valid Unsubscribe Link");
    println!("==================================");

    let now = get_current_timestamp();
    let result = verify_unsubscribe(
        "https://example.com/unsubscribe",
        now - 5000, // 5 seconds ago
        200,
        87,
        "abc123def456",
        "valid_signature",
    );

    println!("Result: {:?}", result);
    assert_eq!(result, StampResult::Success);
    println!("✓ Verification passed\n");

    // Generate and display proof
    if let Some(proof) = generate_proof(
        "https://example.com/unsubscribe",
        now - 5000,
        200,
        87,
        "abc123def456",
        "valid_signature",
    ) {
        println!("Proof generated:");
        println!("{}\n", proof);
    }

    // Example 2: Invalid URL
    println!("Example 2: Invalid URL");
    println!("======================");

    let result = verify_unsubscribe(
        "not_a_url",
        now,
        200,
        87,
        "token",
        "sig",
    );

    println!("Result: {:?}", result);
    assert_eq!(result, StampResult::ErrorInvalidUrl);
    println!("✓ Correctly rejected invalid URL\n");

    // Example 3: Valid consent chain
    println!("Example 3: Valid Consent Chain");
    println!("===============================");

    let initial = 1706630400000u64;
    let confirmation = initial + 100000; // 100 seconds later

    let result = verify_consent(
        initial,
        confirmation,
        "192.168.1.100",
        "consent_token",
    );

    println!("Initial: {}", initial);
    println!("Confirmation: {} (+{} seconds)", confirmation, (confirmation - initial) / 1000);
    println!("Result: {:?}", result);
    assert_eq!(result, StampResult::Success);
    println!("✓ Consent verified\n");

    // Example 4: Invalid consent (confirmation before request)
    println!("Example 4: Invalid Consent Order");
    println!("=================================");

    let result = verify_consent(
        confirmation,  // Swapped!
        initial,
        "192.168.1.100",
        "consent_token",
    );

    println!("Result: {:?}", result);
    assert_eq!(result, StampResult::ErrorConsentInvalid);
    println!("✓ Correctly rejected invalid consent order\n");

    // Example 5: Rate limit check
    println!("Example 5: Rate Limit Check");
    println!("============================");

    let account_age = now - (45 * 24 * 60 * 60 * 1000); // 45 days ago

    let result = verify_rate_limit(
        "user_12345",
        account_age,
        150,   // messages today
        10000, // daily limit
    );

    println!("Account age: {} days", (now - account_age) / (24 * 60 * 60 * 1000));
    println!("Messages: 150/10000");
    println!("Result: {:?}", result);
    assert_eq!(result, StampResult::Success);
    println!("✓ Within rate limits\n");

    // Example 6: Rate limit exceeded
    println!("Example 6: Rate Limit Exceeded");
    println!("===============================");

    let result = verify_rate_limit(
        "spammer",
        account_age,
        10000, // At limit
        10000,
    );

    println!("Messages: 10000/10000");
    println!("Result: {:?}", result);
    assert_eq!(result, StampResult::ErrorRateLimitExceeded);
    println!("✓ Correctly detected rate limit violation\n");

    println!("All examples passed! ✓");
}
