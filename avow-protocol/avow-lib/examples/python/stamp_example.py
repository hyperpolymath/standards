#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

"""
Python example using libstamp via ctypes FFI
"""

import ctypes
import platform
import time
from pathlib import Path
from typing import Optional

# ============================================================================
# Load Library
# ============================================================================

def load_libstamp():
    """Load libstamp shared library"""
    # Determine library extension based on platform
    if platform.system() == "Darwin":
        lib_name = "libstamp.dylib"
    elif platform.system() == "Windows":
        lib_name = "stamp.dll"
    else:
        lib_name = "libstamp.so"

    # Try to find library in standard locations
    search_paths = [
        Path(__file__).parent.parent.parent / "ffi" / "zig" / "zig-out" / "lib" / lib_name,
        Path("/usr/local/lib") / lib_name,
        Path("/usr/lib") / lib_name,
    ]

    for path in search_paths:
        if path.exists():
            return ctypes.CDLL(str(path))

    raise FileNotFoundError(f"Could not find {lib_name}. Build with: cd ffi/zig && zig build")

libstamp = load_libstamp()

# ============================================================================
# Type Definitions
# ============================================================================

class StampResult:
    SUCCESS = 0
    ERROR_INVALID_URL = -1
    ERROR_TIMEOUT = -2
    ERROR_INVALID_RESPONSE = -3
    ERROR_INVALID_SIGNATURE = -4
    ERROR_RATE_LIMIT_EXCEEDED = -5
    ERROR_CONSENT_INVALID = -6
    ERROR_NULL_POINTER = -7
    ERROR_INTERNAL = -99

    @staticmethod
    def to_string(code: int) -> str:
        mapping = {
            0: "✓ SUCCESS",
            -1: "✗ INVALID_URL",
            -2: "✗ TIMEOUT",
            -3: "✗ INVALID_RESPONSE",
            -4: "✗ INVALID_SIGNATURE",
            -5: "✗ RATE_LIMIT_EXCEEDED",
            -6: "✗ CONSENT_INVALID",
            -7: "✗ NULL_POINTER",
            -99: "✗ INTERNAL_ERROR",
        }
        return mapping.get(code, "✗ UNKNOWN_ERROR")

class StampUnsubscribeParams(ctypes.Structure):
    _fields_ = [
        ("url", ctypes.c_char_p),
        ("tested_at", ctypes.c_uint64),
        ("response_code", ctypes.c_uint16),
        ("response_time", ctypes.c_uint32),
        ("token", ctypes.c_char_p),
        ("signature", ctypes.c_char_p),
    ]

class StampConsentParams(ctypes.Structure):
    _fields_ = [
        ("initial_request", ctypes.c_uint64),
        ("confirmation", ctypes.c_uint64),
        ("ip_address", ctypes.c_char_p),
        ("token", ctypes.c_char_p),
    ]

class StampRateLimitParams(ctypes.Structure):
    _fields_ = [
        ("sender_id", ctypes.c_char_p),
        ("account_created", ctypes.c_uint64),
        ("messages_today", ctypes.c_uint32),
        ("daily_limit", ctypes.c_uint32),
    ]

class StampProof(ctypes.Structure):
    _fields_ = [
        ("data", ctypes.c_char_p),
        ("length", ctypes.c_size_t),
        ("signature", ctypes.c_char_p),
    ]

# ============================================================================
# Function Signatures
# ============================================================================

libstamp.stamp_verify_unsubscribe.argtypes = [ctypes.POINTER(StampUnsubscribeParams)]
libstamp.stamp_verify_unsubscribe.restype = ctypes.c_int

libstamp.stamp_verify_consent.argtypes = [ctypes.POINTER(StampConsentParams)]
libstamp.stamp_verify_consent.restype = ctypes.c_int

libstamp.stamp_verify_rate_limit.argtypes = [ctypes.POINTER(StampRateLimitParams)]
libstamp.stamp_verify_rate_limit.restype = ctypes.c_int

libstamp.stamp_generate_proof.argtypes = [ctypes.POINTER(StampUnsubscribeParams)]
libstamp.stamp_generate_proof.restype = ctypes.POINTER(StampProof)

libstamp.stamp_free_proof.argtypes = [ctypes.POINTER(StampProof)]
libstamp.stamp_free_proof.restype = None

libstamp.stamp_version.argtypes = []
libstamp.stamp_version.restype = ctypes.c_char_p

# ============================================================================
# Python Wrappers
# ============================================================================

def get_current_timestamp() -> int:
    """Get current Unix timestamp in milliseconds"""
    return int(time.time() * 1000)

def verify_unsubscribe(
    url: str,
    tested_at: int,
    response_code: int,
    response_time: int,
    token: str,
    signature: str,
) -> int:
    """Verify an unsubscribe link"""
    params = StampUnsubscribeParams(
        url=url.encode('utf-8'),
        tested_at=tested_at,
        response_code=response_code,
        response_time=response_time,
        token=token.encode('utf-8'),
        signature=signature.encode('utf-8'),
    )
    return libstamp.stamp_verify_unsubscribe(ctypes.byref(params))

def verify_consent(
    initial_request: int,
    confirmation: int,
    ip_address: str,
    token: str,
) -> int:
    """Verify a consent chain"""
    params = StampConsentParams(
        initial_request=initial_request,
        confirmation=confirmation,
        ip_address=ip_address.encode('utf-8'),
        token=token.encode('utf-8'),
    )
    return libstamp.stamp_verify_consent(ctypes.byref(params))

def verify_rate_limit(
    sender_id: str,
    account_created: int,
    messages_today: int,
    daily_limit: int,
) -> int:
    """Verify rate limit compliance"""
    params = StampRateLimitParams(
        sender_id=sender_id.encode('utf-8'),
        account_created=account_created,
        messages_today=messages_today,
        daily_limit=daily_limit,
    )
    return libstamp.stamp_verify_rate_limit(ctypes.byref(params))

def generate_proof(
    url: str,
    tested_at: int,
    response_code: int,
    response_time: int,
    token: str,
    signature: str,
) -> Optional[str]:
    """Generate a verification proof"""
    params = StampUnsubscribeParams(
        url=url.encode('utf-8'),
        tested_at=tested_at,
        response_code=response_code,
        response_time=response_time,
        token=token.encode('utf-8'),
        signature=signature.encode('utf-8'),
    )

    proof_ptr = libstamp.stamp_generate_proof(ctypes.byref(params))
    if not proof_ptr:
        return None

    proof = proof_ptr.contents
    proof_data = proof.data.decode('utf-8')

    libstamp.stamp_free_proof(proof_ptr)
    return proof_data

def get_version() -> str:
    """Get library version"""
    return libstamp.stamp_version().decode('utf-8')

# ============================================================================
# Examples
# ============================================================================

def main():
    print("=== STAMP Python Example ===\n")
    print(f"Version: {get_version()}\n")

    now = get_current_timestamp()

    # Example 1: Valid unsubscribe link
    print("Example 1: Valid Unsubscribe Link")
    print("==================================")

    result = verify_unsubscribe(
        url="https://example.com/unsubscribe",
        tested_at=now - 5000,  # 5 seconds ago
        response_code=200,
        response_time=87,
        token="abc123def456",
        signature="valid_signature",
    )

    print(f"Result: {StampResult.to_string(result)}")
    assert result == StampResult.SUCCESS
    print()

    # Generate proof
    proof = generate_proof(
        url="https://example.com/unsubscribe",
        tested_at=now - 5000,
        response_code=200,
        response_time=87,
        token="abc123def456",
        signature="valid_signature",
    )
    if proof:
        print("Proof generated:")
        print(proof)
        print()

    # Example 2: Invalid URL
    print("Example 2: Invalid URL")
    print("======================")

    result = verify_unsubscribe(
        url="not_a_url",
        tested_at=now,
        response_code=200,
        response_time=87,
        token="token",
        signature="sig",
    )

    print(f"Result: {StampResult.to_string(result)}")
    assert result == StampResult.ERROR_INVALID_URL
    print()

    # Example 3: Valid consent chain
    print("Example 3: Valid Consent Chain")
    print("===============================")

    initial = 1706630400000
    confirmation = initial + 100000  # 100 seconds later

    result = verify_consent(
        initial_request=initial,
        confirmation=confirmation,
        ip_address="192.168.1.100",
        token="consent_token",
    )

    print(f"Initial: {initial}")
    print(f"Confirmation: {confirmation} (+{(confirmation - initial) // 1000} seconds)")
    print(f"Result: {StampResult.to_string(result)}")
    assert result == StampResult.SUCCESS
    print()

    # Example 4: Invalid consent order
    print("Example 4: Invalid Consent Order")
    print("=================================")

    result = verify_consent(
        initial_request=confirmation,  # Swapped!
        confirmation=initial,
        ip_address="192.168.1.100",
        token="consent_token",
    )

    print(f"Result: {StampResult.to_string(result)}")
    assert result == StampResult.ERROR_CONSENT_INVALID
    print()

    # Example 5: Rate limit check
    print("Example 5: Rate Limit Check")
    print("============================")

    account_age = now - (45 * 24 * 60 * 60 * 1000)  # 45 days ago

    result = verify_rate_limit(
        sender_id="user_12345",
        account_created=account_age,
        messages_today=150,
        daily_limit=10000,
    )

    age_days = (now - account_age) // (24 * 60 * 60 * 1000)
    print(f"Account age: {age_days} days")
    print(f"Messages: 150/10000")
    print(f"Result: {StampResult.to_string(result)}")
    assert result == StampResult.SUCCESS
    print()

    # Example 6: Rate limit exceeded
    print("Example 6: Rate Limit Exceeded")
    print("===============================")

    result = verify_rate_limit(
        sender_id="spammer",
        account_created=account_age,
        messages_today=10000,  # At limit
        daily_limit=10000,
    )

    print(f"Messages: 10000/10000")
    print(f"Result: {StampResult.to_string(result)}")
    assert result == StampResult.ERROR_RATE_LIMIT_EXCEEDED
    print()

    print("All examples passed! ✓")

if __name__ == "__main__":
    main()
