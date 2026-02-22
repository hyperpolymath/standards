# SPDX-License-Identifier: MIT OR GPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

"""
AIBDP + HTTP 430 Middleware for Flask

Implements AI Boundary Declaration Protocol (AIBDP) enforcement
with HTTP 430 (Consent Required) responses for Flask applications.

License: MIT
Author: Jonathan D.A. Jewell <jonathan@metadatastician.art>
"""

import json
import re
from pathlib import Path
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple, Callable
from functools import wraps

from flask import request, jsonify, Response, make_response

# AI User-Agent patterns for detection
AI_USER_AGENTS = [
    re.compile(r'GPTBot', re.IGNORECASE),
    re.compile(r'ChatGPT-User', re.IGNORECASE),
    re.compile(r'Claude-Web', re.IGNORECASE),
    re.compile(r'anthropic-ai', re.IGNORECASE),
    re.compile(r'Google-Extended', re.IGNORECASE),
    re.compile(r'CCBot', re.IGNORECASE),
    re.compile(r'Googlebot', re.IGNORECASE),
    re.compile(r'Bingbot', re.IGNORECASE),
    re.compile(r'Slurp', re.IGNORECASE),
    re.compile(r'DuckDuckBot', re.IGNORECASE),
    re.compile(r'Baiduspider', re.IGNORECASE),
    re.compile(r'YandexBot', re.IGNORECASE),
    re.compile(r'PerplexityBot', re.IGNORECASE),
    re.compile(r'Diffbot', re.IGNORECASE),
]


class AIBDPManifest:
    """
    AIBDP Manifest loader and cache manager.
    """

    def __init__(self, manifest_path: str, cache_duration: int = 3600):
        """
        Initialize manifest loader.

        Args:
            manifest_path: Path to aibdp.json file
            cache_duration: Cache duration in seconds (default: 1 hour)
        """
        self.manifest_path = Path(manifest_path)
        self.cache_duration = cache_duration
        self._manifest: Optional[Dict] = None
        self._load_time: Optional[datetime] = None

    def load(self) -> Optional[Dict]:
        """
        Load manifest from file with caching.

        Returns:
            Parsed manifest dict or None if not found/invalid
        """
        now = datetime.now()

        # Return cached if still valid
        if self._manifest and self._load_time:
            if (now - self._load_time).total_seconds() < self.cache_duration:
                return self._manifest

        # Load from file
        try:
            if not self.manifest_path.exists():
                return None

            with open(self.manifest_path, 'r', encoding='utf-8') as f:
                self._manifest = json.load(f)
                self._load_time = now
                return self._manifest

        except (json.JSONDecodeError, OSError) as e:
            print(f"Warning: Failed to load AIBDP manifest: {e}")
            return None

    @property
    def manifest(self) -> Optional[Dict]:
        """Get current manifest (cached or fresh)."""
        return self.load()


def is_ai_user_agent(user_agent: str) -> bool:
    """
    Check if User-Agent indicates an AI system.

    Args:
        user_agent: User-Agent header value

    Returns:
        True if AI system detected
    """
    if not user_agent:
        return False

    return any(pattern.search(user_agent) for pattern in AI_USER_AGENTS)


def extract_ai_purpose(headers: Dict[str, str]) -> str:
    """
    Extract AI purpose from request headers.

    Args:
        headers: Request headers dict

    Returns:
        Detected purpose (training, indexing, etc.) or 'unknown'
    """
    # Check custom AI-Purpose header
    if 'AI-Purpose' in headers:
        return headers['AI-Purpose'].lower()

    # Infer from User-Agent
    ua = headers.get('User-Agent', '')

    if re.search(r'GPTBot', ua, re.IGNORECASE):
        return 'training'
    if re.search(r'Claude-Web', ua, re.IGNORECASE):
        return 'indexing'
    if re.search(r'Google-Extended', ua, re.IGNORECASE):
        return 'training'
    if re.search(r'Googlebot', ua, re.IGNORECASE):
        return 'indexing'

    return 'unknown'


def path_matches(request_path: str, pattern: str) -> bool:
    """
    Check if request path matches glob-style pattern.

    Args:
        request_path: Request path (e.g., '/docs/guide.html')
        pattern: Pattern from manifest (e.g., '/docs/**', '*.pdf')

    Returns:
        True if path matches pattern
    """
    if pattern == 'all':
        return True

    # Convert glob pattern to regex
    regex_pattern = (
        pattern
        .replace('.', r'\.')
        .replace('**', '.*')
        .replace('*', '[^/]*')
        .replace('?', '.')
    )

    return bool(re.match(f'^{regex_pattern}$', request_path))


def get_applicable_policy(
    manifest: Dict,
    purpose: str,
    request_path: str
) -> Optional[Dict]:
    """
    Get applicable policy for request path and purpose.

    Args:
        manifest: AIBDP manifest dict
        purpose: AI purpose (training, indexing, etc.)
        request_path: Request path

    Returns:
        Applicable policy dict or None
    """
    if not manifest or 'policies' not in manifest:
        return None

    policy = manifest['policies'].get(purpose)
    if not policy:
        return None

    # Check scope
    scope = policy.get('scope')
    if scope:
        if isinstance(scope, list):
            if not any(path_matches(request_path, pattern) for pattern in scope):
                return None
        # scope: "all" always matches

    # Check exceptions
    exceptions = policy.get('exceptions', [])
    for exception in exceptions:
        if path_matches(request_path, exception['path']):
            return exception  # Exception takes precedence

    return policy


def check_policy_conditions(policy: Dict, headers: Dict[str, str]) -> Tuple[bool, List[str]]:
    """
    Check if request satisfies policy conditions.

    Args:
        policy: Policy dict from manifest
        headers: Request headers

    Returns:
        Tuple of (satisfied: bool, missing: List[str])
    """
    if policy.get('status') != 'conditional':
        return True, []

    conditions = policy.get('conditions', [])
    if not conditions:
        return True, []

    missing = []

    # Check for consent headers
    if 'AI-Consent-Reviewed' not in headers:
        missing.append('AI-Consent-Reviewed header required')

    if 'AI-Consent-Conditions' not in headers:
        missing.append('AI-Consent-Conditions header required')

    # TODO: More sophisticated condition checking

    return len(missing) == 0, missing


def create_430_response(
    manifest: Dict,
    policy: Dict,
    purpose: str
) -> Response:
    """
    Create HTTP 430 Consent Required response.

    Args:
        manifest: AIBDP manifest
        policy: Violated policy
        purpose: AI purpose

    Returns:
        Flask Response object with HTTP 430
    """
    manifest_uri = manifest.get('canonical_uri', '/.well-known/aibdp.json')

    response_data = {
        'error': 'AI usage boundaries declared in AIBDP manifest not satisfied',
        'manifest': manifest_uri,
        'violated_policy': purpose,
        'policy_status': policy['status'],
        'required_conditions': policy.get('conditions', []),
        'rationale': policy.get('rationale', 'No additional information provided'),
        'contact': manifest.get('contact')
    }

    response = make_response(jsonify(response_data), 430)
    response.headers['Link'] = f'<{manifest_uri}>; rel="blocked-by-consent"'
    response.headers['Retry-After'] = '86400'  # 24 hours

    return response


class AIBDPMiddleware:
    """
    Flask middleware for AIBDP enforcement.
    """

    def __init__(
        self,
        app=None,
        manifest_path: str = '.well-known/aibdp.json',
        enforce_for_all: bool = False,
        on_violation: Optional[Callable] = None
    ):
        """
        Initialize AIBDP middleware.

        Args:
            app: Flask application (optional, can use init_app later)
            manifest_path: Path to AIBDP manifest
            enforce_for_all: Enforce for all requests, not just AI bots
            on_violation: Callback when violation detected
        """
        self.manifest_loader = AIBDPManifest(manifest_path)
        self.enforce_for_all = enforce_for_all
        self.on_violation = on_violation

        if app:
            self.init_app(app)

    def init_app(self, app):
        """
        Initialize middleware with Flask application.

        Args:
            app: Flask application
        """
        app.before_request(self.before_request)

    def before_request(self):
        """
        Flask before_request handler for AIBDP enforcement.

        Returns:
            Response object (HTTP 430) if violation detected, None otherwise
        """
        try:
            manifest = self.manifest_loader.manifest
            if not manifest:
                return None  # No manifest, no enforcement

            # Detect AI systems
            user_agent = request.headers.get('User-Agent', '')
            is_ai = self.enforce_for_all or is_ai_user_agent(user_agent)

            if not is_ai:
                return None  # Not an AI system, allow through

            # Extract purpose
            purpose = extract_ai_purpose(request.headers)

            # Get applicable policy
            policy = get_applicable_policy(manifest, purpose, request.path)

            if not policy:
                return None  # No policy for this purpose/path

            # Check policy status
            if policy['status'] == 'refused':
                if self.on_violation:
                    self.on_violation(request, policy, purpose)
                return create_430_response(manifest, policy, purpose)

            if policy['status'] == 'conditional':
                satisfied, missing = check_policy_conditions(policy, request.headers)
                if not satisfied:
                    if self.on_violation:
                        self.on_violation(request, policy, purpose)

                    response = create_430_response(manifest, policy, purpose)
                    response_data = response.get_json()
                    response_data['missing_conditions'] = missing
                    return make_response(jsonify(response_data), 430)

            # Policy satisfied or allowed
            return None

        except Exception as e:
            print(f"AIBDP middleware error: {e}")
            return None  # Fail open - don't break the site


def serve_manifest(manifest_path: str = '.well-known/aibdp.json'):
    """
    Create Flask route handler to serve AIBDP manifest.

    Args:
        manifest_path: Path to manifest file

    Returns:
        Route handler function
    """
    manifest_loader = AIBDPManifest(manifest_path)

    def handler():
        manifest = manifest_loader.manifest
        if not manifest:
            return jsonify({'error': 'Manifest not found'}), 404

        response = make_response(jsonify(manifest))
        response.headers['Content-Type'] = 'application/aibdp+json'
        response.headers['Cache-Control'] = 'public, max-age=3600'  # 1 hour
        response.headers['Access-Control-Allow-Origin'] = '*'
        return response

    return handler


def aibdp_required(
    manifest_path: str = '.well-known/aibdp.json',
    purpose: str = 'unknown'
):
    """
    Decorator to protect specific Flask routes with AIBDP enforcement.

    Args:
        manifest_path: Path to AIBDP manifest
        purpose: AI purpose to check against (training, indexing, etc.)

    Returns:
        Decorator function

    Example:
        @app.route('/article')
        @aibdp_required(purpose='training')
        def article():
            return 'Protected content'
    """
    manifest_loader = AIBDPManifest(manifest_path)

    def decorator(f):
        @wraps(f)
        def wrapper(*args, **kwargs):
            manifest = manifest_loader.manifest
            if not manifest:
                return f(*args, **kwargs)  # No manifest, allow through

            user_agent = request.headers.get('User-Agent', '')
            if not is_ai_user_agent(user_agent):
                return f(*args, **kwargs)  # Not AI, allow through

            policy = get_applicable_policy(manifest, purpose, request.path)
            if not policy:
                return f(*args, **kwargs)  # No policy, allow through

            if policy['status'] == 'refused':
                return create_430_response(manifest, policy, purpose)

            if policy['status'] == 'conditional':
                satisfied, _ = check_policy_conditions(policy, request.headers)
                if not satisfied:
                    return create_430_response(manifest, policy, purpose)

            return f(*args, **kwargs)

        return wrapper
    return decorator
