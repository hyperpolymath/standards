# SPDX-License-Identifier: MIT OR GPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

"""
Example Flask server demonstrating AIBDP + HTTP 430 middleware

This example shows how to integrate consent-aware HTTP infrastructure
into a Flask application.

Usage:
    pip install -r requirements.txt
    python example_server.py

Test with:
    curl http://localhost:5000/
    curl http://localhost:5000/article -H "User-Agent: GPTBot/1.0"
    curl http://localhost:5000/.well-known/aibdp.json
"""

from flask import Flask, render_template_string
from aibdp_middleware import AIBDPMiddleware, serve_manifest, aibdp_required

app = Flask(__name__)

# Initialize AIBDP middleware
middleware = AIBDPMiddleware(
    app,
    manifest_path='example-aibdp.json',
    enforce_for_all=False,  # Only enforce for detected AI systems
    on_violation=lambda req, policy, purpose: print(
        f"[AIBDP] Blocked: {req.method} {req.path}\n"
        f"  User-Agent: {req.headers.get('User-Agent')}\n"
        f"  Purpose: {purpose}\n"
        f"  Policy: {policy['status']}"
    )
)

# Serve AIBDP manifest
app.route('/.well-known/aibdp.json')(serve_manifest('example-aibdp.json'))


# Routes
@app.route('/')
def index():
    return render_template_string('''
    <html>
      <head>
        <title>Consent-Aware HTTP Example (Python/Flask)</title>
      </head>
      <body>
        <h1>Welcome to Consent-Aware HTTP</h1>
        <p>This Flask server implements HTTP 430 + AIBDP.</p>

        <h2>Try these requests:</h2>
        <ul>
          <li><code>curl http://localhost:5000/</code> - Normal access (allowed)</li>
          <li><code>curl http://localhost:5000/article -H "User-Agent: GPTBot/1.0"</code> - AI bot (may be blocked)</li>
          <li><code>curl http://localhost:5000/.well-known/aibdp.json</code> - View AIBDP manifest</li>
        </ul>

        <h2>Resources:</h2>
        <ul>
          <li><a href="/article">Article</a> - Protected content</li>
          <li><a href="/public">Public content</a> - Allowed for all</li>
          <li><a href="/protected">Protected route (decorator)</a> - Using @aibdp_required</li>
          <li><a href="/.well-known/aibdp.json">AIBDP Manifest</a></li>
        </ul>
      </body>
    </html>
    ''')


@app.route('/article')
def article():
    return render_template_string('''
    <html>
      <head>
        <title>Protected Article</title>
      </head>
      <body>
        <h1>Protected Article</h1>
        <p>This content has AI usage boundaries declared via AIBDP.</p>
        <p><strong>Training:</strong> Conditional (requires attribution)</p>
        <p><strong>Generation:</strong> Refused</p>
        <p><strong>Indexing:</strong> Allowed</p>
      </body>
    </html>
    ''')


@app.route('/public')
def public():
    return render_template_string('''
    <html>
      <head>
        <title>Public Content</title>
      </head>
      <body>
        <h1>Public Content</h1>
        <p>This content is available for all purposes, including AI training.</p>
      </body>
    </html>
    ''')


@app.route('/protected')
@aibdp_required(purpose='training')
def protected():
    """
    Example of using @aibdp_required decorator for route-specific protection.
    """
    return render_template_string('''
    <html>
      <head>
        <title>Protected Route</title>
      </head>
      <body>
        <h1>Protected Route (Decorator)</h1>
        <p>This route uses the @aibdp_required decorator.</p>
        <p>AI training is not permitted on this content.</p>
      </body>
    </html>
    ''')


@app.route('/health')
def health():
    return {
        'status': 'healthy',
        'aibdp_enabled': True,
        'http_430_enabled': True
    }


@app.errorhandler(404)
def not_found(e):
    return 'Not Found', 404


@app.errorhandler(500)
def server_error(e):
    return 'Internal Server Error', 500


if __name__ == '__main__':
    print('🚀 Consent-Aware HTTP server (Python/Flask) running on http://localhost:5000')
    print('📄 AIBDP manifest: http://localhost:5000/.well-known/aibdp.json')
    print('🛡️  HTTP 430 enforcement: ENABLED')
    print('')
    print('Try these commands:')
    print('  curl http://localhost:5000/')
    print('  curl http://localhost:5000/article -H "User-Agent: GPTBot/1.0"')
    print('  curl http://localhost:5000/.well-known/aibdp.json')
    print('')

    app.run(debug=True, port=5000)
