# AIBDP + HTTP 430 Middleware for Flask

Reference implementation of the AI Boundary Declaration Protocol (AIBDP) with HTTP 430 (Consent Required) enforcement for Python/Flask applications.

## Features

- **AIBDP Manifest Parsing**: Load and cache `.well-known/aibdp.json`
- **AI System Detection**: Identify AI user-agents (GPTBot, Claude-Web, etc.)
- **Policy Enforcement**: Block or allow based on declared boundaries
- **HTTP 430 Responses**: Standards-compliant consent violation responses
- **Path Scoping**: Glob-pattern matching for granular control
- **Conditional Policies**: Check for consent headers and conditions
- **Automatic Caching**: Manifest caching with configurable TTL
- **Type Hints**: Full type annotations for IDE support
- **Decorator Support**: `@aibdp_required` for route-specific protection

## Installation

```bash
pip install -r requirements.txt
```

## Quick Start

### Basic Usage

```python
from flask import Flask
from aibdp_middleware import AIBDPMiddleware, serve_manifest

app = Flask(__name__)

# Initialize AIBDP middleware
middleware = AIBDPMiddleware(
    app,
    manifest_path='.well-known/aibdp.json'
)

# Serve AIBDP manifest
app.route('/.well-known/aibdp.json')(serve_manifest())

# Your routes
@app.route('/')
def index():
    return 'Hello, consent-aware world!'

if __name__ == '__main__':
    app.run()
```

### Run Example Server

```bash
python example_server.py
```

Then test with:

```bash
# Normal browser access (allowed)
curl http://localhost:5000/

# AI bot access (may be blocked based on manifest)
curl http://localhost:5000/article -H "User-Agent: GPTBot/1.0"

# View AIBDP manifest
curl http://localhost:5000/.well-known/aibdp.json
```

## API Reference

### `AIBDPMiddleware`

Flask middleware class for AIBDP enforcement.

**Constructor:**

```python
AIBDPMiddleware(
    app=None,
    manifest_path: str = '.well-known/aibdp.json',
    enforce_for_all: bool = False,
    on_violation: Optional[Callable] = None
)
```

**Arguments:**

- `app` (Flask, optional): Flask application (can use `init_app` later)
- `manifest_path` (str): Path to AIBDP manifest file
- `enforce_for_all` (bool): Enforce for all requests, not just AI bots
- `on_violation` (callable): Callback when violation detected. Signature: `(request, policy, purpose) -> None`

**Example:**

```python
def log_violation(request, policy, purpose):
    print(f"Blocked {purpose} from {request.remote_addr}")

middleware = AIBDPMiddleware(
    app,
    manifest_path='./my-aibdp.json',
    enforce_for_all=False,
    on_violation=log_violation
)
```

### `serve_manifest(manifest_path)`

Create route handler to serve AIBDP manifest at `/.well-known/aibdp.json`.

**Arguments:**

- `manifest_path` (str): Path to manifest file

**Returns:** Flask route handler function

**Example:**

```python
app.route('/.well-known/aibdp.json')(serve_manifest('./aibdp.json'))
```

### `@aibdp_required` Decorator

Protect specific routes with AIBDP enforcement.

**Arguments:**

- `manifest_path` (str): Path to AIBDP manifest (default: `.well-known/aibdp.json`)
- `purpose` (str): AI purpose to check against (training, indexing, etc.)

**Example:**

```python
from aibdp_middleware import aibdp_required

@app.route('/article')
@aibdp_required(purpose='training')
def article():
    return 'Protected content'
```

This decorator will return HTTP 430 if AI training is refused or conditional requirements are not met.

### Utility Functions

#### `is_ai_user_agent(user_agent: str) -> bool`

Check if User-Agent indicates an AI system.

```python
from aibdp_middleware import is_ai_user_agent

if is_ai_user_agent('GPTBot/1.0'):
    print('AI system detected')
```

#### `extract_ai_purpose(headers: Dict[str, str]) -> str`

Extract AI purpose from request headers.

```python
from aibdp_middleware import extract_ai_purpose

purpose = extract_ai_purpose({
    'User-Agent': 'GPTBot/1.0',
    'AI-Purpose': 'training'
})
print(purpose)  # 'training'
```

#### `path_matches(request_path: str, pattern: str) -> bool`

Check if request path matches glob pattern.

```python
from aibdp_middleware import path_matches

path_matches('/docs/guide.html', '/docs/**')  # True
path_matches('/article.pdf', '*.pdf')  # True
path_matches('/blog/post.html', '/docs/**')  # False
```

#### `get_applicable_policy(manifest: Dict, purpose: str, request_path: str) -> Optional[Dict]`

Get applicable policy for request.

```python
from aibdp_middleware import get_applicable_policy

manifest = {...}  # Loaded AIBDP manifest
policy = get_applicable_policy(manifest, 'training', '/article.html')
```

## Manifest Format

Example `.well-known/aibdp.json`:

```json
{
  "aibdp_version": "0.2",
  "contact": "mailto:policy@example.org",
  "policies": {
    "training": {
      "status": "conditional",
      "conditions": ["Attribution required", "Non-commercial use only"],
      "scope": ["/articles/**"]
    },
    "indexing": {
      "status": "allowed",
      "scope": "all"
    },
    "generation": {
      "status": "refused",
      "rationale": "Content should not be synthetically replicated"
    }
  }
}
```

### Policy Status Values

- `allowed`: Usage permitted without conditions
- `refused`: Usage explicitly prohibited
- `conditional`: Usage permitted if conditions met
- `encouraged`: Usage actively encouraged

## HTTP 430 Response Format

When a policy is violated, the middleware responds with HTTP 430:

```http
HTTP/1.1 430 Consent Required
Content-Type: application/json
Link: <https://example.org/.well-known/aibdp.json>; rel="blocked-by-consent"
Retry-After: 86400

{
  "error": "AI usage boundaries declared in AIBDP manifest not satisfied",
  "manifest": "https://example.org/.well-known/aibdp.json",
  "violated_policy": "training",
  "policy_status": "refused",
  "required_conditions": [],
  "rationale": "Content should not be used for training",
  "contact": "mailto:policy@example.org"
}
```

## AI Consent Headers

AI systems can indicate compliance by sending:

```http
GET /article HTTP/1.1
Host: example.org
User-Agent: ResearchBot/1.0
AI-Purpose: indexing
AI-Consent-Reviewed: https://example.org/.well-known/aibdp.json
AI-Consent-Conditions: attribution,non-commercial
```

The middleware checks for these headers when enforcing conditional policies.

## Type Safety

This implementation includes full type hints for IDE support and type checking with mypy:

```bash
pip install mypy
mypy aibdp_middleware.py
```

## Testing

Create a test file `test_middleware.py`:

```python
from aibdp_middleware import is_ai_user_agent, path_matches, extract_ai_purpose

def test_ai_detection():
    assert is_ai_user_agent('GPTBot/1.0') == True
    assert is_ai_user_agent('Mozilla/5.0') == False

def test_path_matching():
    assert path_matches('/docs/guide.html', '/docs/**') == True
    assert path_matches('/blog/post.html', '/docs/**') == False

def test_purpose_extraction():
    headers = {'User-Agent': 'GPTBot/1.0'}
    assert extract_ai_purpose(headers) == 'training'

if __name__ == '__main__':
    test_ai_detection()
    test_path_matching()
    test_purpose_extraction()
    print('All tests passed!')
```

Run with:

```bash
python test_middleware.py
```

## Deployment Considerations

### Production Checklist

- ✅ Create AIBDP manifest at `.well-known/aibdp.json`
- ✅ Set appropriate `expires` field in manifest (30-90 days recommended)
- ✅ Provide contact information for policy questions
- ✅ Monitor logs for violations
- ✅ Set up manifest validation in CI/CD
- ✅ Use production WSGI server (gunicorn, uWSGI)
- ✅ Enable HTTPS for manifest integrity
- ✅ Document rationale in human-readable policy page

### Production Deployment

Use a production WSGI server:

```bash
pip install gunicorn
gunicorn -w 4 -b 0.0.0.0:5000 example_server:app
```

Or with uWSGI:

```bash
pip install uwsgi
uwsgi --http :5000 --wsgi-file example_server.py --callable app --processes 4
```

### Performance

- Manifest is cached in memory (default: 1 hour)
- Minimal latency impact for non-AI requests
- Failed manifest loads fail open (don't break site)
- Regex compilation is efficient

### Security

- Manifest served over HTTPS prevents tampering
- JSON parsing errors fail gracefully
- No sensitive information in manifest
- Type-safe implementation reduces bugs

## Standards Compliance

This implementation follows:

- [draft-jewell-aibdp-00](https://github.com/Hyperpolymath/consent-aware-http/blob/main/drafts/draft-jewell-aibdp-00.xml) - AIBDP specification
- [draft-jewell-http-430-consent-required-00](https://github.com/Hyperpolymath/consent-aware-http/blob/main/draft-jewell-http-430-consent-required-00.xml) - HTTP 430 status code
- [RFC 8615](https://www.rfc-editor.org/info/rfc8615) - Well-Known URIs
- [RFC 8259](https://www.rfc-editor.org/info/rfc8259) - JSON format

## License

MIT License - see LICENSE file for details

## Contributing

See [CONTRIBUTING.md](../../../.github/CONTRIBUTING.md) in the main repository.

## Support

- **Issues**: https://github.com/Hyperpolymath/consent-aware-http/issues
- **Discussions**: https://github.com/Hyperpolymath/consent-aware-http/discussions
- **Email**: jonathan@metadatastician.art

## Related Projects

- [AIBDP Specification](https://github.com/Hyperpolymath/consent-aware-http)
- [Node.js Implementation](../nodejs/)
- [Rust Implementation](../rust/)

---

_"Without refusal, permission is meaningless."_
