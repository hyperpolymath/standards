# Server Configuration Guides for HTTP 430 + AIBDP

Comprehensive guide for implementing consent-aware HTTP infrastructure across popular web servers and platforms.

## Table of Contents

- [nginx](#nginx)
- [Apache](#apache)
- [Caddy](#caddy)
- [Cloudflare Workers](#cloudflare-workers)
- [AWS CloudFront](#aws-cloudfront)
- [Vercel](#vercel)
- [Netlify](#netlify)
- [Testing Your Configuration](#testing-your-configuration)

---

## nginx

### Basic Configuration

Add AIBDP manifest serving and HTTP 430 enforcement to your nginx configuration.

#### Serve AIBDP Manifest

```nginx
# Serve .well-known/aibdp.json
location = /.well-known/aibdp.json {
    root /var/www/html;
    add_header Content-Type application/aibdp+json;
    add_header Cache-Control "public, max-age=3600";
    add_header Access-Control-Allow-Origin "*";
}
```

#### Block AI Bots Based on User-Agent

```nginx
# Map AI user-agents to a flag
map $http_user_agent $is_ai_bot {
    default 0;
    ~*GPTBot 1;
    ~*ChatGPT-User 1;
    ~*Claude-Web 1;
    ~*anthropic-ai 1;
    ~*Google-Extended 1;
    ~*CCBot 1;
    ~*PerplexityBot 1;
    ~*Diffbot 1;
}

# In your server block
server {
    listen 80;
    server_name example.org;

    # Block AI bots with HTTP 430
    if ($is_ai_bot = 1) {
        return 430;
    }

    # Serve AIBDP manifest
    location = /.well-known/aibdp.json {
        root /var/www/html;
        add_header Content-Type application/aibdp+json;
        add_header Cache-Control "public, max-age=3600";
    }

    # Your normal locations
    location / {
        root /var/www/html;
        index index.html;
    }
}
```

#### Custom HTTP 430 Error Page

```nginx
# Define custom error page for 430
error_page 430 /430.json;

location = /430.json {
    internal;
    default_type application/json;
    add_header Link '<https://example.org/.well-known/aibdp.json>; rel="blocked-by-consent"';
    add_header Retry-After 86400;
    return 430 '{
      "error": "AI usage boundaries declared in AIBDP manifest not satisfied",
      "manifest": "https://example.org/.well-known/aibdp.json",
      "contact": "mailto:policy@example.org"
    }';
}
```

#### Path-Specific Protection

```nginx
# Protect /articles/ from AI training bots
location /articles/ {
    if ($is_ai_bot = 1) {
        return 430;
    }
    root /var/www/html;
}

# Allow AI bots in /public/
location /public/ {
    root /var/www/html;
}
```

#### Complete nginx Example

```nginx
# /etc/nginx/nginx.conf or /etc/nginx/sites-available/example.org

# AI bot detection map
map $http_user_agent $is_ai_bot {
    default 0;
    ~*GPTBot 1;
    ~*ChatGPT-User 1;
    ~*Claude-Web 1;
    ~*anthropic-ai 1;
    ~*Google-Extended 1;
    ~*CCBot 1;
    ~*Googlebot-Extended 1;
    ~*PerplexityBot 1;
    ~*Diffbot 1;
    ~*Omgilibot 1;
}

server {
    listen 80;
    listen [::]:80;
    server_name example.org;

    root /var/www/html;
    index index.html;

    # Serve AIBDP manifest
    location = /.well-known/aibdp.json {
        add_header Content-Type application/aibdp+json;
        add_header Cache-Control "public, max-age=3600";
        add_header Access-Control-Allow-Origin "*";
    }

    # Custom 430 error page
    error_page 430 /430.json;
    location = /430.json {
        internal;
        default_type application/json;
        add_header Link '<https://example.org/.well-known/aibdp.json>; rel="blocked-by-consent"';
        add_header Retry-After 86400;
        return 430 '{
          "error": "AI usage boundaries declared in AIBDP manifest not satisfied",
          "manifest": "https://example.org/.well-known/aibdp.json",
          "violated_policy": "training",
          "policy_status": "refused",
          "contact": "mailto:policy@example.org"
        }';
    }

    # Protect articles from AI bots
    location /articles/ {
        if ($is_ai_bot = 1) {
            return 430;
        }
    }

    # Allow all access to public content
    location / {
        try_files $uri $uri/ =404;
    }
}
```

### Testing nginx Configuration

```bash
# Test configuration
sudo nginx -t

# Reload nginx
sudo nginx -s reload

# Test with curl
curl http://example.org/ -H "User-Agent: GPTBot/1.0"
```

---

## Apache

### Basic Configuration

Add AIBDP enforcement to Apache using `.htaccess` or VirtualHost configuration.

#### .htaccess Method

Create `/var/www/html/.htaccess`:

```apache
# Serve AIBDP manifest
<Files "aibdp.json">
    Header set Content-Type "application/aibdp+json"
    Header set Cache-Control "public, max-age=3600"
    Header set Access-Control-Allow-Origin "*"
</Files>

# Block AI bots with HTTP 430
RewriteEngine On
RewriteCond %{HTTP_USER_AGENT} (GPTBot|ChatGPT-User|Claude-Web|anthropic-ai|Google-Extended|CCBot|PerplexityBot|Diffbot) [NC]
RewriteRule ^ - [R=430,L]

# Custom 430 error document
ErrorDocument 430 /430.json
```

#### VirtualHost Configuration

```apache
# /etc/apache2/sites-available/example.org.conf

<VirtualHost *:80>
    ServerName example.org
    DocumentRoot /var/www/html

    # Enable rewrite engine
    RewriteEngine On

    # Serve AIBDP manifest
    <Location "/.well-known/aibdp.json">
        Header set Content-Type "application/aibdp+json"
        Header set Cache-Control "public, max-age=3600"
        Header set Access-Control-Allow-Origin "*"
    </Location>

    # Block AI bots
    RewriteCond %{HTTP_USER_AGENT} (GPTBot|ChatGPT-User|Claude-Web|anthropic-ai|Google-Extended|CCBot|PerplexityBot|Diffbot) [NC]
    RewriteRule ^/articles/ - [R=430,L]

    # Custom 430 error page
    ErrorDocument 430 /430.json

    <Directory /var/www/html>
        AllowOverride All
        Require all granted
    </Directory>
</VirtualHost>
```

#### Custom 430 Response

Create `/var/www/html/430.json`:

```json
{
  "error": "AI usage boundaries declared in AIBDP manifest not satisfied",
  "manifest": "https://example.org/.well-known/aibdp.json",
  "violated_policy": "training",
  "policy_status": "refused",
  "contact": "mailto:policy@example.org"
}
```

#### Path-Specific Protection

```apache
# Protect /articles/ only
<Location "/articles/">
    RewriteEngine On
    RewriteCond %{HTTP_USER_AGENT} (GPTBot|ChatGPT-User|Claude-Web) [NC]
    RewriteRule ^ - [R=430,L]
</Location>

# Allow AI bots in /public/
<Location "/public/">
    # No restrictions
</Location>
```

### Required Apache Modules

```bash
# Enable required modules
sudo a2enmod rewrite
sudo a2enmod headers

# Restart Apache
sudo systemctl restart apache2
```

---

## Caddy

Caddy has a simpler configuration syntax and built-in HTTPS.

### Caddyfile Configuration

```caddy
# /etc/caddy/Caddyfile

example.org {
    # Serve AIBDP manifest
    handle /.well-known/aibdp.json {
        header Content-Type application/aibdp+json
        header Cache-Control "public, max-age=3600"
        header Access-Control-Allow-Origin *
        root * /var/www/html
        file_server
    }

    # Block AI bots from /articles/
    @ai_bots {
        header_regexp User-Agent (GPTBot|ChatGPT-User|Claude-Web|anthropic-ai|Google-Extended|CCBot|PerplexityBot|Diffbot)
        path /articles/*
    }

    handle @ai_bots {
        header Link <https://example.org/.well-known/aibdp.json>; rel="blocked-by-consent"
        header Retry-After 86400
        respond `{
          "error": "AI usage boundaries declared in AIBDP manifest not satisfied",
          "manifest": "https://example.org/.well-known/aibdp.json",
          "violated_policy": "training",
          "policy_status": "refused",
          "contact": "mailto:policy@example.org"
        }` 430 {
            header Content-Type application/json
        }
    }

    # Default file server
    root * /var/www/html
    file_server
}
```

### Advanced Caddy Configuration

```caddy
example.org {
    # AIBDP manifest
    handle /.well-known/aibdp.json {
        header Content-Type application/aibdp+json
        header Cache-Control "public, max-age=3600"
        root * /var/www/html
        file_server
    }

    # AI bot matcher
    @ai_training {
        header_regexp User-Agent (GPTBot|Google-Extended|CCBot)
    }

    @ai_indexing {
        header_regexp User-Agent (Googlebot|Bingbot|DuckDuckBot)
    }

    # Block AI training bots everywhere except /public/
    @ai_training_blocked {
        header_regexp User-Agent (GPTBot|Google-Extended|CCBot)
        not path /public/*
    }

    handle @ai_training_blocked {
        respond `{"error": "Training refused"}` 430 {
            header Content-Type application/json
            header Link <https://example.org/.well-known/aibdp.json>; rel="blocked-by-consent"
        }
    }

    # Allow indexing bots (no 430)
    handle @ai_indexing {
        root * /var/www/html
        file_server
    }

    # Default
    root * /var/www/html
    file_server
}
```

### Reload Caddy

```bash
# Validate configuration
caddy validate --config /etc/caddy/Caddyfile

# Reload
caddy reload --config /etc/caddy/Caddyfile
```

---

## Cloudflare Workers

Deploy AIBDP enforcement at the edge with Cloudflare Workers.

### Worker Script

```javascript
// worker.js - Deploy as Cloudflare Worker

const AIBDP_MANIFEST_URL = 'https://example.org/.well-known/aibdp.json';

const AI_USER_AGENTS = [
  /GPTBot/i,
  /ChatGPT-User/i,
  /Claude-Web/i,
  /anthropic-ai/i,
  /Google-Extended/i,
  /CCBot/i,
  /PerplexityBot/i,
  /Diffbot/i,
];

function isAIBot(userAgent) {
  return AI_USER_AGENTS.some(pattern => pattern.test(userAgent));
}

function create430Response() {
  const body = {
    error: 'AI usage boundaries declared in AIBDP manifest not satisfied',
    manifest: AIBDP_MANIFEST_URL,
    violated_policy: 'training',
    policy_status: 'refused',
    contact: 'mailto:policy@example.org'
  };

  return new Response(JSON.stringify(body), {
    status: 430,
    headers: {
      'Content-Type': 'application/json',
      'Link': `<${AIBDP_MANIFEST_URL}>; rel="blocked-by-consent"`,
      'Retry-After': '86400'
    }
  });
}

addEventListener('fetch', event => {
  event.respondWith(handleRequest(event.request));
});

async function handleRequest(request) {
  const url = new URL(request.url);
  const userAgent = request.headers.get('User-Agent') || '';

  // Serve AIBDP manifest
  if (url.pathname === '/.well-known/aibdp.json') {
    return fetch(request);
  }

  // Detect AI bots
  if (isAIBot(userAgent)) {
    // Block from /articles/ path
    if (url.pathname.startsWith('/articles/')) {
      return create430Response();
    }
  }

  // Pass through to origin
  return fetch(request);
}
```

### Deploy Worker

```bash
# Install Wrangler CLI
npm install -g wrangler

# Login to Cloudflare
wrangler login

# Initialize project
wrangler init aibdp-worker

# Deploy
wrangler publish
```

### wrangler.toml Configuration

```toml
name = "aibdp-worker"
type = "javascript"
account_id = "your-account-id"
workers_dev = true
route = "example.org/*"
zone_id = "your-zone-id"

[build]
command = ""

[build.upload]
format = "service-worker"
```

---

## AWS CloudFront

Use Lambda@Edge to enforce AIBDP at CloudFront distribution.

### Lambda@Edge Function

```javascript
// index.js - Deploy as Lambda@Edge (Viewer Request)

const AI_USER_AGENTS = [
  /GPTBot/i,
  /ChatGPT-User/i,
  /Claude-Web/i,
  /anthropic-ai/i,
  /Google-Extended/i,
];

exports.handler = async (event) => {
  const request = event.Records[0].cf.request;
  const headers = request.headers;
  const uri = request.uri;

  // Serve AIBDP manifest
  if (uri === '/.well-known/aibdp.json') {
    return request;
  }

  // Check User-Agent
  const userAgent = headers['user-agent'] ? headers['user-agent'][0].value : '';

  // Detect AI bots
  const isAIBot = AI_USER_AGENTS.some(pattern => pattern.test(userAgent));

  if (isAIBot && uri.startsWith('/articles/')) {
    const response = {
      status: '430',
      statusDescription: 'Consent Required',
      headers: {
        'content-type': [{
          key: 'Content-Type',
          value: 'application/json'
        }],
        'link': [{
          key: 'Link',
          value: '<https://example.org/.well-known/aibdp.json>; rel="blocked-by-consent"'
        }],
        'retry-after': [{
          key: 'Retry-After',
          value: '86400'
        }]
      },
      body: JSON.stringify({
        error: 'AI usage boundaries declared in AIBDP manifest not satisfied',
        manifest: 'https://example.org/.well-known/aibdp.json',
        violated_policy: 'training',
        policy_status: 'refused'
      })
    };

    return response;
  }

  return request;
};
```

### Deploy Lambda@Edge

```bash
# Package function
zip function.zip index.js

# Create Lambda function (us-east-1 required for Lambda@Edge)
aws lambda create-function \
  --region us-east-1 \
  --function-name aibdp-enforcement \
  --runtime nodejs18.x \
  --role arn:aws:iam::ACCOUNT:role/lambda-edge-role \
  --handler index.handler \
  --zip-file fileb://function.zip \
  --publish

# Associate with CloudFront distribution
aws cloudfront update-distribution \
  --id DISTRIBUTION_ID \
  --distribution-config file://distribution-config.json
```

---

## Vercel

Use Vercel Edge Middleware for AIBDP enforcement.

### middleware.ts

```typescript
// middleware.ts in your Next.js project

import { NextRequest, NextResponse } from 'next/server';

const AI_USER_AGENTS = [
  /GPTBot/i,
  /ChatGPT-User/i,
  /Claude-Web/i,
  /anthropic-ai/i,
  /Google-Extended/i,
];

export function middleware(request: NextRequest) {
  const userAgent = request.headers.get('user-agent') || '';
  const pathname = request.nextUrl.pathname;

  // Serve AIBDP manifest
  if (pathname === '/.well-known/aibdp.json') {
    return NextResponse.next();
  }

  // Detect AI bots
  const isAIBot = AI_USER_AGENTS.some(pattern => pattern.test(userAgent));

  if (isAIBot && pathname.startsWith('/articles')) {
    return new NextResponse(
      JSON.stringify({
        error: 'AI usage boundaries declared in AIBDP manifest not satisfied',
        manifest: 'https://example.org/.well-known/aibdp.json',
        violated_policy: 'training',
        policy_status: 'refused'
      }),
      {
        status: 430,
        headers: {
          'Content-Type': 'application/json',
          'Link': '<https://example.org/.well-known/aibdp.json>; rel="blocked-by-consent"',
          'Retry-After': '86400'
        }
      }
    );
  }

  return NextResponse.next();
}

export const config = {
  matcher: [
    '/((?!api|_next/static|_next/image|favicon.ico).*)',
  ],
};
```

### Deploy to Vercel

```bash
# Ensure middleware.ts is in project root
vercel deploy --prod
```

---

## Netlify

Use Netlify Edge Functions for AIBDP enforcement.

### netlify/edge-functions/aibdp.ts

```typescript
import type { Context } from "https://edge.netlify.com";

const AI_USER_AGENTS = [
  /GPTBot/i,
  /ChatGPT-User/i,
  /Claude-Web/i,
  /anthropic-ai/i,
  /Google-Extended/i,
];

export default async (request: Request, context: Context) => {
  const url = new URL(request.url);
  const userAgent = request.headers.get('user-agent') || '';

  // Serve AIBDP manifest
  if (url.pathname === '/.well-known/aibdp.json') {
    return context.next();
  }

  // Detect AI bots
  const isAIBot = AI_USER_AGENTS.some(pattern => pattern.test(userAgent));

  if (isAIBot && url.pathname.startsWith('/articles')) {
    return new Response(
      JSON.stringify({
        error: 'AI usage boundaries declared in AIBDP manifest not satisfied',
        manifest: 'https://example.org/.well-known/aibdp.json',
        violated_policy: 'training',
        policy_status: 'refused'
      }),
      {
        status: 430,
        headers: {
          'Content-Type': 'application/json',
          'Link': '<https://example.org/.well-known/aibdp.json>; rel="blocked-by-consent"',
          'Retry-After': '86400'
        }
      }
    );
  }

  return context.next();
};

export const config = { path: "/*" };
```

### Deploy to Netlify

```bash
# Ensure netlify/edge-functions/aibdp.ts exists
netlify deploy --prod
```

---

## Testing Your Configuration

### Manual Testing with curl

```bash
# Test normal access
curl -I http://example.org/

# Test AI bot detection
curl -I http://example.org/articles/test.html -H "User-Agent: GPTBot/1.0"

# Should return HTTP 430
# HTTP/1.1 430 Consent Required
# Link: <https://example.org/.well-known/aibdp.json>; rel="blocked-by-consent"
# Retry-After: 86400

# Verify AIBDP manifest is served
curl http://example.org/.well-known/aibdp.json
```

### Automated Testing Script

```bash
#!/bin/bash
# test-aibdp.sh

DOMAIN="https://example.org"

echo "Testing AIBDP implementation..."

# Test 1: Manifest accessibility
echo "1. Checking AIBDP manifest..."
STATUS=$(curl -s -o /dev/null -w "%{http_code}" "$DOMAIN/.well-known/aibdp.json")
if [ "$STATUS" -eq 200 ]; then
    echo "✅ AIBDP manifest accessible (HTTP 200)"
else
    echo "❌ AIBDP manifest NOT accessible (HTTP $STATUS)"
fi

# Test 2: AI bot blocking
echo "2. Testing AI bot blocking..."
STATUS=$(curl -s -o /dev/null -w "%{http_code}" "$DOMAIN/articles/" -H "User-Agent: GPTBot/1.0")
if [ "$STATUS" -eq 430 ]; then
    echo "✅ AI bot blocked correctly (HTTP 430)"
else
    echo "❌ AI bot NOT blocked (HTTP $STATUS)"
fi

# Test 3: Normal access
echo "3. Testing normal access..."
STATUS=$(curl -s -o /dev/null -w "%{http_code}" "$DOMAIN/" -H "User-Agent: Mozilla/5.0")
if [ "$STATUS" -eq 200 ]; then
    echo "✅ Normal access works (HTTP 200)"
else
    echo "❌ Normal access blocked (HTTP $STATUS)"
fi

echo "Testing complete!"
```

Run with:

```bash
chmod +x test-aibdp.sh
./test-aibdp.sh
```

---

## Production Checklist

- ✅ AIBDP manifest exists at `/.well-known/aibdp.json`
- ✅ Manifest has correct `Content-Type: application/aibdp+json`
- ✅ AI User-Agent detection configured
- ✅ HTTP 430 responses include `Link` header to manifest
- ✅ Custom 430 error page provides clear information
- ✅ Path-specific rules match AIBDP manifest policies
- ✅ HTTPS enabled (recommended for manifest integrity)
- ✅ Testing completed for AI bots and normal users
- ✅ Monitoring/logging configured for 430 responses
- ✅ Contact information in manifest is monitored

---

## Troubleshooting

### Issue: 430 responses not working

**Check:**
- User-Agent regex patterns are correct (case-insensitive)
- Server configuration is loaded (restart/reload required)
- Custom status code 430 is recognized by server

### Issue: AIBDP manifest not accessible

**Check:**
- File exists at `.well-known/aibdp.json` in document root
- File permissions allow web server to read (644 or 755)
- `.well-known` directory exists and is not blocked

### Issue: Normal users getting blocked

**Check:**
- User-Agent detection is not too broad
- Path matching is correct
- No accidental wildcard matches

---

## Standards Compliance

These configurations implement:

- [draft-jewell-aibdp-00](https://github.com/Hyperpolymath/consent-aware-http) - AIBDP specification
- [draft-jewell-http-430-consent-required-00](https://github.com/Hyperpolymath/consent-aware-http) - HTTP 430 status code
- [RFC 8615](https://www.rfc-editor.org/info/rfc8615) - Well-Known URIs

---

_"Without refusal, permission is meaningless."_
