# SPDX-License-Identifier: PMPL-1.0-or-later
# Cloudflare Setup Guide for avow-protocol.org

## Prerequisites

- Cloudflare account with avow-protocol.org domain
- GitHub repository: hyperpolymath/avow-protocol
- Cloudflare API credentials

## Step 1: DNS Configuration

1. Import DNS zone file:
   ```bash
   # Review the zone file
   cat cloudflare-dns-zone.txt

   # Apply via Cloudflare Dashboard or API
   ```

2. Update placeholder values:
   - Replace `192.0.2.1` with actual origin IP
   - Replace `2001:db8::1` with actual IPv6
   - Update `tunnel-id` with your Cloudflare Tunnel ID
   - Update SSHFP fingerprints with actual SSH keys
   - Update TLSA records with actual certificate hashes

## Step 2: GitHub Pages Setup

1. In GitHub repository settings:
   - Enable GitHub Pages from `main` branch
   - Set custom domain: `avow-protocol.org`
   - Wait for GitHub verification

2. Add GitHub verification TXT record:
   ```
   github-pages-challenge-hyperpolymath=avow-protocol-verification
   ```

## Step 3: Cloudflare Pages Setup

Alternative to GitHub Pages for better integration:

1. Connect repository to Cloudflare Pages:
   - Build command: `deno task build`
   - Output directory: `.` (root)
   - Environment variables: None required

2. Configure custom domain:
   - Add `avow-protocol.org` as custom domain
   - Enable automatic HTTPS

## Step 4: Security Headers

Add security headers via Cloudflare Transform Rules:

```http
Strict-Transport-Security: max-age=63072000; includeSubDomains; preload
X-Content-Type-Options: nosniff
X-Frame-Options: DENY
X-XSS-Protection: 1; mode=block
Content-Security-Policy: default-src 'self'; script-src 'self' 'unsafe-inline'; style-src 'self' 'unsafe-inline'
Referrer-Policy: strict-origin-when-cross-origin
Permissions-Policy: geolocation=(), microphone=(), camera=()
```

## Step 5: Zero Trust/SDP Configuration

For internal services (dashboard.internal, ci.internal, db.internal):

1. Create Cloudflare Tunnel:
   ```bash
   cloudflared tunnel create avow-protocol
   ```

2. Configure tunnel routes:
   ```yaml
   # config.yml
   tunnel: <tunnel-id>
   credentials-file: /path/to/credentials.json

   ingress:
     - hostname: dashboard.internal.avow-protocol.org
       service: http://localhost:3000
     - hostname: ci.internal.avow-protocol.org
       service: http://localhost:8080
     - hostname: db.internal.avow-protocol.org
       service: tcp://localhost:5432
     - service: http_status:404
   ```

3. Set up Cloudflare Access:
   - Create Access policy for internal subdomains
   - Configure authentication (SAML, GitHub OAuth, etc.)
   - Enforce MFA

## Step 6: WAF Rules

Configure Web Application Firewall rules:

1. Rate limiting:
   - API endpoints: 100 requests/minute per IP
   - General pages: 1000 requests/minute per IP

2. Custom rules:
   - Block known bad user agents
   - Require valid Referer for POST requests
   - Block requests with suspicious patterns

## Step 7: WASM Proxy

Deploy WASM-based request filtering:

1. Create Workers script:
   ```javascript
   // wasm-proxy.js
   export default {
     async fetch(request, env) {
       // Load AVOW verification WASM module
       const wasmModule = await WebAssembly.instantiate(env.AVOW_WASM);

       // Validate request using WASM
       const isValid = await wasmModule.exports.validateRequest(request);

       if (!isValid) {
         return new Response('Invalid request', { status: 403 });
       }

       return await fetch(request);
     }
   };
   ```

2. Deploy Worker:
   ```bash
   wrangler deploy
   ```

3. Configure route:
   - Route: `wasm.avow-protocol.org/*`
   - Worker: `avow-wasm-proxy`

## Step 8: Post-Quantum TLS

Cloudflare automatically supports post-quantum key exchange (Kyber) for TLS 1.3.

Verify configuration:
```bash
curl -I --tlsv1.3 https://avow-protocol.org
```

Look for: `CF-QUIC` and `Alt-Svc: h3=":443"` headers

## Step 9: Monitoring and Alerts

1. Enable Cloudflare Analytics
2. Set up alerts for:
   - High error rates (4xx, 5xx)
   - DDoS attacks
   - Certificate expiration
   - DNS changes

## Step 10: Testing

Test all security configurations:

```bash
# Test HTTPS redirect
curl -I http://avow-protocol.org

# Test security headers
curl -I https://avow-protocol.org

# Test DNSSEC
dig +dnssec avow-protocol.org

# Test CAA records
dig CAA avow-protocol.org

# Test TLSA records
dig TLSA _443._tcp.avow-protocol.org

# Test MTA-STS
dig TXT _mta-sts.avow-protocol.org
```

## Maintenance

- Update DNS zone serial on changes: `YYYYMMDDNN` format
- Rotate TLSA records 30 days before certificate renewal
- Review WAF rules quarterly
- Update security requirements as standards evolve

## References

- [Cloudflare DNS](https://developers.cloudflare.com/dns/)
- [Cloudflare Pages](https://developers.cloudflare.com/pages/)
- [Cloudflare Tunnel](https://developers.cloudflare.com/cloudflare-one/connections/connect-apps/)
- [Cloudflare Workers](https://developers.cloudflare.com/workers/)
- [Post-Quantum Cryptography](https://blog.cloudflare.com/post-quantum-tls/)

## Security Requirements

See `security-requirements.scm` for detailed cryptographic requirements including:
- Dilithium5-AES signatures
- Kyber-1024 key exchange
- SHAKE3-512 hashing
- XChaCha20-Poly1305 encryption
- Idris2 formal verification
