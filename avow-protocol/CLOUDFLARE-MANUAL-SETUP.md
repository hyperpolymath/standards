# SPDX-License-Identifier: PMPL-1.0-or-later
# Cloudflare Manual Setup - Detailed Step-by-Step Instructions

Complete visual guide for deploying AVOW Protocol to Cloudflare via dashboard.

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Part 1: Cloudflare Pages Setup](#part-1-cloudflare-pages-setup)
3. [Part 2: DNS Configuration](#part-2-dns-configuration)
4. [Part 3: Security Configuration](#part-3-security-configuration)
5. [Part 4: Zero Trust Setup](#part-4-zero-trust-setup)
6. [Part 5: Testing & Verification](#part-5-testing--verification)

---

## Prerequisites

- [ ] Cloudflare account (free or paid)
- [ ] Domain `avow-protocol.org` added to Cloudflare
- [ ] GitHub repository `hyperpolymath/avow-protocol` accessible
- [ ] Repository pushed to main branch

---

## Part 1: Cloudflare Pages Setup

### Step 1.1: Access Cloudflare Pages

1. Log in to [Cloudflare Dashboard](https://dash.cloudflare.com)
2. Click on your account in the top left
3. In the left sidebar, click **"Workers & Pages"**
4. Click **"Create application"** button
5. Select **"Pages"** tab
6. Click **"Connect to Git"**

### Step 1.2: Connect GitHub Repository

1. Click **"Connect GitHub"** (or "Connect another repository" if already connected)
2. Authorize Cloudflare to access your repositories
3. In the repository list, find and select: **`hyperpolymath/avow-protocol`**
4. Click **"Begin setup"**

### Step 1.3: Configure Build Settings

On the "Set up builds and deployments" page:

**Framework preset:** None (or Custom)

**Build configuration:**
- Build command: `deno task build`
- Build output directory: `.` (just a dot)
- Root directory: `/` (leave as default)

**Environment variables:** (click "Add variable" if needed)
- None required for initial deployment

**Branch deployment:**
- Production branch: `main`
- Preview branch: Leave enabled for pull requests

Click **"Save and Deploy"**

### Step 1.4: Wait for Initial Build

- Watch the build logs in real-time
- Initial build takes 2-5 minutes
- Look for: `✅ Success! Deployed to https://avow-protocol-xxx.pages.dev`
- Copy the `.pages.dev` URL for testing

**If build fails:**
- Check that all dependencies are in `package.json`
- Verify `deno.json` has correct build task
- Check build logs for specific errors

### Step 1.5: Verify Initial Deployment

1. Click the `.pages.dev` URL from build logs
2. Verify the site loads correctly
3. Check browser console for any errors (F12 → Console)
4. Test navigation between pages

---

## Part 2: DNS Configuration

### Step 2.1: Access DNS Settings

1. In Cloudflare Dashboard, click on **`avow-protocol.org`** domain
2. Click **"DNS"** in the left sidebar
3. You'll see the DNS records table

### Step 2.2: Import DNS Zone (Recommended)

**Option A: Bulk Import**

1. Click **"Advanced"** at top right
2. Click **"Import and export"**
3. Click **"Import DNS records"**
4. Copy contents from `cloudflare-dns-zone.txt`
5. Paste into the text box
6. Click **"Preview records"**
7. Review the records to be imported
8. Click **"Import"**

**Option B: Manual Entry** (if import fails)

Add these critical records manually:

#### Root Domain Records

| Type | Name | Content | Proxy | TTL |
|------|------|---------|-------|-----|
| A | @ | 192.0.2.1 | ✅ Proxied | Auto |
| AAAA | @ | 2001:db8::1 | ✅ Proxied | Auto |
| A | www | 192.0.2.1 | ✅ Proxied | Auto |
| AAAA | www | 2001:db8::1 | ✅ Proxied | Auto |

**⚠️ Update IPs:** Replace `192.0.2.1` and `2001:db8::1` with your actual IPs (or leave if using Cloudflare Pages)

#### GitHub Pages CNAME (if using GitHub Pages)

| Type | Name | Content | Proxy | TTL |
|------|------|---------|-------|-----|
| CNAME | gh-pages | hyperpolymath.github.io | ❌ DNS only | Auto |

#### Security Records

| Type | Name | Content |
|------|------|---------|
| CAA | @ | `0 issue "letsencrypt.org"` |
| CAA | @ | `0 issue "digicert.com"` |
| CAA | @ | `0 iodef "mailto:jonathan.jewell@open.ac.uk"` |
| TXT | @ | `v=spf1 include:_spf.github.com ~all` |
| TXT | @ | `github-pages-challenge-hyperpolymath=avow-protocol-verification` |

#### Mail Security (Optional)

| Type | Name | Content |
|------|------|---------|
| TXT | _dmarc | `v=DMARC1; p=reject; rua=mailto:jonathan.jewell@open.ac.uk` |
| MX | @ | `10 mail.avow-protocol.org` |

### Step 2.3: Configure Custom Domain for Pages

1. Go back to **Workers & Pages** → **avow-protocol** project
2. Click **"Custom domains"** tab
3. Click **"Set up a custom domain"**
4. Enter: `avow-protocol.org`
5. Click **"Continue"**
6. Cloudflare will automatically create DNS records
7. Wait for "Active" status (1-5 minutes)
8. Repeat for `www.avow-protocol.org`

### Step 2.4: Enable DNSSEC

1. In domain settings, click **"DNS"**
2. Scroll to **"DNSSEC"** section
3. Click **"Enable DNSSEC"**
4. Copy the DS record information
5. Add DS record to your domain registrar (if required)

---

## Part 3: Security Configuration

### Step 3.1: Configure Security Headers

1. In domain overview, click **"Rules"** in left sidebar
2. Click **"Transform Rules"**
3. Click **"Create rule"**
4. Rule name: `AVOW Security Headers`

**When incoming requests match:**
- Select: "All incoming requests"

**Then:**
- Click "Set static" → "Response header"
- Add these headers one by one:

| Header | Value |
|--------|-------|
| Strict-Transport-Security | `max-age=63072000; includeSubDomains; preload` |
| X-Content-Type-Options | `nosniff` |
| X-Frame-Options | `DENY` |
| X-XSS-Protection | `1; mode=block` |
| Content-Security-Policy | `default-src 'self'; script-src 'self' 'unsafe-inline'; style-src 'self' 'unsafe-inline'` |
| Referrer-Policy | `strict-origin-when-cross-origin` |
| Permissions-Policy | `geolocation=(), microphone=(), camera=()` |

5. Click **"Deploy"**

### Step 3.2: Configure WAF Rules

1. In left sidebar, click **"Security"** → **"WAF"**
2. Click **"Create rule"**

**Rate Limiting - API Endpoints:**
- Rule name: `API Rate Limit`
- If: `URI Path contains "/api/"`
- Then: Rate limit
- Requests: 100 per minute
- Action: Block
- Duration: 600 seconds

**Rate Limiting - General:**
- Rule name: `General Rate Limit`
- If: `All incoming requests`
- Then: Rate limit
- Requests: 1000 per minute
- Action: Challenge (CAPTCHA)

3. Click **"Deploy"**

### Step 3.3: Enable SSL/TLS

1. Click **"SSL/TLS"** in left sidebar
2. Set encryption mode to: **"Full (strict)"**
3. Click **"Edge Certificates"**
4. Enable:
   - ✅ Always Use HTTPS
   - ✅ HTTP Strict Transport Security (HSTS)
   - ✅ Minimum TLS Version: TLS 1.3
   - ✅ TLS 1.3 (Post-Quantum)
   - ✅ Automatic HTTPS Rewrites

### Step 3.4: Configure Firewall Rules

1. Go to **Security** → **WAF**
2. Click **"Custom rules"** tab
3. Add these rules:

**Block Bad Bots:**
```
(cf.client.bot) and not (cf.verified_bot_category in {"Search Engine Crawler" "Monitoring & Analytics"})
Action: Block
```

**Require Valid Referer for POST:**
```
(http.request.method eq "POST") and not (http.referer contains "avow-protocol.org")
Action: Block
```

---

## Part 4: Zero Trust Setup

### Step 4.1: Create Cloudflare Tunnel

1. In Cloudflare Dashboard, go to **Zero Trust**
2. Click **"Networks"** → **"Tunnels"**
3. Click **"Create a tunnel"**
4. Tunnel name: `avow-protocol-tunnel`
5. Click **"Save tunnel"**
6. Copy the tunnel token (you'll need this)

### Step 4.2: Install cloudflared

On your server:

```bash
# Install cloudflared
curl -L https://github.com/cloudflare/cloudflared/releases/latest/download/cloudflared-linux-amd64 -o cloudflared
chmod +x cloudflared
sudo mv cloudflared /usr/local/bin/

# Authenticate
cloudflared tunnel login

# Create tunnel
cloudflared tunnel create avow-protocol

# Configure tunnel
cat > ~/.cloudflared/config.yml <<EOF
tunnel: <your-tunnel-id>
credentials-file: /path/to/credentials.json

ingress:
  - hostname: dashboard.internal.avow-protocol.org
    service: http://localhost:3000
  - hostname: ci.internal.avow-protocol.org
    service: http://localhost:8080
  - service: http_status:404
EOF

# Run tunnel
cloudflared tunnel run avow-protocol
```

### Step 4.3: Add DNS Records for Tunnel

In Cloudflare DNS, add:

| Type | Name | Content |
|------|------|---------|
| CNAME | dashboard.internal | `<tunnel-id>.cfargotunnel.com` |
| CNAME | ci.internal | `<tunnel-id>.cfargotunnel.com` |

### Step 4.4: Configure Access Policies

1. Go to **Zero Trust** → **Access** → **Applications**
2. Click **"Add an application"**
3. Select **"Self-hosted"**
4. Application name: `AVOW Internal Dashboard`
5. Session duration: 24 hours
6. Application domain: `dashboard.internal.avow-protocol.org`
7. Click **"Next"**

**Add policy:**
- Policy name: `AVOW Team Access`
- Action: Allow
- Include: Emails: `jonathan.jewell@open.ac.uk`
- Click **"Next"** → **"Add application"**

---

## Part 5: Testing & Verification

### Step 5.1: Test HTTPS and SSL

```bash
# Test SSL certificate
curl -I https://avow-protocol.org

# Check for HSTS header
curl -I https://avow-protocol.org | grep -i strict

# Test HTTP → HTTPS redirect
curl -I http://avow-protocol.org

# Test TLS 1.3
openssl s_client -connect avow-protocol.org:443 -tls1_3
```

### Step 5.2: Test Security Headers

```bash
# Check all security headers
curl -I https://avow-protocol.org | grep -E "(X-|Content-Security|Strict-Transport)"
```

Expected headers:
- ✅ Strict-Transport-Security
- ✅ X-Content-Type-Options: nosniff
- ✅ X-Frame-Options: DENY
- ✅ Content-Security-Policy

### Step 5.3: Test DNS Records

```bash
# Test DNSSEC
dig +dnssec avow-protocol.org

# Test CAA records
dig CAA avow-protocol.org

# Test SPF
dig TXT avow-protocol.org | grep spf
```

### Step 5.4: Test Performance

1. **Lighthouse Audit:**
   - Open Chrome DevTools (F12)
   - Click "Lighthouse" tab
   - Click "Generate report"
   - Target scores: 95+ for all categories

2. **WebPageTest:**
   - Go to https://www.webpagetest.org
   - Enter: `https://avow-protocol.org`
   - Run test
   - Check CDN performance

3. **SSL Labs:**
   - Go to https://www.ssllabs.com/ssltest/
   - Enter: `avow-protocol.org`
   - Target grade: A+

### Step 5.5: Functional Testing

Manual testing checklist:

- [ ] Home page loads correctly
- [ ] All navigation links work
- [ ] Interactive demo functions
- [ ] Forms submit properly
- [ ] No console errors (F12 → Console)
- [ ] Mobile responsive (F12 → Device toolbar)
- [ ] All assets load (images, CSS, JS)
- [ ] No mixed content warnings

---

## Troubleshooting

### DNS Not Propagating

```bash
# Check current DNS
dig avow-protocol.org

# Flush local DNS cache (Linux)
sudo systemctl restart systemd-resolved

# Or (macOS)
sudo dscacheutil -flushcache
```

### SSL Certificate Issues

1. Check Cloudflare SSL mode: must be "Full (strict)"
2. Verify origin certificate on server
3. Wait 5-10 minutes for certificate provisioning
4. Clear browser cache and cookies

### Build Failures

1. Check build logs in Pages dashboard
2. Verify `deno.json` build command
3. Test build locally: `deno task build`
4. Check for missing dependencies in `package.json`

### Rate Limiting Too Aggressive

1. Go to Security → WAF → Custom rules
2. Adjust rate limits:
   - Increase requests per minute
   - Change action to "Challenge" instead of "Block"

---

## Post-Deployment Checklist

- [ ] Custom domain configured and active
- [ ] DNS records imported and verified
- [ ] DNSSEC enabled
- [ ] SSL/TLS set to Full (strict)
- [ ] Security headers configured
- [ ] WAF rules deployed
- [ ] Rate limiting configured
- [ ] Zero Trust tunnel created (optional)
- [ ] Access policies configured (optional)
- [ ] All tests passing
- [ ] Lighthouse score 95+
- [ ] SSL Labs grade A+
- [ ] No console errors
- [ ] Mobile responsive verified

---

## Monitoring & Maintenance

### Daily

- Check Cloudflare Analytics for traffic patterns
- Review Security Events for blocked requests
- Monitor error rates in Real-time logs

### Weekly

- Review WAF rule effectiveness
- Check certificate expiration dates (auto-renewed)
- Update DNS records if IPs changed

### Monthly

- Run Lighthouse audit
- Run SSL Labs test
- Update security headers if needed
- Review Zero Trust policies

---

## Support

**Documentation:**
- Cloudflare Pages: https://developers.cloudflare.com/pages/
- Cloudflare DNS: https://developers.cloudflare.com/dns/
- Cloudflare Tunnel: https://developers.cloudflare.com/cloudflare-one/

**Contact:**
- Jonathan D.A. Jewell: jonathan.jewell@open.ac.uk
- GitHub Issues: https://github.com/hyperpolymath/avow-protocol/issues

---

## Complete! 🎉

Your AVOW Protocol site is now deployed with:
- ✅ Global CDN (Cloudflare)
- ✅ Post-quantum TLS 1.3
- ✅ DNSSEC enabled
- ✅ Security headers configured
- ✅ WAF protection active
- ✅ Rate limiting enabled
- ✅ Zero Trust ready (optional)

Visit: https://avow-protocol.org
