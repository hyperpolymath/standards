# Technical Demonstration Scripts

This directory contains interactive demonstration scripts for BoF presentations of the Palimpsest License framework.

## Available Demonstrations

### 1. DNS License Discovery (`demo-dns-discovery.sh`)

Demonstrates DNS-based license discovery mechanism including:
- Basic TXT record queries
- DNSSEC validation
- Multiple query methods (dig, host, nslookup)
- Caching behaviour and performance
- Integration with HTTP workflows
- Zone file configuration examples
- Comparison with alternative approaches

**Best for**: IETF, RIPE, NANOG, UKNOF (technical audiences)

### 2. HTTP Header Signalling (`demo-http-headers.sh`)

Demonstrates HTTP header-based consent signalling including:
- License header inspection
- Header field breakdown and semantics
- Web server configuration (nginx, Apache)
- CDN edge computing (Cloudflare Workers)
- AI scraper validation examples
- Performance impact measurement
- CDN caching behaviour

**Best for**: NANOG, UKNOF, IETF, platform operators

## Usage

### Prerequisites

Install required tools:

```bash
# Debian/Ubuntu
sudo apt-get install dnsutils curl

# macOS
brew install bind curl

# Fedora/RHEL
sudo dnf install bind-utils curl
```

### Running Demonstrations

Make scripts executable:

```bash
chmod +x demo-dns-discovery.sh
chmod +x demo-http-headers.sh
```

Run interactively (recommended for live presentations):

```bash
./demo-dns-discovery.sh
# Press Enter to advance through demonstrations

./demo-http-headers.sh
# Press Enter to advance through demonstrations
```

Run non-interactively (for pre-recording):

```bash
# Edit scripts to remove 'read -p' pauses
./demo-dns-discovery.sh | tee demo-output.txt
```

## Customisation for Live Demos

### Setting Up Test Infrastructure

Before the BoF session, set up actual test infrastructure:

1. **Register a test domain** (e.g., `palimpsest-demo.org`)

2. **Configure DNS records**:
   ```
   _license.palimpsest-demo.org. 3600 IN TXT "palimpsest-v0.4 https://palimpsest-demo.org/license"
   _license.palimpsest-demo.org. 3600 IN TXT "ai-consent=interpretive-only"
   ```

3. **Set up test web server** with license headers:
   ```nginx
   server {
       server_name palimpsest-demo.org;
       add_header X-License-URI "https://palimpsest-demo.org/license" always;
       add_header X-AI-Consent "interpretive-only" always;
   }
   ```

4. **Update scripts** to use your test domain instead of `example.com`

### Customising for Your Conference

Edit the scripts to:
- Replace `example.com` with your test domain
- Adjust timing (`sleep` commands) for your presentation pace
- Add or remove demonstrations based on time constraints
- Customize output formatting for readability on projector

### Example Customisation

```bash
# Original
command_demo "dig +short TXT _license.example.com"

# Customised for your domain
command_demo "dig +short TXT _license.palimpsest-demo.org"
```

## Screen Recording

For backup or remote presentations, record the demonstrations:

```bash
# Using asciinema (recommended)
asciinema rec dns-demo.cast
./demo-dns-discovery.sh
# Press Ctrl+D when done

# Play back recording
asciinema play dns-demo.cast

# Using script command
script -c "./demo-dns-discovery.sh" dns-demo.txt
```

## Presentation Tips

### Before the Session

- [ ] Test all demonstrations on conference WiFi (if possible)
- [ ] Have backup screenshots in case of network issues
- [ ] Increase terminal font size (18-24pt for projector)
- [ ] Use high-contrast colour scheme
- [ ] Prepare fallback slides showing expected output
- [ ] Test DNS propagation for your test domain (24h before)

### During the Session

- [ ] Open terminal in full screen
- [ ] Clear terminal before starting: `clear`
- [ ] Speak before typing (narrate what you're about to demonstrate)
- [ ] Pause after each output for audience to read
- [ ] Have a second terminal window ready for ad-hoc queries
- [ ] Keep water nearby (speaking while demoing is challenging!)

### Handling Issues

**DNS queries fail:**
- Have pre-captured output ready to show
- Explain what *should* happen
- Continue with next demonstration

**Network latency:**
- Acknowledge: "Conference WiFi latency demo!"
- Show cached results if possible
- Move to offline demonstrations (config examples)

**Unexpected output:**
- Stay calm, explain what happened
- Shows real-world operational challenges
- Great opportunity for discussion

## Terminal Setup for Presentations

### Font and Colours

```bash
# Increase font size (macOS Terminal)
# Preferences → Profiles → Font → Size: 18-24pt

# High-contrast colour scheme (optional)
# Preferences → Profiles → Colors → Choose high-contrast theme

# Linux (gnome-terminal)
gsettings set org.gnome.desktop.interface text-scaling-factor 1.5
```

### Clear Prompt

Use a simple, clear prompt for presentations:

```bash
# Temporary prompt for demo
export PS1='\[\033[01;34m\]$\[\033[00m\] '
```

### Window Management

```bash
# Full screen terminal (most systems)
# Press F11

# Zoom in (macOS)
# Cmd + Plus

# Zoom in (Linux)
# Ctrl + Plus
```

## Advanced Demonstrations

For more advanced sessions, add:

### 1. DNSSEC Chain Validation

```bash
# Show full DNSSEC validation chain
dig +dnssec +multiline _license.example.com TXT

# Verify signature
delv @8.8.8.8 _license.example.com TXT
```

### 2. Performance Benchmarking

```bash
# DNS query latency statistics
for i in {1..100}; do
    dig +short TXT _license.example.com > /dev/null
done

# HTTP header overhead measurement
ab -n 1000 -c 10 https://example.com/
```

### 3. CDN Header Propagation

```bash
# Test header propagation through CDN
curl -I https://cdn.example.com/ | grep -i x-license

# Compare origin vs edge
diff <(curl -sI https://origin.example.com/ | grep -i x-) \
     <(curl -sI https://cdn.example.com/ | grep -i x-)
```

## Troubleshooting

### DNS Queries Return NXDOMAIN

**Cause**: Domain not configured or DNS not yet propagated

**Solution**:
- Wait 24-48 hours for DNS propagation
- Check authoritative nameserver directly: `dig @ns1.example.com`
- Verify zone file syntax

### Headers Not Appearing

**Cause**: Web server configuration not applied

**Solution**:
- Verify config file syntax: `nginx -t`
- Reload web server: `sudo systemctl reload nginx`
- Check server logs: `sudo tail -f /var/log/nginx/error.log`
- Test locally first: `curl -I http://localhost/`

### Colour Output Not Working

**Cause**: Terminal doesn't support ANSI colours

**Solution**:
- Set `TERM` environment variable: `export TERM=xterm-256color`
- Edit scripts to remove colour codes
- Use plain output mode

## Post-Demo Follow-Up

After the session, provide attendees with:
- Link to demonstration scripts: `https://github.com/palimpsest-license/bof-meetings`
- Sample configuration files
- Documentation for setting up their own test infrastructure
- Contact information for technical support

## Feedback and Improvements

If you use these scripts at a BoF session:
- Document what worked well and what didn't
- Share timing information (was it too fast/slow?)
- Contribute improvements via pull request
- Share photos/videos of your session (with permission)

## Contact

**Questions about demonstrations**: demos@palimpsest-license.org
**Technical support**: technical@palimpsest-license.org
**Script contributions**: https://github.com/palimpsest-license

---

**Last Updated**: 22 November 2025
**Tested On**: Debian 12, Ubuntu 22.04, macOS 14, Fedora 39
