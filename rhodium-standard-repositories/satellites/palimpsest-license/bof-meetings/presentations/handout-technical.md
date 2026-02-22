# Palimpsest License Framework
## Technical Infrastructure for AI-Era Content Attribution

**Website**: https://palimpsest-license.org | **GitHub**: github.com/palimpsest-license | **Version**: 0.4

---

### The Problem

AI training systems scrape billions of creative works, stripping metadata and ignoring licensing terms. Current infrastructure lacks:
- **License discovery mechanisms** (DNS-based autodiscovery)
- **AI consent signalling** (interpretive vs. non-interpretive systems)
- **Metadata preservation** through transformation chains
- **Attribution tracking** for derivative works

### The Solution: Three Technical Layers

#### 1. DNS-Based License Discovery
```
_license.example.com. 3600 IN TXT "palimpsest-v0.4 https://example.com/license"
_license.example.com. 3600 IN TXT "ai-consent=interpretive-only"
```
- Automated license detection for scrapers and AI systems
- DNSSEC authentication for trust
- Cached queries: <1ms latency

#### 2. HTTP Header Extensions
```
X-License-URI: https://example.com/palimpsest.html
X-AI-Consent: non-interpretive-prohibited
X-Attribution-Chain: sha256:abc123def456...
X-Emotional-Lineage: protest-song; cultural-heritage
```
- Inline consent signalling (~200 bytes overhead)
- CDN-compatible (preserves through caching)
- Standards-track proposal for IETF

#### 3. Metadata Preservation
- **Images**: EXIF/IPTC retention through optimisation
- **HTML**: JSON-LD structured data
- **All formats**: Content-addressable attribution chains
- **Future-proof**: Post-quantum cryptographic signatures

### Implementation Examples

**nginx Configuration:**
```nginx
add_header X-License-URI "https://example.com/license.html" always;
add_header X-AI-Consent "non-interpretive-prohibited" always;
proxy_pass_header X-Attribution-Chain;
```

**Apache Configuration:**
```apache
Header always set X-License-URI "https://example.com/license.html"
Header always set X-AI-Consent "non-interpretive-prohibited"
```

**Cloudflare Worker:**
```javascript
addEventListener('fetch', event => {
  event.respondWith(handleRequest(event.request))
})

async function handleRequest(request) {
  const response = await fetch(request)
  const newResponse = new Response(response.body, response)
  newResponse.headers.set('X-License-URI', 'https://example.com/license')
  newResponse.headers.set('X-AI-Consent', 'interpretive-only')
  return newResponse
}
```

### Performance Impact

| Mechanism | Latency | Bandwidth Overhead | Cache Impact |
|-----------|---------|-------------------|--------------|
| HTTP headers | <1ms | ~200 bytes | Negligible |
| DNS TXT (uncached) | 5-50ms | ~150 bytes | Separate cache |
| DNS TXT (cached) | <1ms | ~150 bytes | High hit rate |
| Attribution chain | 2-10ms | ~500 bytes | Moderate |

### Regulatory Alignment

- **EU AI Act**: Training data documentation (Article 53)
- **DSA**: Content traceability (Article 14)
- **UK Online Safety Act**: Platform obligations
- **GDPR**: Privacy-by-design (Article 25)

### Key Features

✓ **Quantum-Proof**: Post-quantum cryptographic signatures
✓ **Distributed**: Non-cryptocurrency ledger integration
✓ **Backwards Compatible**: Graceful degradation
✓ **Open Standards**: IETF, W3C alignment
✓ **Privacy-Preserving**: Minimal personal data

### Deployment Paths

1. **Web servers**: nginx, Apache, Caddy configuration
2. **CDNs**: Cloudflare, Fastly, Akamai integration
3. **Platforms**: WordPress plugin, GitHub .license file
4. **DNS**: TXT record at authoritative nameserver

### Standardisation Efforts

- **IETF**: Proposed Working Group on Creative Content Attribution
- **W3C**: JSON-LD schema extensions (schema.org collaboration)
- **RIPE**: RDAP extension for licensing metadata
- **DNS-OARC**: DNS RRTYPE discussion

### Next Steps

1. **Pilot deployment** with 3-5 hosting providers
2. **IETF Internet-Draft** submission
3. **RIPE Atlas measurement** campaign
4. **Performance benchmarks** publication
5. **Open source tooling** (validation, monitoring)

### Get Involved

**Operators Mailing List**: operators@palimpsest-license.org
**Technical Specs**: https://palimpsest-license.org/specs
**Pilot Program**: https://palimpsest-license.org/pilot
**Standards Discussions**: GitHub Issues

### Contact

**Technical Lead**: Dr. Sarah Chen (s.chen@palimpsest-license.org)
**Infrastructure**: Marieke van der Berg (m.vandeberg@palimpsest-license.org)
**Security**: Elena Popescu (e.popescu@palimpsest-license.org)

---

*"Technical infrastructure for protecting creative work in the age of AI."*

**License**: This handout is CC-BY-SA 4.0 | **QR Code**: [Project website]
