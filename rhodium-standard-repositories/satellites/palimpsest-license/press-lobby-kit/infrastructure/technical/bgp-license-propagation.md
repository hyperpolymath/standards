# BGP and License Propagation
## Technical Advisory Document

**Version:** 1.0
**Date:** 2025-11-22
**Status:** Draft for Discussion
**Audience:** Network Engineers, ISP Technical Staff, BGP Operators

---

## Executive Summary

This document proposes mechanisms for propagating creative licensing metadata through the Border Gateway Protocol (BGP) infrastructure, enabling network-level awareness of content licensing requirements. By extending existing BGP community and extended community mechanisms, Internet Service Providers (ISPs) can participate in protecting creative works at the infrastructure layer.

## Background

### The Challenge

Creative content traverses the Internet without licensing metadata attached at the network layer. This creates:

- **Metadata loss:** Licensing information stripped during content delivery
- **Attribution failure:** No mechanism for ISPs to preserve creator rights
- **Enforcement gaps:** Content filtering/caching without license awareness
- **AI training exposure:** Network-level access without consent tracking

### The Opportunity

BGP already propagates routing policy through communities and extended communities. These same mechanisms can signal licensing requirements to network operators.

## Technical Approach

### 1. BGP Communities for License Signalling

#### Standard Communities

We propose a well-known BGP community structure:

```
Format: ASN:Value
Example: 64512:1000 (Palimpsest License Protected)
```

**Proposed Community Values:**

| Community | Meaning | Action Required |
|-----------|---------|----------------|
| ASN:1000 | Palimpsest-licensed content | Preserve metadata in caching |
| ASN:1001 | Non-interpretive consent required | Block AI training scraping |
| ASN:1002 | Emotional lineage protected | Honour attribution requirements |
| ASN:1003 | Quantum-proof traceability | Maintain cryptographic chains |
| ASN:1010 | License metadata available via DNS | Query TXT records for details |

#### Extended Communities

For richer metadata, use extended communities (RFC 4360):

```
Type: Opaque Extended Community (0x03)
Format: [Type][Subtype][6-byte value]

Example:
0x03:0x01:PALIMPSEST_V04
```

**Encoding Structure:**

```
Byte 0-1:  Type and Subtype (0x0301 = License Info)
Byte 2-3:  License Version (0x0004 = v0.4)
Byte 4:    Flags (NI-consent, DAO-governed, etc.)
Byte 5-7:  Reserved for future use
```

### 2. Route Object Annotations

Extend RPSL (Routing Policy Specification Language) objects:

```rpsl
route: 192.0.2.0/24
origin: AS64512
descr: Palimpsest-licensed creative content
remarks: license-type: palimpsest-v0.4
remarks: license-url: https://example.org/.well-known/license
remarks: ni-consent-required: true
remarks: metadata-preservation: mandatory
mnt-by: EXAMPLE-MNT
source: RIPE
```

### 3. BGP Path Attributes

Leverage BGP optional transitive attributes to carry licensing metadata:

```
Attribute Type: Optional Transitive (0xC0)
Attribute Code: TBD (requires IANA assignment)
Format: Type-Length-Value (TLV)

TLV Structure:
Type:   License-Info (proposed code: 250)
Length: Variable
Value:  [License-Type][Version][Flags][URL-Length][URL]
```

### 4. Implementation Strategy

#### Phase 1: Voluntary Adoption
- ISPs opt-in to community-based signalling
- Communities attached to routes serving licensed content
- No protocol changes required

#### Phase 2: Extended Communities
- Richer metadata in extended community format
- Requires BGP software support
- Backward compatible with Phase 1

#### Phase 3: Formal Standards
- IETF RFC for license-aware routing
- IANA-assigned attribute codes
- Industry-wide adoption

## Operational Considerations

### Route Filtering

ISPs should configure filters to:

1. **Preserve license communities** through transit
2. **Log license-tagged traffic** for compliance
3. **Alert on license violations** (e.g., AI scraping of NI-protected content)

**Example Cisco IOS Configuration:**

```cisco
! Match Palimpsest-licensed routes
ip community-list 100 permit 64512:1000

! Preserve communities
route-map PRESERVE_LICENSE permit 10
 match community 100
 set community additive
 continue 20
```

**Example BIRD Configuration:**

```bird
# Define Palimpsest communities
define PALIMPSEST_PROTECTED = (64512, 1000);
define NI_CONSENT_REQUIRED = (64512, 1001);

# Filter to preserve license metadata
filter preserve_license {
    if (PALIMPSEST_PROTECTED ~ bgp_community) then {
        # Log and preserve
        print "Palimpsest-licensed route: ", net;
        bgp_community.add(PALIMPSEST_PROTECTED);
    }
    accept;
}
```

### Caching and CDN Integration

Content Delivery Networks (CDNs) and ISP caches should:

- **Detect license communities** on origin routes
- **Preserve metadata** in cached objects
- **Inject license headers** in HTTP responses
- **Block AI scrapers** for NI-consent routes

### Peering Policy

Update peering agreements to include:

```
§X.X License Metadata Preservation

Both parties agree to:
a) Preserve BGP communities indicating content licensing
b) Not strip license-related metadata from cached content
c) Honour non-interpretive consent flags
d) Maintain audit logs for license-tagged traffic
```

## Security Considerations

### Authentication

BGP license communities must be authenticated to prevent abuse:

- **RPKI (Resource Public Key Infrastructure):** Validate origin AS
- **BGPsec:** Authenticate path to prevent injection
- **ROA (Route Origin Authorisation):** Link licenses to address space

### Abuse Prevention

Mitigate potential attacks:

- **Community spoofing:** Use RPKI to validate license claims
- **Denial of service:** Rate-limit license metadata processing
- **False claims:** Verify license URLs against .well-known/license

## Benefits for ISPs

### Legal Protection

- **Safe harbour reinforcement:** Demonstrable respect for licensing
- **DMCA compliance:** Network-level awareness of rights
- **EU Copyright Directive:** Article 17 compliance support

### Technical Benefits

- **Enhanced routing policy:** More granular traffic management
- **Customer differentiation:** Support for creator-friendly hosting
- **Future-proofing:** Prepare for AI regulation compliance

### Business Case

- **New service offerings:** "License-preserving transit"
- **Premium peering:** License-aware interconnection
- **Compliance as a service:** Audit support for content platforms

## Interoperability

### DNS Integration

BGP communities should reference DNS TXT records for full license text:

```
Route: 192.0.2.0/24 with community 64512:1010
→ Query: _license.example.org TXT
→ Returns: "palimpsest-v0.4 https://palimpsest.example/.well-known/license"
```

### HTTP Header Correlation

License communities should match HTTP headers:

```http
X-License: Palimpsest-v0.4
X-License-NI-Consent: required
X-License-Metadata-URL: https://example.org/.well-known/license
```

### WHOIS Extension

IRR (Internet Routing Registry) WHOIS queries should reveal licensing:

```
$ whois -h whois.ripe.net 192.0.2.0/24
...
remarks: license-type: palimpsest-v0.4
remarks: ni-consent-required: true
...
```

## Migration Path

### For Content Providers

1. Register license metadata in DNS TXT records
2. Request BGP community assignment from upstream
3. Update route objects with license remarks
4. Monitor compliance through logs

### For ISPs

1. Update route filters to preserve license communities
2. Configure logging for license-tagged routes
3. Integrate with caching infrastructure
4. Update peering templates

### For Transit Providers

1. Implement community-preserving policies
2. Offer license-aware routing as premium service
3. Participate in IETF standardisation
4. Educate customers on licensing options

## Standardisation Roadmap

### Short Term (6-12 months)

- Deploy using private ASN communities
- Gather operational experience
- Document best practices

### Medium Term (1-2 years)

- Submit IETF Internet-Draft
- Coordinate with IANA for code point allocation
- Engage routing vendors (Cisco, Juniper, Arista)

### Long Term (2-5 years)

- Publish RFC for license-aware routing
- Integrate with RPKI/BGPsec
- Industry-wide adoption

## Case Study: Example Deployment

### Scenario

A UK-based creative content platform (example.org.uk) serves Palimpsest-licensed works.

**Network Setup:**

- Origin AS: AS64512
- Transit providers: HE.net (AS6939), BT (AS5400)
- Address space: 192.0.2.0/24

**Configuration:**

1. **DNS TXT Record:**
   ```
   _license.example.org.uk. IN TXT "palimpsest-v0.4 ni-consent-required"
   ```

2. **BGP Announcement:**
   ```
   192.0.2.0/24 origin AS64512
   Communities: 64512:1000, 64512:1001
   ```

3. **Route Object (RIPE):**
   ```rpsl
   route: 192.0.2.0/24
   origin: AS64512
   remarks: license-type: palimpsest-v0.4
   ```

**Result:**

- Transit providers preserve communities
- CDNs detect license requirements
- AI scrapers blocked at network edge
- Metadata maintained through delivery chain

## Conclusion

BGP-based license propagation provides a network-layer foundation for protecting creative works. By leveraging existing BGP mechanisms, ISPs can support the Palimpsest License without significant protocol changes.

**Key Takeaways:**

- BGP communities enable license signalling today
- Extended communities provide richer metadata
- Integration with DNS/HTTP creates end-to-end protection
- Standardisation ensures long-term viability

**Call to Action:**

We invite ISPs, transit providers, and network operators to:

1. Review this proposal and provide feedback
2. Pilot license community preservation
3. Join the IETF standardisation effort
4. Integrate with the Palimpsest License ecosystem

---

## References

- RFC 1997: BGP Communities Attribute
- RFC 4360: BGP Extended Communities Attribute
- RFC 2622: Routing Policy Specification Language (RPSL)
- RFC 6810: RPKI/Router Protocol
- RIPE Database Documentation

## Contact

For technical questions or collaboration:
- Palimpsest Stewardship Council
- Technical Working Group
- Email: infrastructure@palimpsest.example

---

*This document is licensed under the Palimpsest License v0.4. Network operators may implement these specifications without restriction, but the ideas and framework remain attributed to the Palimpsest project.*
