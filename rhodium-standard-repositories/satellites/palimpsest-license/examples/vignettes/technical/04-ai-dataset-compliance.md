# Technical Vignette: AI Training Dataset Compliance Check

## Scenario: Automated Palimpsest License Detection in Training Data

**Organization**: EthicalAI Research Lab (university-based)
**Challenge**: Building AI training dataset that respects Palimpsest licenses
**Tool**: Automated compliance scanner
**Outcome**: First verified Palimpsest-compliant AI training dataset

---

## The Problem

EthicalAI Research Lab wanted to train a multilingual language model on diverse cultural texts. But they discovered their initial dataset (scraped from the web) included thousands of Palimpsest-licensed works without consent.

**Legal risks:**
- Clause 1.2 violations (interpretive AI without consent)
- Potential lawsuits from creators
- Reputational harm for "ethical AI" organization

**Ethical concerns:**
- Using marginalized communities' cultural work without permission
- Training AI on trauma narratives without consent
- Contributing to AI's extraction of cultural knowledge

**Technical challenge:**
- How to detect Palimpsest licenses at scale?
- How to verify consent?
- How to build compliant dataset efficiently?

---

## The Solution: Automated Compliance Scanner

**Tool name**: `palimpsest-scanner`

**Capabilities:**
1. Detect Palimpsest licenses in web pages, documents, and code repos
2. Parse machine-readable metadata (JSON-LD, schema.org, custom formats)
3. Check for explicit AI training consent
4. Generate compliance reports
5. Suggest alternatives or permission requests

### Technical Implementation

**1. License Detection:**

```python
# palimpsest_scanner/detector.py
import re
import json
from bs4 import BeautifulSoup
from typing import Optional, Dict, List

class PalimpsestDetector:
    """Detect Palimpsest licenses in various formats"""

    PALIMPSEST_PATTERNS = [
        r'Palimpsest\s+License\s+v?0\.4',
        r'palimpsest-0\.4',
        r'SPDX-License-Identifier:\s*Palimpsest-0\.4',
        r'license:\s*["\']Palimpsest-0\.4["\']',
    ]

    def detect_in_text(self, text: str) -> bool:
        """Detect Palimpsest license in plain text"""
        for pattern in self.PALIMPSEST_PATTERNS:
            if re.search(pattern, text, re.IGNORECASE):
                return True
        return False

    def detect_in_html(self, html: str) -> Optional[Dict]:
        """Detect Palimpsest license in HTML with metadata extraction"""
        soup = BeautifulSoup(html, 'html.parser')

        # Check for JSON-LD metadata
        json_ld_scripts = soup.find_all('script', type='application/ld+json')
        for script in json_ld_scripts:
            try:
                data = json.loads(script.string)
                if 'license' in data and 'palimpsest' in data['license'].lower():
                    return {
                        'detected': True,
                        'method': 'json-ld',
                        'version': self._extract_version(data['license']),
                        'metadata': data
                    }
            except json.JSONDecodeError:
                continue

        # Check for .palimpsest.json links
        palimpsest_links = soup.find_all('a', href=re.compile(r'\.palimpsest\.json$'))
        if palimpsest_links:
            return {
                'detected': True,
                'method': 'metadata-file',
                'metadata_url': palimpsest_links[0]['href']
            }

        # Check for license declarations in meta tags
        meta_license = soup.find('meta', attrs={'name': 'license'})
        if meta_license and 'palimpsest' in meta_license.get('content', '').lower():
            return {
                'detected': True,
                'method': 'meta-tag',
                'version': self._extract_version(meta_license['content'])
            }

        # Check for text mentions
        if self.detect_in_text(soup.get_text()):
            return {
                'detected': True,
                'method': 'text-mention',
                'version': '0.4'  # Assume current version
            }

        return None

    def _extract_version(self, text: str) -> str:
        """Extract version number from license string"""
        match = re.search(r'v?(\d+\.\d+)', text)
        return match.group(1) if match else 'unknown'

    def check_ai_consent(self, metadata: Dict) -> bool:
        """Check if AI training is explicitly permitted"""
        # Check various metadata formats
        if 'permissions' in metadata:
            return metadata['permissions'].get('interpretiveAI', False)

        if 'aiTrainingConsent' in metadata:
            return metadata['aiTrainingConsent'] in ['permitted', 'true', True]

        if 'palimpsest' in metadata:
            return metadata['palimpsest'].get('interpretiveAI', False)

        # Default: no consent
        return False
```

**2. Dataset Auditing:**

```python
# palimpsest_scanner/audit.py
import asyncio
from typing import List, Dict
from .detector import PalimpsestDetector

class DatasetAuditor:
    """Audit training dataset for Palimpsest compliance"""

    def __init__(self):
        self.detector = PalimpsestDetector()
        self.violations = []
        self.consented = []
        self.unclear = []

    async def audit_urls(self, urls: List[str]) -> Dict:
        """Audit list of URLs for Palimpsest licenses"""
        tasks = [self._check_url(url) for url in urls]
        results = await asyncio.gather(*tasks)

        summary = {
            'total': len(urls),
            'palimpsest_detected': len([r for r in results if r['palimpsest_detected']]),
            'ai_consent_given': len([r for r in results if r.get('ai_consent', False)]),
            'violations': len(self.violations),
            'action_required': len(self.violations) + len(self.unclear)
        }

        return {
            'summary': summary,
            'violations': self.violations,
            'consented': self.consented,
            'unclear': self.unclear,
            'recommendations': self._generate_recommendations()
        }

    async def _check_url(self, url: str) -> Dict:
        """Check single URL for Palimpsest license"""
        try:
            # Fetch URL content (implementation would use aiohttp)
            html = await self._fetch_url(url)

            detection = self.detector.detect_in_html(html)

            if not detection:
                return {'url': url, 'palimpsest_detected': False}

            # Check for AI consent
            ai_consent = False
            if detection.get('metadata'):
                ai_consent = self.detector.check_ai_consent(detection['metadata'])

            result = {
                'url': url,
                'palimpsest_detected': True,
                'version': detection.get('version'),
                'ai_consent': ai_consent,
                'detection_method': detection['method']
            }

            if ai_consent:
                self.consented.append(result)
            else:
                self.violations.append(result)

            return result

        except Exception as e:
            self.unclear.append({'url': url, 'error': str(e)})
            return {'url': url, 'error': str(e)}

    def _generate_recommendations(self) -> List[str]:
        """Generate action recommendations based on audit"""
        recs = []

        if self.violations:
            recs.append(f"Remove {len(self.violations)} Palimpsest-licensed works without AI consent")
            recs.append("Contact creators for permission (use template: templates/ai-consent-request.md)")

        if self.consented:
            recs.append(f"Verify consent documentation for {len(self.consented)} consented works")
            recs.append("Ensure attribution metadata is preserved in training process")

        if self.unclear:
            recs.append(f"Manually review {len(self.unclear)} unclear cases")

        recs.append("Implement ongoing monitoring for license changes")

        return recs
```

**3. Compliance Report:**

```python
# palimpsest_scanner/reporter.py
class ComplianceReporter:
    """Generate human-readable compliance reports"""

    def generate_report(self, audit_results: Dict, output_format='markdown') -> str:
        """Generate compliance report"""

        if output_format == 'markdown':
            return self._markdown_report(audit_results)
        elif output_format == 'json':
            return json.dumps(audit_results, indent=2)
        else:
            raise ValueError(f"Unsupported format: {output_format}")

    def _markdown_report(self, results: Dict) -> str:
        summary = results['summary']
        violations = results['violations']
        consented = results['consented']
        recs = results['recommendations']

        report = f"""# Palimpsest License Compliance Report

## Summary

- **Total URLs scanned**: {summary['total']}
- **Palimpsest licenses detected**: {summary['palimpsest_detected']}
- **AI training consent given**: {summary['ai_consent_given']}
- **Violations (no consent)**: {summary['violations']}
- **Action required**: {summary['action_required']}

## Compliance Status

{'✅ **COMPLIANT**' if summary['violations'] == 0 else '❌ **NON-COMPLIANT**'}

## Violations Requiring Action

"""
        if violations:
            for v in violations[:10]:  # Show first 10
                report += f"- `{v['url']}` - Palimpsest v{v.get('version', 'unknown')}, **NO AI consent**\n"

            if len(violations) > 10:
                report += f"\n... and {len(violations) - 10} more violations.\n"

            report += "\n**Required action**: Remove these URLs from training dataset or obtain explicit consent.\n\n"
        else:
            report += "No violations detected. ✅\n\n"

        report += f"""## Works with AI Consent

{len(consented)} Palimpsest-licensed works have explicit AI training consent:

"""
        for c in consented[:5]:
            report += f"- `{c['url']}` - Consent verified ✅\n"

        if len(consented) > 5:
            report += f"... and {len(consented) - 5} more.\n"

        report += f"""

## Recommendations

"""
        for rec in recs:
            report += f"- {rec}\n"

        report += """

## Next Steps

1. Remove all non-consented Palimpsest works from dataset
2. Contact creators for permission (template: [ai-consent-request.md](templates/ai-consent-request.md))
3. Document consent in dataset manifest
4. Re-run audit after changes
5. Implement automated scanning in data pipeline

---

**Generated by**: palimpsest-scanner v1.0.0
**Date**: {current_date}
**Audit ID**: {audit_id}
"""

        return report
```

---

## Real-World Application

**EthicalAI Research Lab's process:**

1. **Initial Scan**: Ran scanner on 500,000-URL dataset
   - Detected: 12,487 Palimpsest-licensed works
   - With AI consent: 243
   - Without consent: 12,244

2. **Immediate Action**: Removed 12,244 non-consented works

3. **Consent Requests**: Contacted creators of high-value works
   - Sent 500 permission requests
   - Received 300 responses (60% response rate)
   - Granted: 180 (60% of responses)
   - Denied: 85 (28%)
   - Conditional: 35 (12% - negotiated terms)

4. **Final Dataset**:
   - Original: 500,000 works
   - Palimpsest without consent: 12,244 removed
   - Palimpsest with consent: 423 retained
   - Compliant dataset: 488,179 works

5. **Documentation**: Published dataset methodology:
   > "This dataset excludes all Palimpsest-licensed works without explicit AI training consent. We contacted 500 creators; 180 granted permission. Full consent documentation available in `PALIMPSEST_CONSENTS/` directory."

**Result**: First verified Palimpsest-compliant AI training dataset in the field.

---

## Impact

**Industry Response:**
- 5 other research labs adopted the scanner
- AI conferences started requiring compliance documentation
- Funders asked grantees to demonstrate license compliance

**Creator Trust:**
- Creators appreciated being contacted
- 40% of those who granted consent became collaborators
- "Finally, an AI lab that asks permission" (common feedback)

**Legal Protection:**
- Zero license violation lawsuits
- Strong defense if challenged (documented good-faith compliance)

---

## Best Practices

### For AI Researchers

**Do:**
- Scan datasets BEFORE training begins
- Remove non-consented Palimpsest works proactively
- Contact creators for permission
- Document all consent
- Re-scan periodically (licenses can change)
- Publish compliance methodology

**Don't:**
- Assume "publicly available" means "OK to train on"
- Scrape first, check later
- Ignore license detection tools
- Train on disputed data "pending clarification"

### For Creators

**Do:**
- Use machine-readable Palimpsest metadata (JSON-LD)
- Make AI consent status explicit
- Respond to good-faith permission requests
- Monitor where your work appears

**Don't:**
- Leave AI permissions ambiguous
- Ignore that your work might be in datasets
- Assume AI companies will check manually

---

## Discussion Questions

1. Should AI training datasets be required to publish compliance audits? Or is it voluntary best practice?

2. If a dataset contains 1% non-consented Palimpsest works, is the entire model "tainted"? Or only proportionally affected?

3. Should there be a central registry of Palimpsest works to make scanning easier? Who would maintain it?

4. Can automated scanning ever be 100% accurate? Or will some licenses always be missed?

5. If a creator grants consent but later revokes it, can an already-trained model continue to be used? Or must it be retrained?

---

**License Note**: This vignette is licensed under CC BY-SA 4.0. EthicalAI Research Lab and `palimpsest-scanner` are fictional, created to illustrate automated compliance checking. The technical patterns are based on real license detection and dataset auditing practices.
