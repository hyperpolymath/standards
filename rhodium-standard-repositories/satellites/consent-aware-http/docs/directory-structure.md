🗂 Repository Structure
consent-aware-http/
├── README.md                  # Project overview and entry point
├── LICENSE.md                 # Dual license: MIT + CC BY-SA
├── .gitignore                 # Ignore artifacts and clutter
├── .gitattributes             # Normalize line endings and highlight formats
├── .nojekyll                  # For OpGitHub Pages compatibility

assets/
├── badge-consent-aware.svg            # Main SVG badge (light theme)
├── badge-consent-aware-dark.svg       # Reverse variant for dark backgrounds
├── badge-description.md               # Usage guidance and accessibility notes
├── error-pages/
│   └── 430-consent-required.html      # WCAG-compliant error page

├── drafts/                   # Internet-Draft XML + .txt files
│   ├── draft-jewell-http-430-consent-required-00.xml
│   ├── draft-jewell-aibdp-00.xml
│   └── legacy-index.txt      # Optional index of prior revisions

├── docs/                     # Human-friendly guides + philosophy
│   ├── technical.md          # Developer explainer
│   ├── explainer.md          # Architectural overview
│   ├── ethics.md             # Cultural / axiological framing
│   ├── governance.md         # Organizational implications
│   ├── start-here.md         # Quick adoption guide
│   ├── references.md         # Citations and influences
│   ├── conformance.md        # What makes a valid implementation?
│   └── example-aibdp.json    # Manifest template with comments

├── .github/                  # Meta guides for contribution
│   ├── CONTRIBUTING.md
│   ├── CODE_OF_CONDUCT.md
│   ├── community-guidelines.md
│   ├── PULL_REQUEST_TEMPLATE.md
│   ├── DISCUSSION_TEMPLATE.md
│   ├── SECURITY.md
│   ├── FUNDING.yml
│   └── ISSUE_TEMPLATE/
│       └── feature_request.yml

├── outreach/
│   ├── install-guidance.md            # How to add to your own site/platform
│   ├── disclosure-template-letter.md  # Sample outreach/lobbying letter
│   ├── org-list.md                    # Suggested recipients: EFF, Mozilla, CDN providers
│   └── badge-announcement.md          # Friendly explainer post for IndieWeb, blogs, social

├── rendered/                 # HTML + PDF versions of drafts
│   ├── 430-consent-required.html
│   ├── 430-consent-required.pdf
│   ├── aibdp.html
│   └── aibdp.pdf

├── scripts/                  # Tools to assist devs
│   └── build-drafts.ps1      # PowerShell builder (xml2rfc)
│   └── makefile              # Optional Makefile (POSIX builds)