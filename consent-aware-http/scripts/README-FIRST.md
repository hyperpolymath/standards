scripts/ — Draft Building Utilities
This folder contains helper scripts to compile Internet-Drafts from their XML source files into readable formats (HTML, PDF, TXT) using the xml2rfc tool.

These drafts form the foundation of the Consent-Aware HTTP standards:

draft-jewell-http-430-consent-required

draft-jewell-aibdp

🛠 Tools Provided
✅ build-drafts.ps1 — PowerShell Script (Windows)
Use this if you're on Windows:

.\scripts\build-drafts.ps1
This invokes xml2rfc to build all drafts into /rendered/.

Requires: pip install xml2rfc Run from the root folder of the repository.

🧪 Makefile — POSIX Shell Targets (Linux/macOS)
Use this if you're on macOS or Linux:

make
This compiles all XML drafts into /rendered/.

Requires: xml2rfc installed and available in your PATH

Use make clean to remove generated outputs.

🔄 Input and Output
📥 Input drafts live in: drafts/

📤 Outputs land in: rendered/

You can review or publish the output HTML/PDFs directly from rendered/.

❓ Troubleshooting
If xml2rfc isn't recognized, try:

pip install xml2rfc
If a draft fails to compile, check that it is valid XML and follows RFC formatting rules.

For help or updates, contact: Jonathan D.A. Jewell — jonathan@metadatastician.art

Declare your perimeter. Compile with care.