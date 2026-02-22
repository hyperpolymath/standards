Quickstart — Make Your Site Consent-Aware

✅ Step 1: Declare Your Boundary
Create a file at:

/.well-known/aibdp.json
Add the following content:

json
{
  "aibdp_version": "0.1",
  "contact": "mailto:editor@sinople.pub",
  "indexing": "allowed",
  "training": "refused",
  "regeneration": "refused"
}

You may include additional fields such as "summary", "policy_url", or "signature" depending on your implementation.

✅ Step 2: Respond with HTTP 430
Configure your server to return:

HTTP/1.1 430 Consent Required
Use this response when:

The manifest is missing

The manifest explicitly refuses the declared intent

AI-related requests do not honor declared boundaries

You can include a brief message in the body such as:

Consent manifest unavailable or denied.
See /.well-known/aibdp.json for declared boundaries.

✅ Step 3: Link Your Policy
Link to your AIBDP manifest for transparency:

In your site footer

In robots.txt

In HTTP headers (e.g. Link: or X-AIBDP-Policy:)

Example addition to robots.txt:

# Consent-aware manifest:

Allow: /.well-known/aibdp.json

✅ Step 4: Publish with Integrity

Once your site includes both:

A valid AIBDP manifest

Refusal logic via 430

You’re ready to adopt the Consent-Aware HTTP protocol.

Consider adding a badge, a disclaimer, or educational links to help others learn and follow your lead.

🛡 Maintenance Suggestions

Validate your manifest with a JSON linter

Consider adding a JWS or COSE signature

Check headers using browser dev tools or curl

Use /docs/technical.md and /docs/conformance.md to go deeper

Consent isn’t just a checkbox. It’s a perimeter. Declare yours. For full documentation, see: README.md