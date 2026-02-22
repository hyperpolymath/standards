;; SPDX-License-Identifier: AGPL-3.0-or-later
;; ECOSYSTEM.scm - Ecosystem relationships for stamp-protocol

(ecosystem
  (version "1.0.0")
  (name "stamp-protocol")
  (type "demonstration-site")
  (purpose
    "Interactive landing page demonstrating STAMP Protocol consent architecture"
    "Showcases formally verified URL validation for unsubscribe links")

  (position-in-ecosystem
    "stamp-protocol is the public-facing demonstration of STAMP Protocol concepts."
    "It serves as:"
    "- Educational reference for how proven formal verification works in practice"
    "- Interactive showcase of consent flow with provable security"
    "- Marketing/outreach tool for STAMP Protocol adoption"
    ""
    "It does NOT contain the full STAMP implementation - that lives in the"
    "protocol specification repository. This is purely a demo/landing page.")

  (related-projects
    (project "rescript-tea"
      (relationship "sibling-standard")
      (description "TEA architecture framework used for state management")
      (integration "Direct dependency - StampApp.res uses Tea.App pattern")
      (url "https://github.com/hyperpolymath/rescript-tea"))

    (project "cadre-tea-router"
      (relationship "sibling-standard")
      (description "URL routing with proven formally verified parsing")
      (integration "Future integration for multi-page navigation")
      (url "https://github.com/hyperpolymath/cadre-tea-router"))

    (project "proven"
      (relationship "sibling-standard")
      (description "Idris2 formally verified library - 90+ proven modules")
      (integration "ProvenSafeUrl for unsubscribe link validation")
      (url "https://github.com/hyperpolymath/proven"))

    (project "stamp-protocol-spec"
      (relationship "inspiration")
      (description "Full STAMP Protocol specification")
      (integration "This site demonstrates concepts from the spec")
      (url "https://github.com/hyperpolymath/stamp-protocol"))

    (project "poly-ssg"
      (relationship "sibling-standard")
      (description "SSG engine comparison site (same architecture)")
      (integration "Shares ReScript+Deno+proven stack")
      (url "https://github.com/hyperpolymath/poly-ssg"))

    (project "asdf-plugins"
      (relationship "sibling-standard")
      (description "Plugin search site (same architecture)")
      (integration "Shares ReScript+Deno+proven stack")
      (url "https://github.com/hyperpolymath/asdf-plugins"))

    (project "axel-protocol"
      (relationship "sibling-standard")
      (description "AXEL label validation (same architecture)")
      (integration "Shares ReScript+Deno+proven stack")
      (url "https://github.com/hyperpolymath/axel-protocol")))

  (what-this-is
    "A demonstration website showing how STAMP Protocol consent flow works"
    "Built with formally verified components (ProvenSafeUrl from proven library)"
    "Interactive TEA application for educational purposes"
    "Reference implementation of secure unsubscribe link handling")

  (what-this-is-not
    "NOT a full STAMP Protocol implementation (see stamp-protocol-spec for that)"
    "NOT a production consent management platform (just a demo)"
    "NOT a general-purpose web framework (specific to STAMP use case)"))
