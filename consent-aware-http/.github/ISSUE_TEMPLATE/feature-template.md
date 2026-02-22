name: Feature Request or Protocol Enhancement
description: Suggest a new capability, integration, or architectural refinement
title: "[Proposal] — your concise topic"
labels: [enhancement]
body:
  - type: markdown
    attributes:
      value: |
        Thanks for contributing to the consent-aware ecosystem.
        Please describe your proposed change below.
  - type: textarea
    id: summary
    attributes:
      label: Summary
      description: What do you propose and why does it matter?
      placeholder: Write a short explanation…
    validations:
      required: true
  - type: textarea
    id: scope
    attributes:
      label: Protocol or tooling scope
      description: Is this related to 430, AIBDP, manifests, or infrastructure?
      placeholder: Draft/AIBDP/API Schema/Docs
  - type: input
    id: source
    attributes:
      label: Supporting references (optional)
      description: Link relevant documentation, standards, or prior discussions
