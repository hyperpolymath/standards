;; SPDX-License-Identifier: PMPL-1.0-or-later
;; SPDX-FileCopyrightText: 2025 Example Corp

;;; META.scm — Comprehensive Example with All Sections
;;; enterprise-platform

(define-module (enterprise-platform meta)
  #:export (architecture-decisions
            development-practices
            design-rationale))

;;; ============================================================
;;; Architecture Decisions Record (ADR)
;;; ============================================================

(define architecture-decisions
  '((adr-001
     (title . "Microservices Architecture")
     (status . "accepted")
     (date . "2025-01-15")
     (context . "System requires independent scaling of components and
                 team autonomy for parallel development")
     (decision . "Adopt microservices architecture with domain-driven
                  service boundaries")
     (consequences . ("Independent deployment of services"
                      "Technology diversity per service"
                      "Increased operational complexity"
                      "Requires robust service discovery"))
     (deciders . ("Alice Chen" "Bob Kumar" "Carol Martinez"))
     (references . ("https://martinfowler.com/microservices/")))

    (adr-002
     (title . "Event-Driven Communication")
     (status . "accepted")
     (date . "2025-01-20")
     (context . "Services need loose coupling for resilience and
                 to handle varying load patterns")
     (decision . "Use Apache Kafka for async communication between services;
                  gRPC for synchronous calls where latency is critical")
     (consequences . ("Eventual consistency model"
                      "Kafka operational expertise required"
                      "Clear synchronous vs async boundaries"
                      "Audit trail via event log")))

    (adr-003
     (title . "PostgreSQL as Primary Database")
     (status . "accepted")
     (date . "2025-02-01")
     (context . "Need relational database with strong consistency,
                 JSON support, and proven reliability")
     (decision . "Use PostgreSQL 16+ for all services unless specific
                  requirements dictate otherwise")
     (consequences . ("Consistent database expertise"
                      "Excellent JSON and full-text search"
                      "Well-understood backup and HA patterns"
                      "May need Redis for caching layer")))

    (adr-004
     (title . "Kubernetes for Orchestration")
     (status . "accepted")
     (date . "2025-02-10")
     (context . "Need container orchestration for microservices deployment")
     (decision . "Use Kubernetes on AWS EKS with Terraform for IaC")
     (consequences . ("Standardized deployment across environments"
                      "Built-in service discovery and load balancing"
                      "Steep learning curve for operations team"
                      "Requires dedicated platform engineering")))

    (adr-005
     (title . "Monorepo vs Polyrepo")
     (status . "rejected")
     (date . "2025-02-15")
     (context . "Team debated repository strategy for microservices")
     (decision . "Rejected monorepo in favor of polyrepo with service
                  ownership model")
     (consequences . ("Clearer ownership boundaries"
                      "Simpler CI/CD per service"
                      "Challenge: shared library versioning"
                      "Requires good documentation practices")))

    (adr-006
     (title . "API Gateway Pattern")
     (status . "proposed")
     (date . "2025-03-01")
     (context . "Need unified entry point for external API consumers")
     (decision . "Implement Kong API Gateway with rate limiting,
                  authentication, and request transformation")
     (consequences . ("Single entry point for clients"
                      "Centralized cross-cutting concerns"
                      "Potential single point of failure"
                      "Additional infrastructure component")))))

;;; ============================================================
;;; Development Practices
;;; ============================================================

(define development-practices
  '((code-style
     (formatter . "prettier for JS/TS, black for Python, rustfmt for Rust")
     (linter . "eslint, pylint, clippy")
     (type-system . "TypeScript strict mode, Python type hints, Rust")
     (line-length . 100)
     (indent . "spaces")
     (indent-size . 2))

    (security
     (command-execution . "subprocess only, never shell")
     (input-validation . "Zod/Pydantic at API boundaries")
     (credentials . "HashiCorp Vault for secrets, never in code")
     (dependencies . "Dependabot weekly, Snyk scans on PR")
     (authentication . "OAuth 2.0 + OIDC via Auth0")
     (authorization . "RBAC with OPA policies"))

    (documentation
     (format . "AsciiDoc for long-form, Markdown for README")
     (api-docs . "OpenAPI 3.1 generated from code annotations")
     (adr-location . "META.scm in each service repository")
     (runbooks . "Required for all production services")
     (diagrams . "Mermaid or PlantUML, version controlled"))

    (testing
     (framework . "Jest for JS, pytest for Python, cargo test for Rust")
     (coverage-minimum . 80)
     (unit-tests . "Required for all business logic")
     (integration-tests . "Required for all API endpoints")
     (e2e-tests . "Critical user journeys only")
     (contract-tests . "Pact for service boundaries")
     (load-tests . "k6 for performance regression"))

    (versioning
     (scheme . "Semantic Versioning 2.0.0")
     (changelog . "Keep a Changelog format")
     (release-process . "GitHub releases with auto-generated notes")
     (breaking-changes . "Announce 2 weeks in advance"))

    (deployment
     (strategy . "Rolling updates with canary for critical services")
     (environments . ("development" "staging" "production"))
     (infrastructure . "AWS EKS via Terraform")
     (ci-cd . "GitHub Actions with ArgoCD for GitOps")
     (feature-flags . "LaunchDarkly for gradual rollouts"))

    (review
     (required-approvals . 2)
     (automated-checks . ("lint" "test" "security-scan" "type-check"))
     (guidelines . "CONTRIBUTING.md")
     (sla . "Review within 24 business hours"))

    (branching
     (strategy . "trunk-based")
     (main-branch . "main")
     (feature-prefix . "feat/")
     (bugfix-prefix . "fix/")
     (release-branches . #f))))

;;; ============================================================
;;; Design Rationale
;;; ============================================================

(define design-rationale
  '((why-microservices
     "We chose microservices over a monolith because:
      (1) Our three product teams need to deploy independently to maintain
          velocity without coordination overhead
      (2) The payment and analytics domains have vastly different scaling
          requirements
      (3) Team expertise varies - some prefer TypeScript, others Rust
      This adds operational complexity we're accepting in exchange for
      development agility. See adr-001.")

    (why-event-driven
     "Kafka-based event sourcing provides:
      (1) Natural audit trail for compliance requirements
      (2) Decoupling that allows services to evolve independently
      (3) Replay capability for debugging and new service bootstrapping
      We use gRPC for the few cases where synchronous request-response
      is genuinely required (sub-100ms latency needs). See adr-002.")

    (why-postgresql-over-nosql
     "Despite microservices trend toward polyglot persistence, we
      standardized on PostgreSQL because:
      (1) Most services have relational data models
      (2) JSONB handles semi-structured data needs
      (3) Team has deep PostgreSQL expertise
      (4) Fewer databases to operate reduces complexity
      Services may request exceptions with justification. See adr-003.")

    (why-rejected-monorepo
     "We evaluated Nx-based monorepo but rejected it because:
      (1) Team ownership is clearer with dedicated repositories
      (2) CI/CD is simpler without affected-project detection
      (3) Onboarding is faster - new developers clone one service
      The tradeoff is shared library versioning complexity, which we
      manage via semantic versioning and a private npm registry.
      See adr-005 for the full decision record.")

    (why-kubernetes
     "Kubernetes was chosen over ECS/Fargate because:
      (1) Portability - we may move clouds in future
      (2) Ecosystem - Helm charts, operators, service mesh options
      (3) Industry standard - easier hiring
      The complexity cost is real but platform team capacity allows it.")))
