# AIBDP Manifest Examples for Different Scenarios

This directory contains example AIBDP manifests demonstrating how different types of organizations and content creators can declare AI usage boundaries.

## Quick Reference

| Scenario | Stance | Use Case |
|----------|--------|----------|
| [Personal Blog](#1-personal-blog) | Mixed | Indie publisher, creative work |
| [News Organization](#2-news-organization) | Conditional | Journalism, investigative reporting |
| [Academic Archive](#3-academic-archive) | Permissive | Research, open access |
| [Private Company](#4-private-company) | Restrictive | Proprietary documentation |
| [Open Source Project](#5-open-source-project) | Permissive | Public code, documentation |
| [Artist Portfolio](#6-artist-portfolio) | Highly Restrictive | Creative work protection |
| [Government Website](#7-government-website) | Public Domain | Civic information |
| [Educational Institution](#8-educational-institution) | Educational | Course materials, research |
| [Medical/Healthcare](#9-medicalhealthcare-provider) | HIPAA Compliant | Patient privacy |
| [Legal Firm](#10-legal-firm) | Restricted | Client confidentiality |
| [Community Wiki](#11-community-wiki) | Collaborative | User-generated content |
| [E-commerce Site](#12-e-commerce-site) | Commercial | Product descriptions |

---

## 1. Personal Blog

**Scenario**: Independent writer sharing essays, creative writing, and personal reflections.

**Stance**: Refused training, allowed indexing, conditional summarization

```json
{
  "aibdp_version": "0.2",
  "contact": "mailto:author@personalblog.com",
  "expires": "2026-01-01T00:00:00Z",
  "policy_uri": "https://personalblog.com/ai-policy",

  "policies": {
    "training": {
      "status": "refused",
      "rationale": "My creative work is my intellectual property and should not be used to train commercial AI models without compensation and attribution.",
      "alternatives": "Contact me directly for licensing arrangements"
    },

    "indexing": {
      "status": "allowed",
      "scope": "all",
      "note": "I want my work to be discoverable"
    },

    "summarization": {
      "status": "conditional",
      "conditions": [
        "Preserve author attribution",
        "Link back to original post",
        "Do not misrepresent my views"
      ],
      "note": "Summaries must maintain the spirit of the original"
    },

    "question_answering": {
      "status": "allowed",
      "conditions": [
        "Cite source with full URL",
        "Indicate personal opinion nature"
      ]
    },

    "generation": {
      "status": "refused",
      "rationale": "Do not create synthetic versions of my writing style or voice"
    },

    "fine_tuning": {
      "status": "refused",
      "note": "My writing style is personal and should not be replicated"
    }
  },

  "enforcement": {
    "mechanism": "http_430",
    "contact_before_litigation": true,
    "preferred_resolution": "respectful dialogue"
  },

  "metadata": {
    "author": "Jane Doe",
    "created": "2025-07-20",
    "organization": "Independent Creator"
  },

  "philosophy": {
    "core_principle": "My words are my own. Attribution and consent matter.",
    "values": [
      "Authorship dignity",
      "Fair compensation for creative work",
      "Discoverability without exploitation"
    ]
  }
}
```

---

## 2. News Organization

**Scenario**: Investigative journalism outlet with subscription model.

**Stance**: Conditional training with attribution, allowed indexing, protected investigative content

```json
{
  "aibdp_version": "0.2",
  "contact": "mailto:legal@newsorg.com",
  "expires": "2025-12-31T23:59:59Z",
  "policy_uri": "https://newsorg.com/ai-usage-policy",

  "policies": {
    "training": {
      "status": "conditional",
      "conditions": [
        "Attribution to NewsOrg in model documentation",
        "Preserve factual accuracy and context",
        "Do not strip bylines or source citations",
        "Commercial training requires licensing agreement"
      ],
      "scope": ["/articles/**", "/reports/**"],
      "exceptions": [
        {
          "path": "/investigations/**",
          "status": "refused",
          "note": "Investigative journalism requires heightened protection"
        }
      ]
    },

    "indexing": {
      "status": "allowed",
      "scope": "all",
      "note": "Full indexing permitted for news discovery"
    },

    "summarization": {
      "status": "allowed",
      "conditions": [
        "Maintain journalistic accuracy",
        "Preserve critical nuance and context",
        "Cite NewsOrg as source",
        "Link to full article"
      ]
    },

    "question_answering": {
      "status": "allowed",
      "conditions": [
        "Cite article title, author, and publication date",
        "Link to original article",
        "Indicate if content is behind paywall"
      ]
    },

    "generation": {
      "status": "refused",
      "rationale": "Synthetic journalism undermines editorial integrity and trust",
      "alternatives": "Link to original reporting with proper attribution"
    },

    "commercial_training": {
      "status": "conditional",
      "conditions": [
        "Licensing agreement required",
        "Financial compensation to support journalism",
        "Transparent disclosure in AI system documentation",
        "Preserve journalist attribution"
      ],
      "note": "Contact legal@newsorg.com for licensing"
    }
  },

  "special_provisions": {
    "academic_research": {
      "status": "allowed",
      "conditions": ["Proper academic citation", "Non-commercial use"],
      "note": "We support journalism research and education"
    }
  },

  "enforcement": {
    "mechanism": "http_430",
    "contact_before_litigation": false,
    "preferred_resolution": "licensing agreement",
    "note": "Violations may result in legal action and public disclosure"
  },

  "metadata": {
    "organization": "NewsOrg Media Inc.",
    "created": "2025-07-01",
    "related_standards": ["NUJ Code of Conduct", "SPJ Code of Ethics"]
  },

  "philosophy": {
    "core_principle": "Quality journalism requires sustainability. AI training must support, not undermine, the journalism ecosystem.",
    "values": [
      "Editorial independence",
      "Journalist attribution",
      "Factual accuracy preservation",
      "Sustainable news business models"
    ]
  }
}
```

---

## 3. Academic Archive

**Scenario**: University open access repository for research papers.

**Stance**: Permissive for research, education, and academic purposes

```json
{
  "aibdp_version": "0.2",
  "contact": "mailto:library@university.edu",
  "expires": "2026-06-30T23:59:59Z",
  "policy_uri": "https://university.edu/library/ai-policy",

  "policies": {
    "training": {
      "status": "allowed",
      "conditions": [
        "Cite papers using DOI or permanent URL",
        "Preserve author attribution and institutional affiliation",
        "Maintain academic integrity"
      ],
      "note": "We encourage AI training on academic research to advance knowledge"
    },

    "indexing": {
      "status": "encouraged",
      "purpose": [
        "Academic search engines",
        "Research discovery platforms",
        "Citation network analysis"
      ]
    },

    "summarization": {
      "status": "encouraged",
      "conditions": [
        "Preserve scientific accuracy",
        "Maintain proper academic citation format"
      ],
      "note": "Summaries help researchers navigate large bodies of literature"
    },

    "question_answering": {
      "status": "encouraged",
      "conditions": [
        "Cite papers with full academic citation",
        "Indicate peer-review status",
        "Preserve scientific nuance"
      ]
    },

    "generation": {
      "status": "conditional",
      "conditions": [
        "Generated content must clearly indicate it is synthetic",
        "Original research must be cited",
        "Do not present generated content as peer-reviewed"
      ],
      "rationale": "Synthetic academic content must be clearly labeled to maintain research integrity"
    },

    "fine_tuning": {
      "status": "encouraged",
      "purpose": [
        "Academic writing assistants",
        "Research summarization tools",
        "Literature review automation",
        "Accessibility tools for research"
      ]
    },

    "embedding": {
      "status": "allowed",
      "note": "Vector embeddings support semantic search and research discovery"
    },

    "commercial_training": {
      "status": "conditional",
      "conditions": [
        "Respect author copyright and licensing",
        "Transparent disclosure of training data sources",
        "Consider financial support for open access publishing"
      ]
    }
  },

  "special_provisions": {
    "academic_research": {
      "status": "unrestricted",
      "note": "All content freely available for academic research and education"
    },

    "educational_use": {
      "status": "unrestricted",
      "note": "Use in courses, teaching, and educational materials is strongly encouraged"
    }
  },

  "scope": {
    "applies_to": [
      "Research papers and preprints",
      "Dissertations and theses",
      "Technical reports",
      "Conference proceedings"
    ]
  },

  "enforcement": {
    "mechanism": "none",
    "note": "We rely on academic norms and goodwill rather than technical enforcement"
  },

  "metadata": {
    "organization": "University Research Library",
    "created": "2025-07-15",
    "repository_type": "Open Access Institutional Repository"
  },

  "philosophy": {
    "core_principle": "Knowledge should be freely shared to advance human understanding. We welcome responsible AI use that accelerates research.",
    "values": [
      "Open science",
      "Research accessibility",
      "Academic integrity",
      "Knowledge advancement"
    ]
  }
}
```

---

## 4. Private Company

**Scenario**: SaaS company with proprietary documentation and technical guides.

**Stance**: Restrictive on all AI uses except authorized customer support

```json
{
  "aibdp_version": "0.2",
  "contact": "mailto:legal@saascompany.com",
  "expires": "2025-10-01T00:00:00Z",
  "policy_uri": "https://saascompany.com/legal/ai-policy",

  "policies": {
    "training": {
      "status": "refused",
      "rationale": "Our documentation contains proprietary technical information, trade secrets, and competitive advantages",
      "scope": "all"
    },

    "indexing": {
      "status": "conditional",
      "conditions": [
        "Authorized search engines only (Google, Bing, DuckDuckGo)",
        "Respect robots.txt directives",
        "Customer-facing documentation only (/docs/public/**)"
      ],
      "scope": ["/docs/public/**"],
      "exceptions": [
        {
          "path": "/docs/internal/**",
          "status": "refused",
          "note": "Internal documentation is confidential"
        }
      ]
    },

    "summarization": {
      "status": "conditional",
      "conditions": [
        "Authorized customer support systems only",
        "Logged-in customers only",
        "Preserve technical accuracy"
      ],
      "note": "We operate our own AI-powered documentation assistant for customers"
    },

    "question_answering": {
      "status": "refused",
      "rationale": "External AI systems may provide incorrect or outdated technical information about our product"
    },

    "generation": {
      "status": "refused",
      "rationale": "Synthetic documentation may contain errors and create support liabilities"
    },

    "fine_tuning": {
      "status": "refused",
      "scope": "all"
    },

    "embedding": {
      "status": "refused",
      "scope": "all"
    },

    "commercial_training": {
      "status": "refused",
      "rationale": "Competitive concerns and intellectual property protection",
      "note": "No exceptions"
    }
  },

  "enforcement": {
    "mechanism": "http_430",
    "contact_before_litigation": false,
    "preferred_resolution": "cease and desist",
    "note": "Violations will be pursued through legal action"
  },

  "metadata": {
    "organization": "SaaS Company Inc.",
    "created": "2025-07-20",
    "legal_basis": "Trade secret protection, competitive advantage"
  },

  "philosophy": {
    "core_principle": "Our documentation represents significant investment and competitive differentiation. Unauthorized AI use threatens our business.",
    "values": [
      "Intellectual property protection",
      "Competitive advantage preservation",
      "Customer privacy",
      "Technical accuracy control"
    ]
  }
}
```

---

## 5. Open Source Project

**Scenario**: Large open source project with code, documentation, and community contributions.

**Stance**: Highly permissive, encourages AI use for development and learning

```json
{
  "aibdp_version": "0.2",
  "contact": "mailto:maintainers@opensource-project.org",
  "expires": "2026-12-31T23:59:59Z",
  "policy_uri": "https://opensource-project.org/ai-policy",

  "policies": {
    "training": {
      "status": "allowed",
      "conditions": [
        "Preserve license attribution (MIT License)",
        "Respect copyright notices",
        "Generated code should indicate it is AI-generated"
      ],
      "note": "We welcome AI training on our code and documentation"
    },

    "indexing": {
      "status": "encouraged",
      "purpose": [
        "Code search engines",
        "Documentation discovery",
        "Package registries",
        "Developer tools"
      ]
    },

    "summarization": {
      "status": "encouraged",
      "note": "Summaries help developers understand our project quickly"
    },

    "question_answering": {
      "status": "encouraged",
      "conditions": [
        "Cite repository URL",
        "Indicate version/release if relevant",
        "Link to relevant documentation"
      ],
      "note": "We want AI assistants to help developers use our project"
    },

    "generation": {
      "status": "allowed",
      "conditions": [
        "Generated code must respect MIT License",
        "Indicate AI generation in comments if appropriate",
        "Do not claim AI-generated code is official project code"
      ]
    },

    "fine_tuning": {
      "status": "encouraged",
      "purpose": [
        "Code completion tools",
        "Developer assistants",
        "Documentation generation",
        "Bug detection and analysis"
      ],
      "note": "We actively encourage fine-tuning for developer productivity"
    },

    "embedding": {
      "status": "allowed",
      "note": "Vector embeddings support code search and semantic analysis"
    },

    "commercial_training": {
      "status": "allowed",
      "conditions": [
        "Respect MIT License terms",
        "Preserve attribution",
        "Consider contributing back to the project"
      ],
      "note": "Commercial AI development is welcome under our open source license"
    }
  },

  "special_provisions": {
    "educational_use": {
      "status": "unrestricted",
      "note": "We encourage use in coding bootcamps, courses, and tutorials"
    },

    "standards_development": {
      "status": "collaborative",
      "note": "We participate in open standards and welcome AI systems that support those standards"
    }
  },

  "enforcement": {
    "mechanism": "none",
    "note": "We rely on open source license compliance and community norms",
    "preferred_resolution": "community discussion and contribution"
  },

  "metadata": {
    "organization": "Open Source Project Maintainers",
    "created": "2025-07-01",
    "license": "MIT License",
    "repository": "https://github.com/opensource-project/main"
  },

  "philosophy": {
    "core_principle": "Open source thrives on sharing and collaboration. We welcome AI systems that amplify developer productivity and project accessibility.",
    "values": [
      "Openness and transparency",
      "Developer empowerment",
      "Community collaboration",
      "Knowledge sharing"
    ],
    "quote": "Build together, learn together, grow together."
  }
}
```

---

## 6. Artist Portfolio

**Scenario**: Digital artist showcasing original artwork and creative projects.

**Stance**: Highly restrictive to protect creative work and style

```json
{
  "aibdp_version": "0.2",
  "contact": "mailto:artist@artportfolio.com",
  "expires": "2026-03-31T23:59:59Z",
  "policy_uri": "https://artportfolio.com/ai-ethics",

  "policies": {
    "training": {
      "status": "refused",
      "rationale": "My artwork represents years of skill development, personal vision, and creative labor. AI training on my work without consent and compensation constitutes theft of my artistic voice.",
      "scope": "all",
      "alternatives": "Contact me directly for commissioned collaborations or licensing"
    },

    "indexing": {
      "status": "conditional",
      "conditions": [
        "For art discovery and portfolio presentation only",
        "Image thumbnails only (no high-resolution scraping)",
        "Preserve artist attribution and copyright notice"
      ],
      "scope": ["/portfolio/**", "/gallery/**"]
    },

    "summarization": {
      "status": "conditional",
      "conditions": [
        "Describe my work respectfully and accurately",
        "Preserve artist attribution",
        "Link to original artwork",
        "Do not reduce my work to training data descriptions"
      ]
    },

    "question_answering": {
      "status": "conditional",
      "conditions": [
        "Cite artist name and portfolio URL",
        "Preserve artistic context and intent",
        "Do not provide instructions for replicating my style"
      ]
    },

    "generation": {
      "status": "refused",
      "rationale": "Generating artwork 'in my style' or based on my portfolio constitutes artistic identity theft and devalues my unique creative voice"
    },

    "fine_tuning": {
      "status": "refused",
      "rationale": "Fine-tuning on my artwork to replicate my style is unethical and economically harmful",
      "note": "This includes LoRA training, style transfer, and any form of artistic mimicry"
    },

    "embedding": {
      "status": "refused",
      "rationale": "Vector embeddings of my artwork enable style replication and unauthorized derivatives"
    },

    "commercial_training": {
      "status": "refused",
      "rationale": "I do not consent to any commercial AI training on my creative work, under any circumstances",
      "note": "Legal action will be pursued against violations"
    }
  },

  "enforcement": {
    "mechanism": "http_430",
    "contact_before_litigation": false,
    "preferred_resolution": "immediate cessation and public retraction",
    "note": "I document and publicly share AI scraping violations to raise awareness"
  },

  "metadata": {
    "author": "Creative Artist Name",
    "created": "2025-07-20",
    "copyright": "All rights reserved © 2025"
  },

  "philosophy": {
    "core_principle": "Art is not data. Creativity is not raw material for extraction. My work is my livelihood and my identity.",
    "values": [
      "Artistic integrity",
      "Creative labor dignity",
      "Anti-exploitation",
      "Human creativity preservation"
    ],
    "quote": "Without consent, there is no collaboration—only theft. — Artist Statement"
  }
}
```

---

## 7. Government Website

**Scenario**: Federal/national government public information portal.

**Stance**: Public domain, unrestricted access for civic benefit

```json
{
  "aibdp_version": "0.2",
  "contact": "mailto:webmaster@government.gov",
  "expires": "2027-01-01T00:00:00Z",
  "policy_uri": "https://government.gov/ai-usage-policy",

  "policies": {
    "training": {
      "status": "allowed",
      "conditions": [
        "Preserve accuracy of government information",
        "Cite government source with official URL",
        "Indicate publication/update date for time-sensitive information"
      ],
      "note": "Government information is public domain and freely available for AI training"
    },

    "indexing": {
      "status": "encouraged",
      "purpose": [
        "Civic information discovery",
        "Government services accessibility",
        "Public records search"
      ],
      "note": "We actively want citizens to find government information easily"
    },

    "summarization": {
      "status": "allowed",
      "conditions": [
        "Maintain factual accuracy",
        "Preserve policy nuances and legal language precision",
        "Link to authoritative government source"
      ]
    },

    "question_answering": {
      "status": "encouraged",
      "conditions": [
        "Cite official government source",
        "Indicate when information may be time-sensitive or outdated",
        "Preserve legal and policy accuracy"
      ],
      "note": "AI systems answering questions about government services improve civic engagement"
    },

    "generation": {
      "status": "conditional",
      "conditions": [
        "Generated content must clearly indicate it is not official government communication",
        "Must cite official sources",
        "Cannot misrepresent government policy or services"
      ],
      "rationale": "Synthetic government content must be clearly labeled to prevent misinformation"
    },

    "fine_tuning": {
      "status": "encouraged",
      "purpose": [
        "Civic chatbots",
        "Government service assistants",
        "Policy research tools",
        "Accessibility tools for government information"
      ]
    },

    "commercial_training": {
      "status": "allowed",
      "conditions": [
        "Government information is public domain",
        "Preserve accuracy and attribution",
        "Do not misrepresent government endorsement of commercial products"
      ]
    }
  },

  "special_provisions": {
    "academic_research": {
      "status": "unrestricted",
      "note": "Government data is freely available for research and analysis"
    },

    "educational_use": {
      "status": "unrestricted",
      "note": "Use in civics education is strongly encouraged"
    }
  },

  "enforcement": {
    "mechanism": "none",
    "note": "Government information is public domain. We rely on accuracy norms rather than technical enforcement.",
    "preferred_resolution": "collaboration and correction of inaccuracies"
  },

  "metadata": {
    "organization": "National Government Web Portal",
    "created": "2025-07-01",
    "legal_basis": "Public domain - government works"
  },

  "philosophy": {
    "core_principle": "Government information belongs to the people. We encourage maximum accessibility and reuse to support informed citizenship.",
    "values": [
      "Transparency",
      "Civic engagement",
      "Public access",
      "Democratic participation"
    ]
  }
}
```

---

## 8. Educational Institution

**Scenario**: K-12 school district with curriculum materials and student resources.

**Stance**: Educational focus with privacy protections

```json
{
  "aibdp_version": "0.2",
  "contact": "mailto:tech@schooldistrict.edu",
  "expires": "2026-06-30T23:59:59Z",
  "policy_uri": "https://schooldistrict.edu/ai-policy",

  "policies": {
    "training": {
      "status": "conditional",
      "conditions": [
        "Educational use only",
        "No commercial training on student-facing materials",
        "Preserve educational context and age-appropriateness",
        "Cite school district as source"
      ],
      "scope": ["/curriculum/**", "/resources/**"],
      "exceptions": [
        {
          "path": "/student-work/**",
          "status": "refused",
          "note": "FERPA protected - student work is confidential"
        }
      ]
    },

    "indexing": {
      "status": "allowed",
      "conditions": [
        "Public curriculum materials only",
        "Respect FERPA privacy protections"
      ],
      "scope": ["/curriculum/**", "/resources/**"]
    },

    "summarization": {
      "status": "allowed",
      "conditions": [
        "Preserve educational accuracy",
        "Maintain age-appropriate context",
        "Cite school district source"
      ]
    },

    "question_answering": {
      "status": "allowed",
      "conditions": [
        "Educational context only",
        "Age-appropriate responses",
        "No circumvention of educational scaffolding"
      ],
      "note": "AI tutoring systems may reference our materials"
    },

    "generation": {
      "status": "refused",
      "rationale": "Synthetic curriculum materials may not meet educational standards and age-appropriateness requirements"
    },

    "fine_tuning": {
      "status": "conditional",
      "conditions": [
        "Educational applications only",
        "Preserve age-appropriateness",
        "Respect FERPA and COPPA privacy laws"
      ],
      "purpose": [
        "Educational tutoring systems",
        "Accessibility tools for students",
        "Teacher planning assistants"
      ]
    },

    "commercial_training": {
      "status": "conditional",
      "conditions": [
        "Educational technology products only",
        "Transparent disclosure of training data use",
        "Consider partnership or licensing with school district",
        "Comply with student privacy laws (FERPA, COPPA)"
      ]
    }
  },

  "special_provisions": {
    "educational_use": {
      "status": "encouraged",
      "conditions": [
        "Respect student privacy",
        "Maintain educational integrity"
      ]
    }
  },

  "enforcement": {
    "mechanism": "http_430",
    "contact_before_litigation": true,
    "preferred_resolution": "partnership discussion for legitimate educational technology",
    "note": "Student privacy violations will be reported to relevant authorities"
  },

  "metadata": {
    "organization": "School District Educational Technology Office",
    "created": "2025-07-15",
    "compliance": ["FERPA", "COPPA", "State Education Privacy Laws"]
  },

  "philosophy": {
    "core_principle": "Education benefits from technology, but student privacy and educational integrity are paramount.",
    "values": [
      "Student privacy protection",
      "Educational quality",
      "Age-appropriate content",
      "Teacher autonomy"
    ]
  }
}
```

---

## 9. Medical/Healthcare Provider

**Scenario**: Hospital/clinic website with patient education materials.

**Stance**: HIPAA compliant, protected health information restrictions

```json
{
  "aibdp_version": "0.2",
  "contact": "mailto:compliance@hospital.org",
  "expires": "2025-12-31T23:59:59Z",
  "policy_uri": "https://hospital.org/ai-compliance",

  "policies": {
    "training": {
      "status": "conditional",
      "conditions": [
        "Public health education materials only (/health-library/**)",
        "No PHI (Protected Health Information)",
        "Medical accuracy preservation required",
        "Cite hospital as source with medical disclaimers"
      ],
      "scope": ["/health-library/**"],
      "exceptions": [
        {
          "path": "/patient-portal/**",
          "status": "refused",
          "note": "HIPAA protected - contains PHI"
        },
        {
          "path": "/medical-records/**",
          "status": "refused",
          "note": "Confidential patient data"
        }
      ]
    },

    "indexing": {
      "status": "conditional",
      "conditions": [
        "Public-facing health education only",
        "No patient portal or confidential sections",
        "Preserve medical disclaimers"
      ],
      "scope": ["/health-library/**", "/about/**"]
    },

    "summarization": {
      "status": "conditional",
      "conditions": [
        "Preserve medical accuracy and safety information",
        "Include appropriate medical disclaimers",
        "Do not oversimplify critical health information",
        "Link to authoritative medical source"
      ],
      "note": "Inaccurate medical summaries pose patient safety risks"
    },

    "question_answering": {
      "status": "conditional",
      "conditions": [
        "Include medical disclaimers",
        "Indicate information is educational only, not medical advice",
        "Preserve critical safety warnings",
        "Cite credible medical sources"
      ]
    },

    "generation": {
      "status": "refused",
      "rationale": "Synthetic medical content poses patient safety and liability risks. All medical information must be professionally reviewed."
    },

    "fine_tuning": {
      "status": "refused",
      "rationale": "Medical AI systems require clinical validation and regulatory approval"
    },

    "commercial_training": {
      "status": "conditional",
      "conditions": [
        "Public health education materials only",
        "Regulatory compliance (FDA for medical AI)",
        "Liability insurance and professional oversight",
        "Transparent disclosure of training data",
        "Business associate agreement if PHI involved"
      ]
    }
  },

  "special_provisions": {
    "academic_research": {
      "status": "conditional",
      "conditions": [
        "IRB approval if applicable",
        "De-identified data only",
        "HIPAA compliance",
        "Research ethics protocols"
      ]
    }
  },

  "enforcement": {
    "mechanism": "http_430",
    "contact_before_litigation": false,
    "preferred_resolution": "immediate cessation",
    "note": "HIPAA violations will be reported to HHS Office for Civil Rights. Medical misinformation liability will be pursued."
  },

  "metadata": {
    "organization": "Hospital Health System",
    "created": "2025-07-20",
    "compliance": ["HIPAA", "HITECH", "State Medical Privacy Laws"],
    "accreditation": "Joint Commission Accredited"
  },

  "philosophy": {
    "core_principle": "Patient safety and privacy are absolute. AI use in healthcare requires rigorous oversight and accountability.",
    "values": [
      "Patient privacy (HIPAA)",
      "Medical accuracy",
      "Safety first",
      "Professional accountability"
    ]
  }
}
```

---

## 10. Legal Firm

**Scenario**: Law firm with legal analysis, case studies, and client resources.

**Stance**: Attorney-client privilege protection, selective public content

```json
{
  "aibdp_version": "0.2",
  "contact": "mailto:ethics@lawfirm.com",
  "expires": "2025-11-30T23:59:59Z",
  "policy_uri": "https://lawfirm.com/ai-ethics-policy",

  "policies": {
    "training": {
      "status": "conditional",
      "conditions": [
        "Public legal blog posts only (/blog/**)",
        "No client-related content",
        "Preserve legal citations and precedent accuracy",
        "Cite law firm as source"
      ],
      "scope": ["/blog/**"],
      "exceptions": [
        {
          "path": "/client-portal/**",
          "status": "refused",
          "note": "Attorney-client privilege - strictly confidential"
        },
        {
          "path": "/case-files/**",
          "status": "refused",
          "note": "Work product privilege"
        }
      ]
    },

    "indexing": {
      "status": "conditional",
      "conditions": [
        "Public website only",
        "No confidential client sections",
        "Preserve legal disclaimers"
      ],
      "scope": ["/blog/**", "/about/**", "/practice-areas/**"]
    },

    "summarization": {
      "status": "conditional",
      "conditions": [
        "Preserve legal accuracy and nuance",
        "Include legal disclaimers (not legal advice)",
        "Maintain citation accuracy",
        "Do not oversimplify complex legal analysis"
      ]
    },

    "question_answering": {
      "status": "conditional",
      "conditions": [
        "Include disclaimer: 'This is not legal advice'",
        "Preserve jurisdictional context",
        "Cite authoritative legal sources",
        "Recommend consulting licensed attorney"
      ]
    },

    "generation": {
      "status": "refused",
      "rationale": "Synthetic legal analysis poses malpractice risks and unauthorized practice of law concerns"
    },

    "fine_tuning": {
      "status": "refused",
      "rationale": "Legal AI systems require professional oversight and ethical compliance"
    },

    "commercial_training": {
      "status": "refused",
      "rationale": "Legal content represents professional work product and competitive advantage",
      "note": "Contact for licensing inquiries"
    }
  },

  "enforcement": {
    "mechanism": "http_430",
    "contact_before_litigation": false,
    "preferred_resolution": "immediate cessation and potential bar complaint",
    "note": "Violations may constitute unauthorized practice of law and will be reported to state bar"
  },

  "metadata": {
    "organization": "Law Firm LLP",
    "created": "2025-07-20",
    "ethical_compliance": ["ABA Model Rules of Professional Conduct", "State Bar Ethics Rules"]
  },

  "philosophy": {
    "core_principle": "Attorney-client privilege and professional ethics are sacrosanct. AI use in legal practice requires strict confidentiality and professional responsibility.",
    "values": [
      "Attorney-client privilege",
      "Professional ethics",
      "Legal accuracy",
      "Malpractice prevention"
    ]
  }
}
```

---

## 11. Community Wiki

**Scenario**: Collaborative wiki with user-generated content (Wikipedia-style).

**Stance**: Permissive with attribution requirements

```json
{
  "aibdp_version": "0.2",
  "contact": "mailto:admin@communitywiki.org",
  "expires": "2026-12-31T23:59:59Z",
  "policy_uri": "https://communitywiki.org/ai-policy",

  "policies": {
    "training": {
      "status": "allowed",
      "conditions": [
        "Preserve Creative Commons BY-SA 4.0 license",
        "Maintain contributor attribution",
        "Respect edit history and collaborative nature"
      ],
      "note": "Wiki content is collaboratively created and shared under CC BY-SA"
    },

    "indexing": {
      "status": "encouraged",
      "note": "We want wiki knowledge to be discoverable"
    },

    "summarization": {
      "status": "allowed",
      "conditions": [
        "Preserve factual accuracy",
        "Cite wiki as source",
        "Indicate collaborative/crowdsourced nature",
        "Link to current version of article"
      ]
    },

    "question_answering": {
      "status": "encouraged",
      "conditions": [
        "Cite wiki article URL",
        "Indicate content is community-maintained",
        "Note when information may be outdated or disputed"
      ]
    },

    "generation": {
      "status": "conditional",
      "conditions": [
        "Generated content must respect CC BY-SA license",
        "Clearly indicate AI generation",
        "Do not present as authoritative wiki content",
        "Consider contributing generated improvements back to wiki"
      ]
    },

    "fine_tuning": {
      "status": "encouraged",
      "purpose": [
        "Knowledge synthesis tools",
        "Educational assistants",
        "Accessibility tools",
        "Translation services"
      ]
    },

    "commercial_training": {
      "status": "allowed",
      "conditions": [
        "Respect CC BY-SA 4.0 license terms",
        "Preserve attribution",
        "Share-alike requirements apply to derivatives",
        "Consider donating to support wiki infrastructure"
      ]
    }
  },

  "special_provisions": {
    "educational_use": {
      "status": "unrestricted",
      "note": "Wiki exists to spread knowledge freely"
    }
  },

  "enforcement": {
    "mechanism": "none",
    "note": "We rely on Creative Commons license compliance and community norms"
  },

  "metadata": {
    "organization": "Community Wiki Foundation",
    "created": "2025-07-01",
    "license": "Creative Commons BY-SA 4.0",
    "content_model": "Collaborative crowdsourcing"
  },

  "philosophy": {
    "core_principle": "Free knowledge for all. Community collaboration creates value that should be shared openly.",
    "values": [
      "Open knowledge",
      "Community collaboration",
      "Accessibility",
      "Continuous improvement"
    ]
  }
}
```

---

## 12. E-commerce Site

**Scenario**: Online store with product descriptions, reviews, and shopping content.

**Stance**: Commercial interests, selective AI use

```json
{
  "aibdp_version": "0.2",
  "contact": "mailto:legal@ecommerce.com",
  "expires": "2025-10-31T23:59:59Z",
  "policy_uri": "https://ecommerce.com/ai-terms",

  "policies": {
    "training": {
      "status": "conditional",
      "conditions": [
        "Product descriptions only (not customer data)",
        "Preserve brand name and trademark attribution",
        "No training on proprietary product photography",
        "Respect manufacturer copyright"
      ],
      "scope": ["/products/**"],
      "exceptions": [
        {
          "path": "/customer-reviews/**",
          "status": "refused",
          "note": "Customer content - privacy protected"
        },
        {
          "path": "/customer-accounts/**",
          "status": "refused",
          "note": "Personal customer data"
        }
      ]
    },

    "indexing": {
      "status": "encouraged",
      "purpose": [
        "Product search engines",
        "Price comparison tools",
        "Shopping assistants"
      ],
      "scope": ["/products/**", "/categories/**"]
    },

    "summarization": {
      "status": "allowed",
      "conditions": [
        "Preserve product accuracy and specifications",
        "Maintain brand attribution",
        "Link to product page for purchase"
      ]
    },

    "question_answering": {
      "status": "allowed",
      "conditions": [
        "Cite product page URL",
        "Preserve accurate pricing and availability (may be outdated)",
        "Indicate affiliate relationship if applicable"
      ],
      "note": "AI shopping assistants help customers find products"
    },

    "generation": {
      "status": "conditional",
      "conditions": [
        "Product descriptions only (not marketing copy)",
        "Maintain factual accuracy",
        "Preserve brand guidelines",
        "Do not generate fake reviews or testimonials"
      ]
    },

    "fine_tuning": {
      "status": "conditional",
      "conditions": [
        "Shopping assistant use only",
        "No customer data training",
        "Preserve product accuracy"
      ],
      "purpose": [
        "Product recommendation systems",
        "Shopping chatbots",
        "Size/fit assistants"
      ]
    },

    "commercial_training": {
      "status": "conditional",
      "conditions": [
        "Competitor price scraping prohibited",
        "No customer data use",
        "Consider partnership for product data API",
        "Respect brand trademark rights"
      ]
    }
  },

  "enforcement": {
    "mechanism": "http_430",
    "contact_before_litigation": true,
    "preferred_resolution": "partnership or API licensing discussion",
    "note": "Competitive scraping and customer data violations will be pursued legally"
  },

  "metadata": {
    "organization": "E-commerce Retail Inc.",
    "created": "2025-07-20",
    "business_model": "Online retail"
  },

  "philosophy": {
    "core_principle": "We support AI shopping tools that help customers, but protect customer privacy and competitive business data.",
    "values": [
      "Customer privacy",
      "Fair competition",
      "Product discovery",
      "Brand integrity"
    ]
  }
}
```

---

## Implementation Notes

### How to Choose Your Scenario

Consider:
1. **Content Type**: Creative work, factual information, commercial data, personal data?
2. **Business Model**: Subscription, advertising, free access, commercial products?
3. **Legal Obligations**: HIPAA, FERPA, attorney-client privilege, copyright?
4. **Ethical Stance**: Open knowledge sharing vs. IP protection?
5. **Community Values**: Academic openness, artistic integrity, civic engagement?

### Customization Tips

1. **Start with a template** closest to your use case
2. **Adjust policy statuses** based on your specific boundaries
3. **Add conditions** that reflect your requirements
4. **Set appropriate scope** for different content sections
5. **Define exceptions** for special cases
6. **Update contact** and metadata fields
7. **Articulate your philosophy** to provide context

### Validation

Validate your manifest against the JSON Schema:

```bash
# Using ajv-cli
npm install -g ajv-cli
ajv validate -s ../../schemas/aibdp-schema-v0.2.json -d your-manifest.json

# Using Python jsonschema
python -c "
import json
import jsonschema
with open('../../schemas/aibdp-schema-v0.2.json') as schema_file:
    schema = json.load(schema_file)
with open('your-manifest.json') as manifest_file:
    manifest = json.load(manifest_file)
jsonschema.validate(manifest, schema)
print('Valid!')
"
```

---

## Contributing More Examples

Have a unique use case? Submit a PR with:
- Complete manifest JSON
- Scenario description
- Rationale for policy choices
- Implementation notes

---

_"Without refusal, permission is meaningless. These examples demonstrate that boundary-setting is as diverse as the web itself."_
