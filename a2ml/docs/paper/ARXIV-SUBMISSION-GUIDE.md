# arXiv Submission Guide: A2ML Paper

**Paper:** A2ML: A Lightweight Markup Language with Formal Proof Obligations
**Author:** Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
**Submission Package:** `a2ml-arxiv-submission.tar.gz` (5.9 KB)

---

## Pre-Submission Checklist

- [x] LaTeX source prepared (`a2ml-arxiv.tex`)
- [x] PDF compiled successfully (`a2ml-arxiv.pdf`, 74 KB)
- [x] Submission package created (`a2ml-arxiv-submission.tar.gz`)
- [x] Author metadata correct (Jonathan D.A. Jewell, The Open University)
- [x] Bibliography included (inline, no external .bib file)
- [ ] arXiv account created (if not already)
- [ ] Subject classification selected (see below)
- [ ] Submission uploaded

---

## Step-by-Step Submission Process

### 1. Create arXiv Account (if needed)

**URL:** https://arxiv.org/user/login

- Click "Register" if you don't have an account
- Use your academic email: `jonathan.jewell@open.ac.uk`
- Verify email address
- Complete profile (name, affiliation, etc.)

### 2. Start New Submission

**URL:** https://arxiv.org/submit

- Log in to your arXiv account
- Click "Start New Submission"
- Select submission type: **New submission**

### 3. Select Archive and Subject Class

**Primary Archive:** `cs` (Computer Science)

**Primary Subject Classification:** Choose ONE primary:

- **cs.PL** - Programming Languages (RECOMMENDED)
  *Best fit: A2ML is a markup language with formal verification in Idris2*

**Secondary Subject Classifications:** (optional, can add 1-2)

- **cs.LO** - Logic in Computer Science
  *Relevant: Formal verification, dependent types, proof obligations*

- **cs.SE** - Software Engineering
  *Relevant: Document engineering, specifications, standards*

**Recommendation:** Use **cs.PL** as primary, **cs.LO** as secondary.

### 4. Upload Files

**File Upload Method:** Upload `.tar.gz` archive

1. Click "Choose File"
2. Select: `~/Documents/hyperpolymath-repos/a2ml/docs/paper/a2ml-arxiv-submission.tar.gz`
3. Click "Upload Files"
4. arXiv will automatically extract and process the archive

**Expected Processing:**

- arXiv extracts `a2ml-arxiv.tex`
- Compiles with `pdflatex` (may take 30-60 seconds)
- Generates preview PDF for verification

**If compilation succeeds:** Proceed to metadata

**If compilation fails:** Check error log, fix .tex file, re-upload

### 5. Enter Metadata

**Title:**
```
A2ML: A Lightweight Markup Language with Formal Proof Obligations
```

**Authors:**
```
Jonathan D.A. Jewell
```

**Affiliation:**
```
The Open University
```

**Abstract:** (copy from paper or use this)
```
We present A2ML (Attested Markup Language), a lightweight markup format that combines ease of authoring with formal verification guarantees. Unlike existing markup languages (Markdown, AsciiDoc, reStructuredText), A2ML provides a typed core with decidable proof obligations implemented in Idris2. Documents can be validated in three modes: lax (permissive authoring), checked (structural validation), and attested (formal proofs required). We demonstrate that A2ML's hybrid architecture—a Djot-inspired surface syntax compiled to a dependently-typed core—enables progressive strictness without sacrificing usability. Our implementation includes a formally verified parser that compiles to JavaScript (45KB) and a ReScript-based GUI. Benchmarks show A2ML parsing performance competitive with Markdown while providing guarantees unattainable in traditional markup languages. A2ML is designed for documents requiring structural invariants: academic papers, technical specifications, and standards documents.
```

**Comments:** (optional, internal notes for arXiv moderators)
```
This paper introduces A2ML, a new markup language with formal verification capabilities. Related to programming languages (Idris2), logic (dependent types), and document engineering.
```

**Report Number:** Leave blank (unless you have an institutional report number)

**Journal Reference:** Leave blank (this is a new submission, not previously published)

**DOI:** Leave blank (arXiv will assign one after acceptance)

### 6. License Selection

**Recommended License:**

- **arXiv.org perpetual, non-exclusive license to distribute this article**
  *(Standard arXiv license, allows arXiv to distribute indefinitely)*

**OR (if you prefer Creative Commons):**

- **CC BY 4.0** (Creative Commons Attribution)
  *Allows anyone to share/adapt with attribution*

**Recommendation:** Use standard arXiv license (first option). This is most common for academic papers and matches PMPL-1.0 philosophy.

### 7. Review and Submit

1. **Preview PDF:** arXiv shows compiled PDF - verify it matches your local `a2ml-arxiv.pdf`
2. **Check metadata:** Review title, authors, abstract, classification
3. **Submit for processing:** Click "Submit to arXiv"

**After submission:**

- arXiv assigns a submission ID (e.g., `2601.12345`)
- Paper enters moderation queue (typically 24-48 hours)
- You'll receive email confirmation

### 8. Moderation Process

**What happens:**

- arXiv moderators review paper for:
  - Topic relevance to selected archive (cs.PL)
  - Quality and clarity
  - Proper LaTeX formatting
  - No obvious errors or spam

**Possible outcomes:**

1. **Accepted** - Paper published, gets arXiv ID (e.g., `arXiv:2601.12345`)
2. **Reclassified** - Moderators suggest different subject classification
3. **Put on hold** - Request clarifications or corrections
4. **Rejected** - Rare, usually for spam or off-topic submissions

**Timeline:** 1-3 business days (usually 24-48 hours)

### 9. After Acceptance

**You'll receive:**

- arXiv ID (e.g., `arXiv:2601.12345`)
- Permanent URL (e.g., `https://arxiv.org/abs/2601.12345`)
- PDF link (e.g., `https://arxiv.org/pdf/2601.12345.pdf`)
- Announcement timestamp (papers announced daily at 20:00 EST)

**Next steps:**

1. **Share the arXiv link:**
   - Add to A2ML README.adoc
   - Share on social media, HN, Reddit, etc.
   - Email to relevant communities (Nickel, Idris2, markup language enthusiasts)

2. **Update repository:**
   - Add arXiv badge to README: `[![arXiv](https://img.shields.io/badge/arXiv-2601.12345-b31b1b.svg)](https://arxiv.org/abs/2601.12345)`
   - Link from documentation

3. **Consider follow-up venues:**
   - Conference submission (e.g., PLDI, ICFP, POPL)
   - Workshop (e.g., TyDe, ML Family Workshop)
   - Journal (e.g., JFP, TOPLAS)

---

## Troubleshooting

### Compilation Fails

**Error:** `! LaTeX Error: File 'X.sty' not found`

**Fix:** arXiv has most standard packages. If using obscure packages, either:
- Remove the package if not essential
- Include the `.sty` file in submission archive

**Error:** `! Undefined control sequence`

**Fix:** Check that all custom commands are defined in the preamble.

### Paper Put on Hold

**Reason:** Moderators may request:
- Better abstract (more specific about contributions)
- Clearer subject classification
- Correction of formatting issues

**Action:** Respond to moderator email with requested changes, re-submit.

### Paper Reclassified

**Example:** Submitted to `cs.PL`, moderators suggest `cs.SE`

**Action:** Accept reclassification or provide justification for original choice.

---

## Post-Submission TODO

After arXiv acceptance:

- [ ] Update `a2ml/README.adoc` with arXiv badge and link
- [ ] Update `a2ml/docs/IANA-MEDIA-TYPE.adoc` to reference arXiv paper
- [ ] Add arXiv link to A2ML website (when created)
- [ ] Share on social media (Twitter/X, Mastodon, LinkedIn)
- [ ] Post to Hacker News (https://news.ycombinator.com/submit)
- [ ] Post to Reddit r/ProgrammingLanguages
- [ ] Email Nickel community (Discord/GitHub discussions)
- [ ] Email Idris2 community (Discord/Discourse)
- [ ] Consider submitting to conference (PLDI, ICFP, POPL deadlines)

---

## Quick Reference

| Item | Value |
|------|-------|
| **Title** | A2ML: A Lightweight Markup Language with Formal Proof Obligations |
| **Author** | Jonathan D.A. Jewell |
| **Affiliation** | The Open University |
| **Email** | jonathan.jewell@open.ac.uk |
| **Primary Subject** | cs.PL (Programming Languages) |
| **Secondary Subject** | cs.LO (Logic in Computer Science) |
| **Submission Package** | `a2ml-arxiv-submission.tar.gz` (5.9 KB) |
| **PDF Size** | 74 KB |
| **Estimated Review Time** | 1-3 business days |

---

## Files in This Directory

```
~/Documents/hyperpolymath-repos/a2ml/docs/paper/
├── a2ml-arxiv.tex                    # LaTeX source
├── a2ml-arxiv.pdf                    # Compiled PDF (74 KB)
├── a2ml-arxiv-submission.tar.gz      # Submission package (5.9 KB) ← UPLOAD THIS
├── Makefile                          # Build script
└── ARXIV-SUBMISSION-GUIDE.md         # This file
```

---

**Ready to submit!** Go to https://arxiv.org/submit and follow the steps above.

Good luck! 🚀
