# rhodium-init: Ada TUI Design Specification

**Version**: 1.0.0
**Language**: Ada 2022
**Target**: Interactive terminal user interface for RSR repository scaffolding
**License**: MIT + Palimpsest v0.8

---

## Executive Summary

`rhodium-init` is an interactive TUI (Terminal User Interface) that guides users through creating **Rhodium Standard Repository (RSR)** compliant projects. Written in Ada 2022, it embodies RSR principles: type safety, formal verification potential, and offline-first capability.

---

## Design Principles

1. **Type Safety**: Ada 2022's strong typing prevents configuration errors at compile-time
2. **User Experience**: Clear prompts, helpful defaults, reversible choices
3. **Offline-First**: No network calls, all templates embedded or locally cached
4. **Validation**: Pre-flight checks ensure valid inputs before file generation
5. **Idempotent**: Running multiple times doesn't break existing repos
6. **Educational**: Explains WHY each choice matters (RSR compliance)

---

## Architecture

### High-Level Flow

```
┌─────────────────────────────────────────────────────┐
│  1. Welcome & Project Type Selection                │
├─────────────────────────────────────────────────────┤
│  2. Basic Information Gathering                     │
│     - Project name, description, language           │
│     - Repository URL, author details                │
├─────────────────────────────────────────────────────┤
│  3. RSR Configuration                               │
│     - Perimeter assignments (TPCF)                  │
│     - Security requirements                         │
│     - Licensing (MIT + Palimpsest v0.8)            │
├─────────────────────────────────────────────────────┤
│  4. Build System Selection                          │
│     - Justfile recipes                              │
│     - Nix flake configuration                       │
│     - CI/CD (GitLab)                                │
├─────────────────────────────────────────────────────┤
│  5. Documentation Setup                             │
│     - Required files (README, SECURITY, etc.)       │
│     - .well-known/ directory                        │
│     - Ethical guidelines                            │
├─────────────────────────────────────────────────────┤
│  6. Validation & Preview                            │
│     - Show file tree                                │
│     - Compliance checklist preview                  │
│     - Confirm generation                            │
├─────────────────────────────────────────────────────┤
│  7. File Generation                                 │
│     - Create directory structure                    │
│     - Render templates with user inputs             │
│     - Initialize Git repository                     │
│     - Set up hooks (RVC)                            │
├─────────────────────────────────────────────────────┤
│  8. Post-Generation Steps                           │
│     - `just validate` execution                     │
│     - Next steps guidance                           │
│     - Link to CLAUDE.md for details                │
└─────────────────────────────────────────────────────┘
```

---

## Data Structures

### Core Types

```ada
-- SPDX-License-Identifier: MIT AND Palimpsest-0.8
-- SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

package Rhodium.Init.Types is

   -- Project languages supported
   type Language_Type is (
      Ada_2022,
      Rust,
      Elixir,
      Haskell,
      ReScript,
      Nickel,
      Bash
   );

   -- TPCF perimeter assignments
   type Perimeter_Type is (
      Core_Systems,        -- Perimeter 1
      Expert_Extensions,   -- Perimeter 2
      Community_Sandbox    -- Perimeter 3
   );

   -- License options
   type License_Type is (
      MIT_Only,
      Palimpsest_Only,
      Dual_MIT_Palimpsest  -- Default
   );

   -- Build systems
   type Build_System is (
      Justfile,
      Make,
      Cargo,     -- Rust
      Mix,       -- Elixir
      GPRbuild,  -- Ada
      Cabal      -- Haskell
   );

   -- Project configuration record
   type Project_Config is record
      Name              : Unbounded_String;
      Description       : Unbounded_String;
      Language          : Language_Type;
      Version           : Unbounded_String := To_Unbounded_String("0.1.0");

      -- Author information
      Author_Name       : Unbounded_String;
      Author_Email      : Unbounded_String;
      Author_URL        : Unbounded_String;

      -- Repository details
      Repo_URL          : Unbounded_String;
      Repo_Group        : Unbounded_String;  -- GitLab group

      -- RSR configuration
      Primary_Perimeter : Perimeter_Type := Community_Sandbox;
      License           : License_Type := Dual_MIT_Palimpsest;
      Offline_First     : Boolean := True;
      Formal_Verification : Boolean := False;

      -- Build configuration
      Build             : Build_System;
      Use_Nix           : Boolean := True;
      Use_Podman        : Boolean := True;
      Use_GitLab_CI     : Boolean := True;

      -- Documentation
      Use_AsciiDoc      : Boolean := True;  -- True = .adoc, False = .md
      Generate_Examples : Boolean := True;

      -- Security
      SPDX_Required     : Boolean := True;
      Security_Email    : Unbounded_String;
      PGP_Key_ID        : Unbounded_String;

      -- Ethical considerations
      Prohibited_Uses   : Boolean := True;  -- Generate ETHICS.md
      AI_Training_Policy : Boolean := True;  -- Generate .well-known/ai.txt

      -- Target directory
      Target_Dir        : Unbounded_String;
   end record;

   -- Validation results
   type Validation_Result is record
      Valid             : Boolean;
      Error_Count       : Natural := 0;
      Warning_Count     : Natural := 0;
      Messages          : Unbounded_String_Vector;
   end record;

   -- Template rendering context
   type Template_Context is record
      Config            : Project_Config;
      Timestamp         : Ada.Calendar.Time;
      ISO_Date          : Unbounded_String;
      Replacements      : String_String_Map;  -- Key-value pairs for templates
   end record;

end Rhodium.Init.Types;
```

---

## User Interface Flows

### 1. Welcome Screen

```
┌────────────────────────────────────────────────────────────┐
│                                                            │
│   🏛️  RHODIUM-INIT: RSR Repository Scaffolding            │
│                                                            │
│   Version: 1.0.0                                           │
│   Language: Ada 2022                                       │
│   License: MIT + Palimpsest v0.8                          │
│                                                            │
│   Create a Rhodium Standard Repository with:              │
│   • Emotional safety (reversibility)                       │
│   • Technical excellence (formal verification)             │
│   • Political autonomy (offline-first)                     │
│   • Ethical grounding (Palimpsest License)                │
│                                                            │
│   Press ENTER to begin, or Q to quit                      │
│                                                            │
└────────────────────────────────────────────────────────────┘
```

---

### 2. Project Type Selection

```
┌────────────────────────────────────────────────────────────┐
│  Step 1/8: Select Project Type                            │
├────────────────────────────────────────────────────────────┤
│                                                            │
│  What kind of project are you creating?                   │
│                                                            │
│  1. Library (reusable code)                                │
│  2. Application (executable program)                       │
│  3. Service (backend API/daemon)                           │
│  4. Documentation (standards, guides)                      │
│  5. Infrastructure (build tools, configs)                  │
│                                                            │
│  Choice [1-5]: _                                           │
│                                                            │
│  ℹ️  This determines which templates are included.         │
│                                                            │
└────────────────────────────────────────────────────────────┘
```

---

### 3. Basic Information

```
┌────────────────────────────────────────────────────────────┐
│  Step 2/8: Basic Information                              │
├────────────────────────────────────────────────────────────┤
│                                                            │
│  Project name: ____________________                        │
│  (lowercase, hyphens OK, e.g. my-project)                 │
│                                                            │
│  One-line description: _______________________________     │
│  (Plain English, < 80 chars)                              │
│                                                            │
│  Primary language:                                         │
│  1. Ada 2022      (type-safe, SPARK verification)         │
│  2. Rust          (memory-safe, no GC)                    │
│  3. Elixir        (fault-tolerant, OTP)                   │
│  4. Haskell       (pure functional)                       │
│  5. ReScript      (OCaml → JS/WASM)                       │
│  6. Nickel        (typed configuration)                   │
│  7. Bash          (shell scripting)                       │
│                                                            │
│  Choice [1-7]: _                                           │
│                                                            │
│  Version [0.1.0]: _____                                    │
│                                                            │
└────────────────────────────────────────────────────────────┘
```

---

### 4. Author & Repository Information

```
┌────────────────────────────────────────────────────────────┐
│  Step 3/8: Author & Repository                            │
├────────────────────────────────────────────────────────────┤
│                                                            │
│  Your name: ___________________________                    │
│  (e.g. Jonathan D.A. Jewell)                              │
│                                                            │
│  Email: ___________________________                        │
│  (Public email for attribution)                           │
│                                                            │
│  GitLab username: ___________________________              │
│  (e.g. hyperpolymath)                                     │
│                                                            │
│  Repository URL: ___________________________               │
│  (e.g. https://gitlab.com/hyperpolymath/my-project)       │
│                                                            │
│  ℹ️  All files will include SPDX headers with your name.   │
│                                                            │
└────────────────────────────────────────────────────────────┘
```

---

### 5. RSR Configuration

```
┌────────────────────────────────────────────────────────────┐
│  Step 4/8: RSR Configuration                              │
├────────────────────────────────────────────────────────────┤
│                                                            │
│  License: [X] MIT + Palimpsest v0.8 (recommended)         │
│           [ ] MIT only                                     │
│           [ ] Palimpsest only                             │
│                                                            │
│  TPCF Primary Perimeter:                                  │
│  [ ] 🔒 Perimeter 1: Core Systems (maintainers only)      │
│  [ ] 🧠 Perimeter 2: Expert Extensions (trusted)          │
│  [X] 🌱 Perimeter 3: Community Sandbox (open)            │
│                                                            │
│  Security Contact Email: ___________________________       │
│  (For vulnerability reports)                              │
│                                                            │
│  PGP Key ID (optional): ___________________________        │
│                                                            │
│  [ ] Enable formal verification (SPARK/Coq)               │
│  [X] Offline-first architecture                           │
│  [X] Generate ETHICS.md (prohibited uses)                 │
│  [X] Generate AI training policy (.well-known/ai.txt)     │
│                                                            │
└────────────────────────────────────────────────────────────┘
```

---

### 6. Build System Configuration

```
┌────────────────────────────────────────────────────────────┐
│  Step 5/8: Build System                                   │
├────────────────────────────────────────────────────────────┤
│                                                            │
│  Build tool for Ada:                                       │
│  [X] GPRbuild (GNAT project files)                        │
│  [ ] Alire (Ada package manager)                          │
│                                                            │
│  Additional tooling:                                       │
│  [X] Justfile (task runner - recommended)                 │
│  [X] Nix flakes (reproducible builds)                     │
│  [X] Podman (containers, Chainguard Wolfi)                │
│  [X] GitLab CI/CD                                         │
│                                                            │
│  Generate sample recipes for:                             │
│  [X] build, test, lint, format                            │
│  [X] validate, audit-license, check-links                 │
│  [X] docs, deps-update, sbom-generate                     │
│                                                            │
└────────────────────────────────────────────────────────────┘
```

---

### 7. Documentation Preferences

```
┌────────────────────────────────────────────────────────────┐
│  Step 6/8: Documentation                                  │
├────────────────────────────────────────────────────────────┤
│                                                            │
│  Documentation format:                                     │
│  [X] AsciiDoc (.adoc) - recommended for RSR               │
│  [ ] Markdown (.md)   - simpler, less structured          │
│                                                            │
│  Generate these required files:                           │
│  [X] README.adoc                                          │
│  [X] LICENSE.txt (MIT + Palimpsest)                       │
│  [X] SECURITY.md                                          │
│  [X] CODE_OF_CONDUCT.adoc                                 │
│  [X] CONTRIBUTING.adoc                                    │
│  [X] FUNDING.yml                                          │
│  [X] GOVERNANCE.adoc                                      │
│  [X] REVERSIBILITY.md                                     │
│  [X] DEPENDENCIES.md                                      │
│  [X] ROADMAP.md                                           │
│  [X] COMPLIANCE.md                                        │
│  [X] ETHICS.md                                            │
│  [X] LEARNING.md                                          │
│  [X] FEEDBACK.md                                          │
│                                                            │
│  Generate .well-known/ directory:                         │
│  [X] security.txt, ai.txt, provenance.json, humans.txt    │
│                                                            │
│  [X] Generate example code/documentation                  │
│                                                            │
└────────────────────────────────────────────────────────────┘
```

---

### 8. Validation & Preview

```
┌────────────────────────────────────────────────────────────┐
│  Step 7/8: Validation & Preview                           │
├────────────────────────────────────────────────────────────┤
│                                                            │
│  ✅ Project name valid: my-ada-project                     │
│  ✅ Repository URL valid                                   │
│  ✅ Email format valid                                     │
│  ✅ Target directory writable: ./my-ada-project/           │
│                                                            │
│  Directory structure to be created:                        │
│  my-ada-project/                                           │
│  ├── README.adoc                                           │
│  ├── LICENSE.txt                                           │
│  ├── SECURITY.md                                           │
│  ├── CODE_OF_CONDUCT.adoc                                  │
│  ├── CONTRIBUTING.adoc                                     │
│  ├── FUNDING.yml                                           │
│  ├── GOVERNANCE.adoc                                       │
│  ├── REVERSIBILITY.md                                      │
│  ├── .well-known/                                          │
│  │   ├── security.txt                                      │
│  │   ├── ai.txt                                            │
│  │   ├── provenance.json                                   │
│  │   └── humans.txt                                        │
│  ├── src/                                                  │
│  ├── bin/                                                  │
│  ├── docs/                                                 │
│  ├── justfile                                              │
│  ├── flake.nix                                             │
│  ├── .gitlab-ci.yml                                        │
│  └── my_ada_project.gpr                                    │
│                                                            │
│  Total files: 45                                           │
│                                                            │
│  Proceed with generation? [Y/n]: _                        │
│                                                            │
└────────────────────────────────────────────────────────────┘
```

---

### 9. Generation Progress

```
┌────────────────────────────────────────────────────────────┐
│  Step 8/8: Generating Repository                          │
├────────────────────────────────────────────────────────────┤
│                                                            │
│  [████████████████████████████████████      ] 90%          │
│                                                            │
│  ✅ Created directory structure                            │
│  ✅ Generated documentation (15 files)                     │
│  ✅ Generated .well-known/ directory (5 files)             │
│  ✅ Generated build files (justfile, flake.nix, CI/CD)     │
│  ✅ Initialized Git repository                             │
│  ✅ Set up Git hooks (RVC pre-commit)                      │
│  ⏳ Running `just validate`...                             │
│                                                            │
└────────────────────────────────────────────────────────────┘
```

---

### 10. Completion & Next Steps

```
┌────────────────────────────────────────────────────────────┐
│  ✨ Success! RSR-compliant repository created              │
├────────────────────────────────────────────────────────────┤
│                                                            │
│  📁 Location: ./my-ada-project/                            │
│  🏆 RSR Compliance: Gold (100%)                            │
│  📝 Files created: 45                                      │
│  🔐 License: MIT + Palimpsest v0.8                        │
│                                                            │
│  Next steps:                                               │
│                                                            │
│  1. cd my-ada-project                                      │
│  2. Review generated files, especially:                    │
│     - README.adoc (customize project description)          │
│     - SECURITY.md (add security contact)                   │
│     - FUNDING.yml (add funding links)                      │
│  3. Customize placeholders marked with *REMINDER*          │
│  4. Run `just validate` to verify compliance               │
│  5. Commit initial state: git commit -m "chore: initial"  │
│  6. Add remote: git remote add origin <repo-url>          │
│  7. Push: git push -u origin main                         │
│                                                            │
│  📖 Full RSR specification: CLAUDE.md                      │
│  📋 Compliance checklist: COMPLIANCE_CHECKLIST.md          │
│  📜 Philosophy: CCCP-MANIFESTO.md                          │
│                                                            │
│  🤝 Need help? https://gitlab.com/hyperpolymath/rhodium    │
│                                                            │
│  Press ENTER to exit                                       │
│                                                            │
└────────────────────────────────────────────────────────────┘
```

---

## Implementation Details

### Package Structure

```ada
-- Top-level package
package Rhodium.Init is
   procedure Run;
end Rhodium.Init;

-- Type definitions
package Rhodium.Init.Types is
   -- (See above)
end Rhodium.Init.Types;

-- User interface
package Rhodium.Init.UI is
   procedure Show_Welcome;
   procedure Show_Progress (Percent : Natural; Message : String);
   function Prompt_Text (Prompt : String; Default : String := "") return String;
   function Prompt_Choice (Prompt : String; Choices : String_Array) return Natural;
   function Prompt_Boolean (Prompt : String; Default : Boolean := True) return Boolean;
   procedure Show_Completion (Config : Project_Config; File_Count : Natural);
end Rhodium.Init.UI;

-- Template rendering
package Rhodium.Init.Templates is
   procedure Render_Template (
      Template_Name : String;
      Context : Template_Context;
      Output_File : String
   );

   procedure Generate_All_Templates (Config : Project_Config);
end Rhodium.Init.Templates;

-- Validation
package Rhodium.Init.Validation is
   function Validate_Project_Name (Name : String) return Validation_Result;
   function Validate_Email (Email : String) return Validation_Result;
   function Validate_URL (URL : String) return Validation_Result;
   function Validate_Config (Config : Project_Config) return Validation_Result;
end Rhodium.Init.Validation;

-- File operations
package Rhodium.Init.Files is
   procedure Create_Directory_Structure (Config : Project_Config);
   procedure Initialize_Git_Repo (Target_Dir : String);
   procedure Setup_Git_Hooks (Target_Dir : String);
   procedure Run_Just_Validate (Target_Dir : String);
end Rhodium.Init.Files;
```

---

## Template Rendering Engine

### Strategy

Templates are embedded in the binary as Ada constants (offline-first):

```ada
package Rhodium.Init.Embedded_Templates is

   -- README template
   README_ADOC_TEMPLATE : constant String :=
      "= {project-name}" & ASCII.LF &
      ":project-name: {project-name}" & ASCII.LF &
      ":project-description: {project-description}" & ASCII.LF &
      -- ... (full template)
      ;

   -- SECURITY template
   SECURITY_MD_TEMPLATE : constant String :=
      "# Security Policy" & ASCII.LF &
      "" & ASCII.LF &
      "## 🔒 Reporting a Vulnerability" & ASCII.LF &
      -- ... (full template)
      ;

   -- ... (all templates)

end Rhodium.Init.Embedded_Templates;
```

### Replacement Logic

```ada
function Replace_Placeholders (
   Template : String;
   Replacements : String_String_Map
) return String is
   Result : Unbounded_String := To_Unbounded_String(Template);
begin
   for Cursor in Replacements.Iterate loop
      declare
         Key : constant String := String_String_Maps.Key(Cursor);
         Value : constant String := String_String_Maps.Element(Cursor);
         Placeholder : constant String := "{" & Key & "}";
      begin
         Result := Unbounded_Slice.Replace (
            Source => Result,
            Pattern => Placeholder,
            By => Value
         );
      end loop;
   end loop;

   return To_String(Result);
end Replace_Placeholders;
```

---

## Error Handling

### SPARK Contracts (Optional)

```ada
procedure Validate_Project_Name (Name : String; Valid : out Boolean)
   with Pre  => Name'Length > 0,
        Post => (if Valid then Name'Length <= 100);
```

### User-Friendly Error Messages

```
❌ Error: Invalid project name "My Project!"

Project names must:
• Be lowercase (use hyphens for spaces)
• Contain only letters, numbers, hyphens
• Start with a letter
• Be between 3-100 characters

Examples:
✅ my-project
✅ cool-rust-tool
✅ ada-formatter-2024

Try again: _
```

---

## Testing Strategy

### Unit Tests

```ada
package body Rhodium.Init.Tests is

   procedure Test_Validate_Project_Name is
      Result : Validation_Result;
   begin
      -- Valid names
      Result := Validate_Project_Name("my-project");
      Assert(Result.Valid, "Should accept lowercase with hyphens");

      Result := Validate_Project_Name("project123");
      Assert(Result.Valid, "Should accept numbers");

      -- Invalid names
      Result := Validate_Project_Name("My Project");
      Assert(not Result.Valid, "Should reject spaces");

      Result := Validate_Project_Name("project!");
      Assert(not Result.Valid, "Should reject special characters");
   end Test_Validate_Project_Name;

end Rhodium.Init.Tests;
```

### Integration Tests

```bash
# Test full generation
./rhodium-init --non-interactive \
  --name "test-project" \
  --language ada \
  --author "Test Author" \
  --email "test@example.com" \
  --repo "https://gitlab.com/test/test-project"

# Verify RSR compliance
cd test-project
just validate  # Should pass 100%
```

---

## Build & Distribution

### GPRbuild Project File

```ada
-- rhodium_init.gpr
project Rhodium_Init is

   for Source_Dirs use ("src");
   for Object_Dir use "obj";
   for Exec_Dir use "bin";
   for Main use ("rhodium_init.adb");

   package Compiler is
      for Switches ("Ada") use (
         "-gnatwa",        -- All warnings
         "-gnatwe",        -- Warnings as errors
         "-gnat2022",      -- Ada 2022
         "-O2",            -- Optimization
         "-gnato",         -- Overflow checks
         "-gnatVa"         -- All validity checks
      );
   end Compiler;

   package Binder is
      for Switches ("Ada") use ("-Es");  -- Symbolic traceback
   end Binder;

end Rhodium_Init;
```

### Justfile for Development

```just
# Build rhodium-init
build:
    gprbuild -P rhodium_init.gpr

# Build with SPARK verification
build-spark:
    gnatprove -P rhodium_init.gpr --level=2

# Run tests
test:
    gprbuild -P rhodium_init_test.gpr
    ./bin/rhodium_init_test

# Install system-wide
install: build
    sudo cp bin/rhodium-init /usr/local/bin/
    @echo "✓ Installed rhodium-init to /usr/local/bin/"
```

---

## Distribution Formats

1. **Source**: `.tar.gz` with GPRbuild files
2. **Binary**: Statically-linked executable for Linux/macOS/Windows
3. **Nix**: Flake for `nix run rhodium-init`
4. **Alire**: Ada package manager integration
5. **GitLab Release**: Attached to tags with SBOM

---

## Future Enhancements

### Phase 2 Features

- [ ] **Template customization**: User-defined templates
- [ ] **Project migration**: Convert existing repos to RSR
- [ ] **Update wizard**: Update existing RSR repos to latest standards
- [ ] **Language plugins**: Extend to more languages dynamically
- [ ] **Web UI**: Browser-based version (Elm, see next section)
- [ ] **IDE integration**: VSCode/Neovim plugins

### Phase 3 Features

- [ ] **AI assistance**: GPT-4 integration for custom prompts
- [ ] **Dependency analysis**: Scan existing code, suggest RSR-compliant alternatives
- [ ] **Compliance reporting**: Generate PDF reports for audits
- [ ] **Team templates**: Shared organizational templates

---

## Success Metrics

**Adoption**:
- 100 repositories created with rhodium-init in first 6 months
- 10 external organizations using rhodium-init

**Quality**:
- 95%+ of generated repos pass `just validate`
- < 5% of users need to manually fix generated files

**User Experience**:
- Average completion time: < 5 minutes
- User satisfaction: > 4.5/5.0

---

## Documentation

- **User Guide**: `docs/user-guide.adoc`
- **Developer Guide**: `docs/developer-guide.adoc`
- **API Reference**: Generated with GNATdoc
- **Video Tutorial**: Screen recording of full workflow

---

## Contact

- **Issues**: https://gitlab.com/hyperpolymath/rhodium-init/-/issues
- **Discussions**: https://gitlab.com/hyperpolymath/rhodium-init/-/discussions
- **Email**: rhodium@hyperpolymath.org

---

*"Making RSR compliance effortless, one project at a time."*

— rhodium-init
