# Rhodium Web UI: Elm GUI Design Specification

**Version**: 1.0.0
**Language**: Elm 0.19.1
**Architecture**: The Elm Architecture (TEA)
**Purpose**: Web-based interface for RSR repository creation and management
**License**: MIT + Palimpsest v0.8

---

## Executive Summary

The **Rhodium Web UI** provides a browser-based alternative to `rhodium-init` (Ada TUI), making RSR-compliant repository creation accessible to users who prefer graphical interfaces. Built with Elm for **type safety, no runtime exceptions, and delightful UX**.

---

## Why Elm?

### Alignment with RSR/CCCP Principles

| Principle | How Elm Delivers |
|-----------|------------------|
| **Type Safety** | Pure functional, Hindley-Milner type system, no `null`/`undefined` |
| **No Runtime Exceptions** | Compiler guarantees (if it compiles, it works) |
| **Reversibility** | Time-traveling debugger, immutable data structures |
| **Offline-First** | Service Workers, LocalStorage, works without backend |
| **Post-JavaScript** | Compiles to JavaScript but provides sound types |
| **Accessibility** | Semantic HTML generation, WCAG 2.1 AA compliant |

### Elm vs. Alternatives

| Framework | Type Safety | Runtime Exceptions | Bundle Size | Verdict |
|-----------|-------------|-------------------|-------------|---------|
| **Elm** | ✅ Sound | ❌ None (guaranteed) | 30KB gzip | ✅ Best for RSR |
| React + TS | ⚠️ Unsound | ✅ Many | 45KB+ | ❌ TypeScript unsound |
| Vue | ❌ None | ✅ Many | 35KB+ | ❌ No type safety |
| Svelte | ⚠️ Optional | ✅ Some | 20KB | ⚠️ Better than React, but less safe than Elm |
| ReScript | ✅ Sound (OCaml) | ❌ Minimal | 25KB | ✅ Alternative option |

**Decision**: **Elm** for soundness + simplicity.

---

## Architecture

### The Elm Architecture (TEA)

```
┌─────────────────────────────────────────────────────────┐
│  Model (Application State)                              │
├─────────────────────────────────────────────────────────┤
│  • Project configuration                                │
│  • Current step in wizard                               │
│  • Validation results                                   │
│  • Generated file preview                               │
└──────────────┬──────────────────────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────────────────────┐
│  Update (State Transitions)                             │
├─────────────────────────────────────────────────────────┤
│  • Handle user input (Msg)                              │
│  • Validate data                                        │
│  • Update model immutably                               │
│  • Trigger side effects (Cmd)                           │
└──────────────┬──────────────────────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────────────────────┐
│  View (Render HTML)                                     │
├─────────────────────────────────────────────────────────┤
│  • Pure function: Model → Html Msg                      │
│  • Semantic HTML                                        │
│  • Accessible (ARIA labels, keyboard nav)               │
│  • Responsive (mobile-first)                            │
└─────────────────────────────────────────────────────────┘
```

---

## Data Model

```elm
-- SPDX-License-Identifier: MIT AND Palimpsest-0.8
-- SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

module Rhodium.Types exposing (..)

-- Primary language options
type Language
    = Ada2022
    | Rust
    | Elixir
    | Haskell
    | ReScript
    | Nickel
    | Bash


-- TPCF perimeters
type Perimeter
    = CoreSystems          -- Perimeter 1
    | ExpertExtensions     -- Perimeter 2
    | CommunitySandbox     -- Perimeter 3


-- License options
type License
    = MITOnly
    | PalimpsestOnly
    | DualMITPalimpsest    -- Default


-- Project configuration
type alias ProjectConfig =
    { name : String
    , description : String
    , language : Language
    , version : String  -- Default: "0.1.0"

    -- Author
    , authorName : String
    , authorEmail : String
    , authorURL : String

    -- Repository
    , repoURL : String
    , repoGroup : String

    -- RSR configuration
    , primaryPerimeter : Perimeter
    , license : License
    , offlineFirst : Bool
    , formalVerification : Bool

    -- Build
    , buildSystem : BuildSystem
    , useNix : Bool
    , usePodman : Bool
    , useGitLabCI : Bool

    -- Documentation
    , useAsciiDoc : Bool
    , generateExamples : Bool

    -- Security
    , spdxRequired : Bool
    , securityEmail : String
    , pgpKeyID : Maybe String

    -- Ethical
    , prohibitedUses : Bool
    , aiTrainingPolicy : Bool

    -- Target
    , targetDir : String
    }


-- Build systems (language-specific)
type BuildSystem
    = Justfile
    | Make
    | Cargo      -- Rust
    | Mix        -- Elixir
    | GPRbuild   -- Ada
    | Cabal      -- Haskell


-- Wizard steps
type WizardStep
    = Welcome
    | ProjectType
    | BasicInfo
    | AuthorInfo
    | RSRConfig
    | BuildConfig
    | DocConfig
    | ValidationPreview
    | Generation
    | Completion


-- Validation results
type alias ValidationResult =
    { valid : Bool
    , errors : List String
    , warnings : List String
    }


-- Application state
type alias Model =
    { config : ProjectConfig
    , currentStep : WizardStep
    , validation : ValidationResult
    , generatedFiles : List GeneratedFile
    , generationProgress : Int  -- 0-100%
    }


-- Generated file preview
type alias GeneratedFile =
    { path : String
    , content : String
    , size : Int  -- bytes
    }


-- Messages (user actions)
type Msg
    = -- Navigation
      NextStep
    | PreviousStep
    | GoToStep WizardStep

    -- Input changes
    | UpdateProjectName String
    | UpdateDescription String
    | SelectLanguage Language
    | UpdateAuthorName String
    | UpdateAuthorEmail String
    | UpdateRepoURL String
    | ToggleUseNix
    | ToggleFormalVerification

    -- Actions
    | ValidateConfig
    | GenerateRepository
    | DownloadZip
    | CopyToClipboard String

    -- Side effects results
    | ValidationComplete ValidationResult
    | GenerationProgress Int
    | GenerationComplete (List GeneratedFile)
    | DownloadReady String  -- ZIP blob URL
```

---

## User Interface Design

### Design System

**Colors** (WCAG 2.1 AA compliant):
- Primary: `#2D3142` (Dark slate)
- Secondary: `#BFC0C0` (Light grey)
- Accent: `#EF8354` (Coral - RSR brand color)
- Success: `#4F772D` (Green)
- Warning: `#F4A261` (Orange)
- Error: `#C9404D` (Red)
- Background: `#FAFAFA` (Off-white)

**Typography**:
- Headings: `Inter` (sans-serif, 600 weight)
- Body: `Inter` (sans-serif, 400 weight)
- Code: `JetBrains Mono` (monospace)

**Spacing**: 8px grid (8, 16, 24, 32, 48, 64)

---

### 1. Welcome Screen

```elm
viewWelcome : Html Msg
viewWelcome =
    section [ class "welcome-screen" ]
        [ div [ class "hero" ]
            [ h1 [] [ text "🏛️ Rhodium Web UI" ]
            , p [ class "tagline" ]
                [ text "Create RSR-compliant repositories with ease" ]
            ]
        , div [ class "features" ]
            [ featureCard "🔐" "Type-Safe" "Elm's compiler eliminates runtime errors"
            , featureCard "📴" "Offline-First" "Works without internet after first load"
            , featureCard "♿" "Accessible" "WCAG 2.1 AA compliant by default"
            , featureCard "🎨" "Beautiful" "Delightful UX inspired by best practices"
            ]
        , button
            [ class "cta-button"
            , onClick NextStep
            ]
            [ text "Create RSR Repository →" ]
        , footer []
            [ a [ href "https://gitlab.com/hyperpolymath/rhodium" ]
                [ text "Documentation" ]
            , text " | "
            , a [ href "/cccp-manifesto" ]
                [ text "CCCP Manifesto" ]
            ]
        ]
```

**Visual**:
```
┌─────────────────────────────────────────────────────────────┐
│                                                             │
│               🏛️ Rhodium Web UI                             │
│                                                             │
│     Create RSR-compliant repositories with ease             │
│                                                             │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐   │
│  │   🔐     │  │   📴     │  │   ♿     │  │   🎨     │   │
│  │ Type-Safe│  │ Offline  │  │Accessible│  │Beautiful │   │
│  │          │  │  First   │  │          │  │          │   │
│  └──────────┘  └──────────┘  └──────────┘  └──────────┘   │
│                                                             │
│          [ Create RSR Repository → ]                        │
│                                                             │
│  Documentation | CCCP Manifesto                             │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

---

### 2. Project Information Form

```elm
viewBasicInfo : ProjectConfig -> Html Msg
viewBasicInfo config =
    form [ class "wizard-form", onSubmit NextStep ]
        [ h2 [] [ text "Project Information" ]
        , p [ class "step-indicator" ]
            [ text "Step 2 of 8" ]

        -- Project name
        , div [ class "form-group" ]
            [ label [ for "project-name" ]
                [ text "Project Name"
                , span [ class "required" ] [ text "*" ]
                ]
            , input
                [ id "project-name"
                , type_ "text"
                , value config.name
                , onInput UpdateProjectName
                , placeholder "my-awesome-project"
                , attribute "pattern" "[a-z0-9-]+"
                , attribute "aria-describedby" "name-help"
                ]
                []
            , small [ id "name-help", class "help-text" ]
                [ text "Lowercase letters, numbers, and hyphens only" ]
            ]

        -- Description
        , div [ class "form-group" ]
            [ label [ for "description" ]
                [ text "One-line description"
                , span [ class "required" ] [ text "*" ]
                ]
            , input
                [ id "description"
                , type_ "text"
                , value config.description
                , onInput UpdateDescription
                , placeholder "A fantastic tool for..."
                , maxlength 80
                ]
                []
            , small [ class "help-text" ]
                [ text (String.fromInt (80 - String.length config.description))
                , text " characters remaining"
                ]
            ]

        -- Language selection
        , div [ class "form-group" ]
            [ label [] [ text "Primary Language" ]
            , div [ class "language-grid" ]
                (List.map (languageButton config.language) allLanguages)
            ]

        -- Navigation
        , div [ class "wizard-nav" ]
            [ button
                [ type_ "button"
                , class "btn-secondary"
                , onClick PreviousStep
                ]
                [ text "← Back" ]
            , button
                [ type_ "submit"
                , class "btn-primary"
                ]
                [ text "Next →" ]
            ]
        ]


languageButton : Language -> Language -> Html Msg
languageButton selected lang =
    button
        [ type_ "button"
        , classList
            [ ( "language-card", True )
            , ( "selected", selected == lang )
            ]
        , onClick (SelectLanguage lang)
        ]
        [ div [ class "lang-icon" ] [ text (languageIcon lang) ]
        , div [ class "lang-name" ] [ text (languageName lang) ]
        , div [ class "lang-desc" ] [ text (languageDescription lang) ]
        ]


languageIcon : Language -> String
languageIcon lang =
    case lang of
        Ada2022 -> "🛡️"
        Rust -> "🦀"
        Elixir -> "💧"
        Haskell -> "λ"
        ReScript -> "🎯"
        Nickel -> "⚙️"
        Bash -> "🐚"
```

**Visual**:
```
┌─────────────────────────────────────────────────────────────┐
│  Project Information                        Step 2 of 8     │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  Project Name *                                             │
│  ┌─────────────────────────────────────────────────────┐   │
│  │ my-awesome-project                                  │   │
│  └─────────────────────────────────────────────────────┘   │
│  Lowercase letters, numbers, and hyphens only               │
│                                                             │
│  One-line description *                                     │
│  ┌─────────────────────────────────────────────────────┐   │
│  │ A fantastic tool for...                             │   │
│  └─────────────────────────────────────────────────────┘   │
│  65 characters remaining                                    │
│                                                             │
│  Primary Language                                           │
│  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐       │
│  │  🛡️ Ada  │  │ 🦀 Rust │  │ 💧 Elixir│  │ λ Haskell│      │
│  │  SPARK   │  │ Memory  │  │ Fault   │  │  Pure    │       │
│  │  proofs  │  │  safety │  │tolerance│  │functional│       │
│  └─────────┘  └─────────┘  └─────────┘  └─────────┘       │
│                                                             │
│  [ ← Back ]                              [ Next → ]        │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

---

### 3. Validation & Preview

```elm
viewValidationPreview : Model -> Html Msg
viewValidationPreview model =
    div [ class "validation-preview" ]
        [ h2 [] [ text "Validation & Preview" ]
        , p [ class "step-indicator" ] [ text "Step 7 of 8" ]

        -- Validation results
        , section [ class "validation-results" ]
            [ h3 [] [ text "Validation" ]
            , if model.validation.valid then
                div [ class "alert alert-success" ]
                    [ text "✅ All checks passed!" ]
              else
                div []
                    [ div [ class "alert alert-error" ]
                        [ text ("❌ " ++ String.fromInt (List.length model.validation.errors) ++ " errors found") ]
                    , ul [] (List.map viewError model.validation.errors)
                    ]

            , if not (List.isEmpty model.validation.warnings) then
                div []
                    [ div [ class "alert alert-warning" ]
                        [ text ("⚠️ " ++ String.fromInt (List.length model.validation.warnings) ++ " warnings") ]
                    , ul [] (List.map viewWarning model.validation.warnings)
                    ]
              else
                text ""
            ]

        -- Directory structure preview
        , section [ class "structure-preview" ]
            [ h3 [] [ text "Directory Structure" ]
            , pre [ class "directory-tree" ]
                [ code [] [ text (generateDirectoryTree model.config) ] ]
            ]

        -- File count summary
        , section [ class "summary" ]
            [ div [ class "stat-card" ]
                [ div [ class "stat-number" ] [ text "45" ]
                , div [ class "stat-label" ] [ text "Files" ]
                ]
            , div [ class "stat-card" ]
                [ div [ class "stat-number" ] [ text "100%" ]
                , div [ class "stat-label" ] [ text "RSR Compliance" ]
                ]
            , div [ class "stat-card" ]
                [ div [ class "stat-number" ] [ text "MIT + Palimpsest" ]
                , div [ class "stat-label" ] [ text "License" ]
                ]
            ]

        -- Navigation
        , div [ class "wizard-nav" ]
            [ button
                [ type_ "button"
                , class "btn-secondary"
                , onClick PreviousStep
                ]
                [ text "← Back" ]
            , button
                [ type_ "button"
                , class "btn-primary"
                , onClick GenerateRepository
                , disabled (not model.validation.valid)
                ]
                [ text "Generate Repository →" ]
            ]
        ]
```

---

### 4. Generation Progress

```elm
viewGeneration : Model -> Html Msg
viewGeneration model =
    div [ class "generation-screen" ]
        [ h2 [] [ text "Generating Repository" ]
        , div [ class "progress-container" ]
            [ div [ class "progress-bar" ]
                [ div
                    [ class "progress-fill"
                    , style "width" (String.fromInt model.generationProgress ++ "%")
                    ]
                    []
                ]
            , div [ class "progress-text" ]
                [ text (String.fromInt model.generationProgress ++ "%") ]
            ]

        -- Task list
        , ul [ class "task-list" ]
            [ taskItem model.generationProgress 10 "Created directory structure"
            , taskItem model.generationProgress 30 "Generated documentation"
            , taskItem model.generationProgress 50 "Generated .well-known/ directory"
            , taskItem model.generationProgress 70 "Generated build files"
            , taskItem model.generationProgress 85 "Initialized Git repository"
            , taskItem model.generationProgress 100 "Validation complete"
            ]
        ]


taskItem : Int -> Int -> String -> Html Msg
taskItem currentProgress threshold label =
    li
        [ classList
            [ ( "task-item", True )
            , ( "completed", currentProgress >= threshold )
            , ( "in-progress", currentProgress >= threshold - 10 && currentProgress < threshold )
            ]
        ]
        [ if currentProgress >= threshold then
            text "✅ "
          else if currentProgress >= threshold - 10 then
            text "⏳ "
          else
            text "⚪ "
        , text label
        ]
```

---

### 5. Completion & Download

```elm
viewCompletion : Model -> Html Msg
viewCompletion model =
    div [ class "completion-screen" ]
        [ div [ class "success-icon" ] [ text "✨" ]
        , h2 [] [ text "Success!" ]
        , p [ class "subtitle" ]
            [ text "RSR-compliant repository created" ]

        -- Summary
        , div [ class "completion-summary" ]
            [ summaryRow "Files created" (String.fromInt (List.length model.generatedFiles))
            , summaryRow "RSR Compliance" "Gold (100%)"
            , summaryRow "License" "MIT + Palimpsest v0.8"
            ]

        -- Download options
        , div [ class "download-options" ]
            [ button
                [ class "btn-primary btn-large"
                , onClick DownloadZip
                ]
                [ text "⬇️ Download ZIP" ]
            , button
                [ class "btn-secondary"
                , onClick (CopyToClipboard (generateBashCommands model.config))
                ]
                [ text "📋 Copy Setup Commands" ]
            ]

        -- Next steps
        , section [ class "next-steps" ]
            [ h3 [] [ text "Next Steps" ]
            , ol []
                [ li [] [ text "Extract the ZIP file" ]
                , li [] [ text "Review generated files, especially README.adoc" ]
                , li [] [ text "Customize placeholders marked with *REMINDER*" ]
                , li [] [ code [] [ text "cd ", text model.config.name, text " && just validate" ] ]
                , li [] [ text "Commit and push to GitLab" ]
                ]
            ]

        -- Resources
        , section [ class "resources" ]
            [ h3 [] [ text "Resources" ]
            , ul []
                [ li [] [ a [ href "/claude-md" ] [ text "📖 Full RSR Specification (CLAUDE.md)" ] ]
                , li [] [ a [ href "/compliance-checklist" ] [ text "📋 Compliance Checklist" ] ]
                , li [] [ a [ href "/cccp-manifesto" ] [ text "📜 CCCP Manifesto" ] ]
                ]
            ]

        -- Start over
        , button
            [ class "btn-text"
            , onClick (GoToStep Welcome)
            ]
            [ text "← Create Another Repository" ]
        ]
```

---

## Offline-First Implementation

### Service Worker

```javascript
// service-worker.js
const CACHE_NAME = 'rhodium-web-ui-v1.0.0';
const urlsToCache = [
  '/',
  '/index.html',
  '/elm.js',  // Compiled Elm app
  '/styles.css',
  '/templates/README.adoc.txt',  // All templates
  '/templates/SECURITY.md.txt',
  // ... (all embedded templates)
];

self.addEventListener('install', event => {
  event.waitUntil(
    caches.open(CACHE_NAME)
      .then(cache => cache.addAll(urlsToCache))
  );
});

self.addEventListener('fetch', event => {
  event.respondWith(
    caches.match(event.request)
      .then(response => response || fetch(event.request))
  );
});
```

### LocalStorage Persistence

```elm
-- Save config to LocalStorage
port saveConfig : ProjectConfig -> Cmd msg

-- Load config from LocalStorage
port loadConfig : (Maybe ProjectConfig -> msg) -> Sub msg


-- JavaScript side (ports.js)
app.ports.saveConfig.subscribe(config => {
  localStorage.setItem('rhodium-config', JSON.stringify(config));
});

app.ports.loadConfig.send(
  JSON.parse(localStorage.getItem('rhodium-config'))
);
```

---

## Accessibility (WCAG 2.1 AA)

### Keyboard Navigation

```elm
-- All interactive elements keyboard-accessible
viewWizardNav : Html Msg
viewWizardNav =
    div
        [ class "wizard-nav"
        , attribute "role" "navigation"
        , attribute "aria-label" "Wizard navigation"
        ]
        [ button
            [ type_ "button"
            , class "btn-secondary"
            , onClick PreviousStep
            , attribute "accesskey" "p"  -- Alt+P for "Previous"
            ]
            [ text "← Back" ]
        , button
            [ type_ "button"
            , class "btn-primary"
            , onClick NextStep
            , attribute "accesskey" "n"  -- Alt+N for "Next"
            ]
            [ text "Next →" ]
        ]
```

### Screen Reader Support

```elm
-- Live regions for dynamic content
viewValidationResults : ValidationResult -> Html Msg
viewValidationResults result =
    div
        [ attribute "role" "alert"
        , attribute "aria-live" "polite"
        , attribute "aria-atomic" "true"
        ]
        [ if result.valid then
            text "Validation successful. All checks passed."
          else
            text ("Validation failed. " ++ String.fromInt (List.length result.errors) ++ " errors found.")
        ]
```

### Focus Management

```elm
-- Focus first error on validation failure
viewError : String -> Html Msg
viewError error =
    li
        [ attribute "role" "alert"
        , tabindex -1  -- Programmatically focusable
        , id ("error-" ++ slugify error)
        ]
        [ text error ]
```

---

## Testing Strategy

### Elm Test

```elm
module Tests.Validation exposing (..)

import Expect
import Test exposing (..)
import Rhodium.Validation exposing (validateProjectName)


suite : Test
suite =
    describe "Project name validation"
        [ test "accepts lowercase with hyphens" <|
            \_ ->
                validateProjectName "my-project"
                    |> .valid
                    |> Expect.equal True

        , test "rejects uppercase" <|
            \_ ->
                validateProjectName "My-Project"
                    |> .valid
                    |> Expect.equal False

        , test "rejects special characters" <|
            \_ ->
                validateProjectName "my_project!"
                    |> .valid
                    |> Expect.equal False

        , test "rejects spaces" <|
            \_ ->
                validateProjectName "my project"
                    |> .valid
                    |> Expect.equal False
        ]
```

### Accessibility Testing

```bash
# pa11y for automated accessibility audits
pa11y http://localhost:8000

# axe-core integration
npm run test:a11y
```

---

## Build & Deploy

### Build Process

```bash
# Development build
elm make src/Main.elm --output=public/elm.js

# Production build (optimized)
elm make src/Main.elm --output=public/elm.js --optimize
uglifyjs public/elm.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output public/elm.min.js
```

### Deployment

**GitLab Pages**:
```yaml
# .gitlab-ci.yml
pages:
  stage: deploy
  image: node:lts-alpine
  script:
    - npm install -g elm uglify-js
    - elm make src/Main.elm --output=public/elm.js --optimize
    - uglifyjs public/elm.js --compress --mangle --output public/elm.min.js
  artifacts:
    paths:
      - public
  only:
    - main
```

**Static hosting**: Netlify, Vercel, Cloudflare Pages (all support Elm)

---

## Future Enhancements

### Phase 2
- [ ] **Dark mode**: Toggle between light/dark themes
- [ ] **Template editor**: Custom template creation in-browser
- [ ] **Drag-and-drop**: File upload for existing repos (migration)
- [ ] **Real-time collaboration**: Multiple users editing same config (CRDTs!)

### Phase 3
- [ ] **AI assistant**: GPT-4 integration for custom prompts
- [ ] **Version control**: Git operations in-browser (via wasm-git)
- [ ] **Deployment**: One-click deploy to GitLab/Cloudflare Pages

---

## Success Metrics

- **Adoption**: 500+ repositories created via web UI in first year
- **Accessibility**: 100% WCAG 2.1 AA compliance (automated + manual audit)
- **Performance**: < 2s load time, < 30KB gzipped bundle
- **User Satisfaction**: > 4.7/5.0 rating

---

## Contact

- **Repository**: https://gitlab.com/hyperpolymath/rhodium-web-ui
- **Issues**: https://gitlab.com/hyperpolymath/rhodium-web-ui/-/issues
- **Live Demo**: https://rhodium.hyperpolymath.org

---

*"Beautiful, accessible, offline-first RSR creation in your browser."*

— Rhodium Web UI (Elm)
