# Palimpsest License Styling System Documentation

## Overview

The Palimpsest License Styling System is a comprehensive, modular SCSS framework designed to provide consistent, accessible, and themeable styles for the Palimpsest License project. Built with WCAG 2.3 compliance at its core, this system supports bilingual content (Dutch/English), multiple themes, and specialised components for licence display, metadata presentation, and compliance indicators.

## Table of Contents

1. [Getting Started](#getting-started)
2. [Project Structure](#project-structure)
3. [Build System](#build-system)
4. [Components](#components)
5. [Theming](#theming)
6. [Customisation](#customisation)
7. [Accessibility](#accessibility)
8. [Integration](#integration)

---

## Getting Started

### Installation

```bash
# Install dependencies
npm install
```

### Development

```bash
# Compile SCSS with source maps and watch for changes
npm run watch

# Or manually compile once
npm run scss
```

### Production Build

```bash
# Compile SCSS with minification (no source maps)
npm run build
```

### Cleaning

```bash
# Remove all compiled CSS files
npm run clean
```

---

## Project Structure

```
styles/
├── scss/                          # SCSS source files
│   ├── _variables.scss           # Design tokens and configuration
│   ├── _mixins.scss              # Reusable mixins
│   ├── _functions.scss           # Helper functions
│   ├── main.scss                 # Main entry point
│   ├── base/                     # Base styles
│   │   ├── _reset.scss          # CSS reset and normalisation
│   │   ├── _accessibility.scss  # Accessibility utilities
│   │   └── _typography.scss     # Typography styles
│   ├── layout/                   # Layout components
│   │   └── _containers.scss     # Containers and grid systems
│   ├── components/               # UI components
│   │   ├── _license-display.scss # Licence display widgets
│   │   ├── _badge.scss          # Badges and labels
│   │   ├── _metadata.scss       # Metadata presentation
│   │   └── _compliance.scss     # Compliance indicators
│   ├── utilities/                # Utility classes
│   │   └── _utilities.scss      # Helper classes
│   └── themes/                   # Theme definitions
│       └── _themes.scss         # Light, dark, and custom themes
├── css/                          # Compiled CSS (gitignored)
│   ├── main.css                 # Expanded CSS
│   ├── main.css.map             # Source map
│   └── main.min.css             # Minified CSS
└── docs/                         # Documentation
    ├── README.md                # This file
    ├── STYLING_GUIDE.md         # Comprehensive styling guide
    ├── COMPONENTS.md            # Component documentation
    └── CUSTOMISATION.md         # Customisation instructions
```

---

## Build System

### Available Scripts

| Script | Description |
|--------|-------------|
| `npm run scss` | Compile SCSS once with source maps (expanded) |
| `npm run scss:watch` | Watch SCSS files and recompile on change |
| `npm run scss:build` | Production build (minified, no source maps) |
| `npm run scss:build:dev` | Development build (expanded, with source maps) |
| `npm run build` | Alias for `scss:build` |
| `npm run watch` | Alias for `scss:watch` |
| `npm run clean` | Remove all compiled CSS files |

### Build Configuration

The build system uses Dart Sass with the following configurations:

- **Development**: Expanded output style with source maps
- **Production**: Compressed output style without source maps
- **Watch mode**: Automatically recompiles on file changes

### Source Maps

Source maps are generated in development mode to facilitate debugging. They map the compiled CSS back to the original SCSS files, making it easier to identify where styles are defined.

---

## Components

The styling system includes several pre-built components specifically designed for the Palimpsest License:

### Licence Display

Display licence information with various styles and layouts.

```html
<div class="license-display">
  <header class="license-display__header">
    <h2 class="license-display__title">
      Palimpsest License
      <span class="license-display__version">v0.4</span>
    </h2>
  </header>
  <div class="license-display__body">
    <!-- Licence content -->
  </div>
  <footer class="license-display__footer">
    <!-- Actions and metadata -->
  </footer>
</div>
```

**Variants:**
- `.license-display--compact` - Reduced padding
- `.license-display--minimal` - For embedding

### Badges

Status indicators and labels for various contexts.

```html
<!-- Status badges -->
<span class="badge badge--compliant">Compliant</span>
<span class="badge badge--warning">Partial</span>
<span class="badge badge--violation">Violation</span>

<!-- Version badge -->
<span class="badge-version">0.4</span>

<!-- AI training status -->
<span class="badge-ai-training badge-ai-training--restricted">
  AI Training Restricted
</span>
```

### Metadata Display

Present licence metadata in various formats.

```html
<div class="metadata">
  <header class="metadata__header">
    <h3>Licence Metadata</h3>
  </header>
  <div class="metadata__body">
    <dl class="metadata-list">
      <div class="metadata-list__item">
        <dt class="metadata-list__key">Creator</dt>
        <dd class="metadata-list__value">Author Name</dd>
      </div>
    </dl>
  </div>
</div>
```

### Compliance Indicators

Visual indicators for compliance status.

```html
<div class="compliance-status compliance-status--compliant">
  <span class="compliance-status__icon"></span>
  <span class="compliance-status__text">Compliant</span>
</div>
```

See [COMPONENTS.md](./COMPONENTS.md) for detailed component documentation.

---

## Theming

The styling system supports multiple themes:

### Built-in Themes

1. **Light Mode** (default)
2. **Dark Mode** (auto-detected via `prefers-color-scheme`)
3. **High Contrast Mode** (auto-detected via `forced-colors`)
4. **Palimpsest Brand Theme**
5. **Minimal Theme** (for embeds)

### Applying Themes

Themes are automatically applied based on user preferences:

```html
<!-- Manual theme application -->
<body data-theme="dark">
  <!-- Content -->
</body>

<body data-theme="palimpsest">
  <!-- Content with Palimpsest branding -->
</body>
```

### Theme Variables

Themes use CSS custom properties for runtime theming:

```css
:root {
  --colour-text: #000000;
  --colour-background: #ffffff;
  --colour-accent: #0366d6;
  --colour-surface: #f6f8fa;
  --colour-border: #e1e4e8;
}
```

---

## Customisation

### Variables

All design tokens are defined in `_variables.scss`:

```scss
// Colours
$colour-text-light: #000000;
$colour-background-light: #ffffff;
$colour-accent-light: #0366d6;

// Typography
$font-size-base: 1rem;
$line-height-normal: 1.5;

// Spacing
$spacing-md: 1rem;
$spacing-lg: 1.5rem;
```

### Creating Custom Components

1. Create a new file in `styles/scss/components/`
2. Import variables and mixins:

```scss
@use '../variables' as *;
@use '../mixins' as *;

.my-component {
  @include card;
  padding: $spacing-lg;
}
```

3. Import your component in `main.scss`:

```scss
@use 'components/my-component';
```

### Overriding Styles

Create a custom SCSS file and import after the main stylesheet:

```scss
@import 'styles/css/main.css';

// Your overrides
.license-display {
  border-color: #custom-colour;
}
```

See [CUSTOMISATION.md](./CUSTOMISATION.md) for more details.

---

## Accessibility

The styling system is built with WCAG 2.3 compliance in mind:

### Features

- **Keyboard Navigation**: Proper focus styles for all interactive elements
- **Screen Reader Support**: Semantic HTML and ARIA attributes
- **Colour Contrast**: Minimum 4.5:1 contrast ratio for text
- **Touch Targets**: Minimum 44×44px for interactive elements
- **Reduced Motion**: Respects `prefers-reduced-motion` preference
- **High Contrast Mode**: Supports `forced-colors` media query
- **Print Styles**: Optimised for printing

### Accessibility Classes

```html
<!-- Screen reader only -->
<span class="sr-only">Hidden from visual users</span>

<!-- Skip link -->
<a href="#main-content" class="skip-link">Skip to main content</a>

<!-- Focus enhancement -->
<button class="focus-enhanced">Button</button>
```

### Testing

- Use keyboard-only navigation to test focus states
- Test with screen readers (NVDA, JAWS, VoiceOver)
- Verify colour contrast with tools like Axe or WAVE
- Test with browser zoom at 200%

---

## Integration

### HTML Integration

Include the compiled CSS in your HTML:

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <link rel="stylesheet" href="styles/css/main.css">
  <title>Palimpsest License</title>
</head>
<body>
  <main class="markdown-body">
    <!-- Your content -->
  </main>
</body>
</html>
```

### Bilingual Support

The system supports Dutch and English:

```html
<!-- Language switcher -->
<nav class="language-switcher" aria-label="Language selection">
  <a href="?lang=en" lang="en" aria-current="true">English</a>
  <a href="?lang=nl" lang="nl">Nederlands</a>
</nav>

<!-- Content sections -->
<section lang="en">
  <h2>English Content</h2>
</section>

<section lang="nl">
  <h2>Nederlandse Inhoud</h2>
</section>
```

### JavaScript Integration

If you need theme switching:

```javascript
// Toggle theme
function setTheme(theme) {
  document.body.setAttribute('data-theme', theme);
  localStorage.setItem('theme', theme);
}

// Detect user preference
const prefersDark = window.matchMedia('(prefers-color-scheme: dark)');
if (prefersDark.matches) {
  setTheme('dark');
}
```

---

## Browser Support

The styling system supports:

- Chrome/Edge (last 2 versions)
- Firefox (last 2 versions)
- Safari (last 2 versions)
- iOS Safari (last 2 versions)
- Android Chrome (last 2 versions)

### Progressive Enhancement

The system uses progressive enhancement:

- Core functionality works in all browsers
- Enhanced features for modern browsers
- Graceful degradation for older browsers

---

## Contributing

When contributing to the styling system:

1. Follow the existing code structure
2. Use British English for comments and documentation
3. Test accessibility with keyboard and screen reader
4. Verify colour contrast meets WCAG 2.3 standards
5. Update documentation for new components
6. Run `npm run format` before committing

---

## Resources

- [SCSS Documentation](https://sass-lang.com/documentation)
- [WCAG 2.3 Guidelines](https://www.w3.org/WAI/WCAG23/quickref/)
- [MDN CSS Reference](https://developer.mozilla.org/en-US/docs/Web/CSS)
- [CSS Tricks](https://css-tricks.com/)

---

## License

This styling system is part of the Palimpsest License project and is available under the MIT License.

---

**Version**: 0.4.0
**Last Updated**: 2025-11-22
**Maintained by**: Palimpsest Stewardship Council
