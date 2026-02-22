# Palimpsest License Styling Guide

## Introduction

This guide provides comprehensive instructions for using and customising the Palimpsest License styling system. It covers design principles, best practices, and practical examples.

---

## Design Principles

### 1. Accessibility First

Every style decision prioritises accessibility:

- **WCAG 2.3 Level AA compliance** minimum
- **Keyboard navigation** for all interactive elements
- **Screen reader compatibility** with semantic HTML and ARIA
- **Colour contrast** minimum 4.5:1 for normal text, 3:1 for large text
- **Touch target size** minimum 44×44 pixels

### 2. Progressive Enhancement

Start with a functional baseline and enhance:

- Core functionality works without JavaScript
- Styles degrade gracefully in older browsers
- Enhanced features for modern browsers
- Respects user preferences (motion, contrast, colour scheme)

### 3. Modularity

Components are self-contained and reusable:

- Each component lives in its own file
- Components can be used independently
- Minimal dependencies between components
- Clear naming conventions

### 4. Maintainability

Code is written to be easily maintained:

- Consistent formatting and structure
- Clear, descriptive variable names
- Comprehensive comments
- British English throughout

---

## Colour System

### Colour Palette

#### Light Mode
```scss
$colour-text-light: #000000;
$colour-background-light: #ffffff;
$colour-accent-light: #0366d6;
$colour-surface-light: #f6f8fa;
$colour-border-light: #e1e4e8;
```

#### Dark Mode
```scss
$colour-text-dark: #c9d1d9;
$colour-background-dark: #0d1117;
$colour-accent-dark: #58a6ff;
$colour-surface-dark: #161b22;
$colour-border-dark: #30363d;
```

#### Semantic Colours
```scss
$colour-success: #28a745;  // Green
$colour-warning: #ffc107;  // Amber
$colour-error: #dc3545;    // Red
$colour-info: #17a2b8;     // Teal
```

#### Brand Colours
```scss
$colour-palimpsest-primary: #4a5568;   // Slate grey
$colour-palimpsest-secondary: #718096;  // Steel grey
$colour-palimpsest-accent: #805ad5;     // Purple
```

### Using Colours

```scss
// In SCSS
.my-element {
  color: var(--colour-text);
  background-color: var(--colour-surface);
  border-color: var(--colour-border);
}

// With semantic colours
.success-message {
  color: $colour-success;
}
```

### Colour Contrast

Always verify colour contrast meets WCAG requirements:

```scss
// Use the contrast-ratio function
$contrast: contrast-ratio($foreground, $background);

// Check if it meets requirements
@if meets-contrast-requirement($foreground, $background, 'AA', 'normal') {
  // Use these colours
}
```

---

## Typography

### Type Scale

```scss
$font-size-xs: 0.75rem;    // 12px
$font-size-sm: 0.875rem;   // 14px
$font-size-md: 1rem;       // 16px (base)
$font-size-lg: 1.125rem;   // 18px
$font-size-xl: 1.25rem;    // 20px
$font-size-2xl: 1.5rem;    // 24px
$font-size-3xl: 1.875rem;  // 30px
$font-size-4xl: 2.25rem;   // 36px
```

### Font Families

```scss
// System font stack (default)
$font-family-base: -apple-system, BlinkMacSystemFont, "Segoe UI",
                   Helvetica, Arial, sans-serif;

// Monospace for code
$font-family-monospace: SFMono-Regular, Consolas, "Liberation Mono",
                        Menlo, monospace;
```

### Font Weights

```scss
$font-weight-light: 300;
$font-weight-normal: 400;
$font-weight-medium: 500;
$font-weight-semibold: 600;
$font-weight-bold: 700;
```

### Line Heights

```scss
$line-height-tight: 1.25;    // Headings
$line-height-normal: 1.5;    // UI text
$line-height-relaxed: 1.6;   // Body text
$line-height-loose: 2;       // Spacious text
```

### Typography Examples

```html
<!-- Headings -->
<h1>Main Heading (font-size-4xl)</h1>
<h2>Sub Heading (font-size-3xl)</h2>
<h3>Section Heading (font-size-2xl)</h3>

<!-- Body text -->
<p>Regular paragraph text uses the base font size.</p>
<p class="lead">Lead paragraph uses font-size-lg.</p>
<p class="small">Small text uses font-size-sm.</p>

<!-- Text utilities -->
<p class="text-muted">Muted text (60% opacity)</p>
<p class="font-semibold">Semibold text</p>
<p class="text-uppercase">Uppercase text</p>
```

---

## Spacing System

### Spacing Scale

```scss
$spacing-xs: 0.25rem;   // 4px
$spacing-sm: 0.5rem;    // 8px
$spacing-md: 1rem;      // 16px
$spacing-lg: 1.5rem;    // 24px
$spacing-xl: 2rem;      // 32px
$spacing-2xl: 3rem;     // 48px
$spacing-3xl: 4rem;     // 64px
$spacing-4xl: 6rem;     // 96px
```

### Using Spacing

```html
<!-- Margin utilities -->
<div class="mt-lg">Margin top large</div>
<div class="mb-md">Margin bottom medium</div>
<div class="mx-auto">Margin horizontal auto (centred)</div>
<div class="my-xl">Margin vertical extra large</div>

<!-- Padding utilities -->
<div class="p-lg">Padding large on all sides</div>
<div class="px-md py-sm">Padding medium horizontal, small vertical</div>

<!-- Gap (for flex/grid) -->
<div class="d-flex gap-md">
  <div>Item 1</div>
  <div>Item 2</div>
</div>
```

### Spacing in SCSS

```scss
.my-component {
  margin: $spacing-lg 0;
  padding: $spacing-md;
  gap: $spacing-sm;
}
```

---

## Layout

### Containers

```html
<!-- Standard container (max-width: 980px) -->
<div class="container">
  Content
</div>

<!-- Size variants -->
<div class="container-sm">Small container (640px)</div>
<div class="container-lg">Large container (980px)</div>
<div class="container-xl">Extra large (1200px)</div>
<div class="container-fluid">Full width with padding</div>
```

### Grid System

```html
<!-- CSS Grid -->
<div class="grid grid-2">
  <div>Column 1</div>
  <div>Column 2</div>
</div>

<div class="grid grid-3">
  <div>Column 1</div>
  <div>Column 2</div>
  <div>Column 3</div>
</div>

<!-- Auto-fit grid -->
<div class="grid grid-auto">
  <div>Auto-sized column</div>
  <div>Auto-sized column</div>
  <div>Auto-sized column</div>
</div>
```

### Flexbox

```html
<!-- Basic flex -->
<div class="d-flex gap-md">
  <div>Item 1</div>
  <div>Item 2</div>
</div>

<!-- Flex with alignment -->
<div class="d-flex justify-between items-center">
  <div>Left</div>
  <div>Right</div>
</div>

<!-- Flex column -->
<div class="d-flex flex-column gap-sm">
  <div>Row 1</div>
  <div>Row 2</div>
</div>
```

---

## Responsive Design

### Breakpoints

```scss
$breakpoint-xs: 320px;   // Mobile (small)
$breakpoint-sm: 640px;   // Mobile (large)
$breakpoint-md: 768px;   // Tablet
$breakpoint-lg: 1024px;  // Desktop (small)
$breakpoint-xl: 1280px;  // Desktop (large)
$breakpoint-2xl: 1536px; // Desktop (extra large)
```

### Using Breakpoints

```scss
// In SCSS
.my-component {
  padding: $spacing-lg;

  @include breakpoint-down(md) {
    padding: $spacing-md;
  }

  @include breakpoint-up(lg) {
    padding: $spacing-xl;
  }
}
```

### Responsive Utilities

```html
<!-- Hide on mobile -->
<div class="d-none d-md-block">
  Visible on tablet and up
</div>

<!-- Mobile-only -->
<div class="d-block d-md-none">
  Visible on mobile only
</div>
```

---

## Mixins

### Common Mixins

#### Card

```scss
.my-card {
  @include card($spacing-lg);
}
```

#### Focus Visible

```scss
.my-button {
  @include focus-visible;
}
```

#### Hover Lift

```scss
.my-card {
  @include hover-lift;
}
```

#### Screen Reader Only

```scss
.hidden-label {
  @include sr-only;
}
```

#### Container

```scss
.my-container {
  @include container($max-width-lg);
}
```

#### Flex Centre

```scss
.centred-content {
  @include flex-center;
}
```

---

## Utility Classes

### Display

```html
<div class="d-none">Hidden</div>
<div class="d-block">Block</div>
<div class="d-flex">Flex</div>
<div class="d-grid">Grid</div>
<div class="d-inline-block">Inline block</div>
```

### Width and Height

```html
<div class="w-100">Full width</div>
<div class="w-50">Half width</div>
<div class="h-100">Full height</div>
```

### Borders

```html
<div class="border">All borders</div>
<div class="border-top">Top border only</div>
<div class="rounded">Rounded corners</div>
<div class="rounded-full">Fully rounded (pill)</div>
```

### Shadows

```html
<div class="shadow-sm">Small shadow</div>
<div class="shadow">Medium shadow</div>
<div class="shadow-lg">Large shadow</div>
```

### Text Alignment

```html
<p class="text-left">Left aligned</p>
<p class="text-center">Centre aligned</p>
<p class="text-right">Right aligned</p>
```

---

## Best Practices

### 1. Use Semantic HTML

```html
<!-- Good -->
<article>
  <header>
    <h1>Title</h1>
  </header>
  <p>Content</p>
</article>

<!-- Avoid -->
<div>
  <div class="title">Title</div>
  <div>Content</div>
</div>
```

### 2. Leverage Utility Classes

```html
<!-- Good -->
<div class="d-flex gap-md items-center">
  <img src="icon.svg" alt="Icon">
  <span>Text</span>
</div>

<!-- Avoid creating custom classes for simple layouts -->
```

### 3. Component Composition

```html
<!-- Combine components and utilities -->
<div class="license-display mt-xl mb-2xl">
  <div class="badge badge--compliant mb-md">Compliant</div>
  <!-- Content -->
</div>
```

### 4. Accessibility Labels

```html
<!-- Always provide labels -->
<nav aria-label="Language selection">
  <a href="?lang=en" aria-current="page">English</a>
  <a href="?lang=nl">Nederlands</a>
</nav>

<!-- Use sr-only for visual-only designs -->
<button>
  <span class="sr-only">Close menu</span>
  <svg aria-hidden="true">×</svg>
</button>
```

### 5. Maintain Consistency

- Use design tokens (variables) instead of hard-coded values
- Follow the established naming conventions
- Reuse existing components before creating new ones
- Document custom components

---

## Troubleshooting

### Common Issues

#### Issue: Styles not applying

**Solution:**
1. Ensure CSS is compiled: `npm run build`
2. Check file path in HTML: `<link href="styles/css/main.css">`
3. Clear browser cache
4. Check browser console for errors

#### Issue: Dark mode not working

**Solution:**
1. Verify browser supports `prefers-color-scheme`
2. Check OS/browser theme settings
3. Use `data-theme="dark"` for manual control

#### Issue: Focus styles not visible

**Solution:**
1. Ensure `:focus-visible` is supported
2. Check if outline is overridden elsewhere
3. Test with keyboard navigation (Tab key)

#### Issue: Responsive breakpoints not working

**Solution:**
1. Check viewport meta tag: `<meta name="viewport" content="width=device-width, initial-scale=1.0">`
2. Test at specific breakpoint widths
3. Use browser dev tools device emulation

---

## Additional Resources

- [SCSS Documentation](https://sass-lang.com/documentation)
- [CSS Tricks - Flexbox Guide](https://css-tricks.com/snippets/css/a-guide-to-flexbox/)
- [CSS Tricks - Grid Guide](https://css-tricks.com/snippets/css/complete-guide-grid/)
- [MDN - CSS Reference](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference)
- [WCAG 2.3 Guidelines](https://www.w3.org/WAI/WCAG23/quickref/)

---

**Maintained by**: Palimpsest Stewardship Council
**Version**: 0.4.0
**Last Updated**: 2025-11-22
