# Palimpsest License Badges

This directory contains badge variants for the Palimpsest License in multiple formats and styles. These badges can be used on websites, documentation, GitHub repositories, and other platforms to indicate that a work is licensed under the Palimpsest License.

## Directory Structure

```
badges/
├── svg/       # SVG badge variants (source files)
├── png/       # PNG exports (512px, 1024px, 2048px)
├── jpg/       # JPG exports (high quality, web optimised)
├── tiff/      # TIFF exports (archival, high quality)
└── README.md  # This file
```

## Available Badge Variants

### By Size

1. **Compact** (`badge-compact.svg`) - 90×20px
   - Minimal design for tight spaces
   - Text: "Palimpsest"

2. **Standard** (`badge-standard.svg`) - 128×32px
   - Default badge for most uses
   - Text: "Palimpsest v0.4"

3. **Large** (`badge-large.svg`) - 180×40px
   - Detailed badge with subtitle
   - Text: "Palimpsest License" + "v0.4 - Emotional Lineage Protection"

### By Colour/Style

1. **Light** (`badge-light.svg`) - White background, optimised for light themes
2. **Dark** (`badge-dark.svg`) - Dark background, optimised for dark themes
3. **Blue** (`badge-blue.svg`) - Blue gradient background
4. **Green** (`badge-green.svg`) - Green gradient background
5. **Flat** (`badge-flat.svg`) - Solid colour, no border
6. **Text-Only** (`badge-text-only.svg`) - No icon, text centred

### Adaptive (Light/Dark)

The **Standard**, **Compact**, and **Large** badges include CSS media queries that automatically adapt to the user's colour scheme preference:

- Light mode: Light background, dark text
- Dark mode: Dark background, light text

## Usage

### Direct SVG (Recommended)

For websites and documentation, use SVG directly for best quality and adaptive dark mode support:

```html
<img src="https://example.com/path/to/badge-standard.svg" alt="Palimpsest License v0.4">
```

### Markdown

For GitHub README or Markdown documentation:

```markdown
![Palimpsest License v0.4](https://example.com/path/to/badge-standard.svg)
```

### PNG (Raster)

For platforms that don't support SVG, use PNG:

```html
<img src="https://example.com/path/to/badge-standard-512w.png" alt="Palimpsest License v0.4" width="128">
```

### JPG (Non-Transparent)

For email signatures or platforms requiring JPG:

```html
<img src="https://example.com/path/to/badge-standard-512w.jpg" alt="Palimpsest License v0.4" width="128">
```

## Generating Raster Formats

To convert SVG badges to PNG, JPG, and TIFF formats:

### Using the Shell Script

```bash
# Convert all badges
cd assets/conversion-scripts
./convert.sh -a ../badges/svg/

# Convert single badge
./convert.sh ../badges/svg/badge-standard.svg

# Convert to PNG only
./convert.sh -f png -a ../badges/svg/

# With optimisation
./convert.sh -O -a ../badges/svg/
```

### Using Node.js

```bash
# Convert all badges
cd assets/conversion-scripts
node convert.js --all

# Convert single badge
node convert.js ../badges/svg/badge-standard.svg

# With optimisation
node convert.js --optimise --all
```

### Using Makefile

```bash
# From project root
make convert-all      # All formats
make convert-png      # PNG only
make convert-jpg      # JPG only
make convert-tiff     # TIFF only
```

## Accessibility

All badges include:

- `aria-label` attributes for screen readers
- `<title>` elements for tooltips
- Descriptive alt text in usage examples
- High colour contrast (WCAG AA compliant)

When embedding badges, always include descriptive alt text:

```html
<!-- Good -->
<img src="badge.svg" alt="Licensed under Palimpsest License v0.4">

<!-- Bad -->
<img src="badge.svg" alt="badge">
```

## Customisation

### Changing Version Number

To update the version number in badges, edit the SVG files:

```xml
<!-- Change this: -->
<text x="30" y="20" class="text">Palimpsest v0.4</text>

<!-- To this: -->
<text x="30" y="20" class="text">Palimpsest v0.5</text>
```

### Changing Colours

Edit the `<style>` block in SVG files:

```xml
<style>
  .icon { fill: #0366d6; }  <!-- Change this colour -->
  .text { fill: #24292e; }  <!-- And this one -->
</style>
```

### Creating Custom Badges

Use an existing badge as a template:

1. Copy an existing SVG badge
2. Modify the colours, text, or layout
3. Test in light and dark modes
4. Validate with an SVG validator
5. Generate raster formats

## File Naming Convention

```
badge-{variant}-{size}w.{ext}

Examples:
- badge-standard.svg          (source)
- badge-standard-512w.png     (512px wide PNG)
- badge-standard-1024w.png    (1024px wide PNG)
- badge-compact-512w.jpg      (512px wide JPG)
- badge-large-archival.tiff   (archival TIFF)
```

## Hosting Recommendations

### GitHub

Host badges in your repository and reference via raw.githubusercontent.com:

```markdown
![Palimpsest License](https://raw.githubusercontent.com/username/repo/main/assets/badges/svg/badge-standard.svg)
```

### CDN

For high-traffic sites, use a CDN:

```html
<img src="https://cdn.jsdelivr.net/gh/username/repo@main/assets/badges/svg/badge-standard.svg" alt="Palimpsest License">
```

### Self-Hosted

Upload badges to your web server:

```html
<img src="/assets/palimpsest-badge.svg" alt="Palimpsest License">
```

## Licence

These badge designs are part of the Palimpsest License project. You are free to use, modify, and distribute these badges when indicating that your work is licensed under the Palimpsest License.

The badges themselves are provided under CC0 (public domain) to facilitate adoption, but their use should accurately represent that your work is licensed under the Palimpsest License.

## See Also

- `embed/license-blocks/` - Full licence blocks with HTML/Markdown snippets
- `assets/conversion-scripts/` - Conversion tools and documentation
- `GUIDES_v0.4/User_Guide.md` - How to apply the Palimpsest License

---

**Last updated:** 2025-11-22
**Maintainer:** Palimpsest Stewardship Council
