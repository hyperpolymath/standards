# Palimpsest Licence Blocks

This directory contains ready-to-use HTML and Markdown snippets for displaying Palimpsest License information on your website, documentation, or project.

## Directory Contents

### HTML Snippets

1. **`html-basic.html`** — Simple licence block with badge and text
2. **`html-detailed.html`** — Comprehensive block with summary and metadata
3. **`html-footer.html`** — Footer-optimised licence block
4. **`html-sidebar.html`** — Sidebar widget with collapsible details
5. **`html-inline.html`** — Inline badge for use within content

### Markdown Snippets

1. **`markdown-basic.md`** — Simple badge and text for README files
2. **`markdown-detailed.md`** — Comprehensive licence sections for documentation

## Quick Start

### For Markdown (GitHub, GitLab, etc.)

Copy this to your README.md:

```markdown
[![Palimpsest License v0.4](https://example.com/path/to/badge-standard.svg)](https://palimpsestlicense.org/)

This work is licensed under the [Palimpsest License v0.4](https://palimpsestlicense.org/).
```

**Don't forget to:**
1. Replace `https://example.com/path/to/` with your actual badge URL
2. Host the badge SVG somewhere accessible (GitHub repo, CDN, etc.)

### For HTML (Websites, Blogs)

Copy from `html-basic.html` or `html-detailed.html`:

```html
<div class="palimpsest-license">
  <a href="https://palimpsestlicense.org/" rel="license">
    <img src="https://example.com/path/to/badge-standard.svg"
         alt="Palimpsest License v0.4"
         width="128" height="32">
  </a>
  <p>
    This work is licensed under the
    <a href="https://palimpsestlicense.org/" rel="license">
      Palimpsest License v0.4
    </a>.
  </p>
</div>
```

**Don't forget to:**
1. Replace `https://example.com/path/to/` with your actual badge URL
2. Include the CSS from the snippet or adapt to your site's styles

## Hosting Badges

You have several options for hosting badge images:

### Option 1: GitHub Raw URL

Upload badges to your GitHub repository:

```
https://raw.githubusercontent.com/username/repo/main/assets/badges/svg/badge-standard.svg
```

**Pros:** Free, version controlled
**Cons:** Not a CDN, may be slow for high traffic

### Option 2: jsDelivr CDN

Use jsDelivr to serve from GitHub:

```
https://cdn.jsdelivr.net/gh/username/repo@main/assets/badges/svg/badge-standard.svg
```

**Pros:** Free CDN, fast globally
**Cons:** Depends on third-party service

### Option 3: Self-Hosted

Upload to your own web server:

```
https://yourdomain.com/assets/palimpsest-badge.svg
```

**Pros:** Full control
**Cons:** You manage hosting

### Option 4: Palimpsest Official CDN (Future)

In the future, the Palimpsest project may provide official badge hosting:

```
https://badges.palimpsestlicense.org/v0.4/standard.svg
```

Check [palimpsestlicense.org](https://palimpsestlicense.org/) for updates.

## Customisation

### Changing Badge Style

Choose from available badge variants:

- `badge-compact.svg` — 90×20px, minimal
- `badge-standard.svg` — 128×32px, default
- `badge-large.svg` — 180×40px, detailed
- `badge-blue.svg` — Blue gradient
- `badge-green.svg` — Green gradient
- `badge-dark.svg` — Dark theme
- `badge-light.svg` — Light theme
- `badge-flat.svg` — Flat design
- `badge-text-only.svg` — No icon

See `assets/badges/README.md` for details.

### Adapting HTML Styles

All HTML snippets include embedded CSS. You can:

1. **Use as-is:** Copy the entire snippet including `<style>` block
2. **Extract CSS:** Move styles to your main stylesheet
3. **Customise:** Modify colours, fonts, spacing to match your site
4. **Framework integration:** Adapt class names for Bootstrap, Tailwind, etc.

### Adding Metadata

For better SEO and semantic web support, use Schema.org markup:

```html
<div itemscope itemtype="https://schema.org/CreativeWork">
  <span itemprop="name">Your Work Title</span>
  by <span itemprop="author">Your Name</span>
  is licensed under
  <a href="https://palimpsestlicense.org/"
     rel="license"
     itemprop="license">
    Palimpsest License v0.4
  </a>
</div>
```

## Accessibility

All snippets are designed with accessibility in mind:

- ✅ Proper ARIA labels
- ✅ Semantic HTML elements
- ✅ Keyboard navigation support
- ✅ Screen reader compatible
- ✅ High contrast colours (WCAG AA)
- ✅ Descriptive alt text

### Accessibility Checklist

When using these snippets:

- [ ] Include descriptive alt text for images
- [ ] Use `rel="license"` on licence links
- [ ] Add `role="contentinfo"` to licence blocks
- [ ] Ensure sufficient colour contrast
- [ ] Test with screen reader
- [ ] Verify keyboard navigation

## Examples in the Wild

Want to see how others use Palimpsest licence blocks? Check out:

- [Example Repository 1](#) — GitHub README implementation
- [Example Website 2](#) — Blog footer usage
- [Example Project 3](#) — Documentation sidebar

*(This section will be updated as projects adopt the licence)*

## Multi-Language Support

For bilingual or multilingual sites, you can provide licence information in multiple languages:

```html
<div class="palimpsest-license" lang="en">
  <p>
    Licensed under the
    <a href="https://palimpsestlicense.org/licence-text/">
      Palimpsest License v0.4
    </a>
  </p>
</div>

<div class="palimpsest-license" lang="nl">
  <p>
    Gelicentieerd onder de
    <a href="https://palimpsestlicense.org/licence-text/nl/">
      Palimpsest Licentie v0.4
    </a>
  </p>
</div>
```

Available translations:
- **English:** `https://palimpsestlicense.org/licence-text/`
- **Dutch (Nederlands):** `https://palimpsestlicense.org/licence-text/nl/`

## Integration with Other Platforms

### WordPress

Add to your theme's `footer.php`:

```php
<div class="palimpsest-license">
  <a href="https://palimpsestlicense.org/" rel="license">
    <img src="<?php echo get_template_directory_uri(); ?>/assets/palimpsest-badge.svg"
         alt="Palimpsest License" />
  </a>
  <p>&copy; <?php echo date('Y'); ?> <?php bloginfo('name'); ?>.
     Licensed under the Palimpsest License v0.4.</p>
</div>
```

### Hugo (Static Site Generator)

Create `layouts/partials/license.html`:

```html
<div class="palimpsest-license">
  <a href="https://palimpsestlicense.org/" rel="license">
    <img src="{{ "images/palimpsest-badge.svg" | relURL }}"
         alt="Palimpsest License" />
  </a>
  <p>Licensed under the Palimpsest License v{{ .Site.Params.licenseVersion }}.</p>
</div>
```

Then include in your footer: `{{ partial "license.html" . }}`

### Jekyll (GitHub Pages)

Create `_includes/license.html`:

```html
<div class="palimpsest-license">
  <a href="https://palimpsestlicense.org/" rel="license">
    <img src="{{ '/assets/palimpsest-badge.svg' | relative_url }}"
         alt="Palimpsest License" />
  </a>
  <p>Licensed under the Palimpsest License v{{ site.license_version }}.</p>
</div>
```

Include in your layout: `{% include license.html %}`

## Troubleshooting

### Badge Not Displaying

**Problem:** Badge image doesn't show
**Solution:**
- Check the image URL is correct and accessible
- Verify CORS headers if hosted on different domain
- Ensure SVG is valid (validate at validator.w3.org)

### Badge Too Large/Small

**Problem:** Badge doesn't fit layout
**Solution:**
- Use appropriate size variant (compact/standard/large)
- Add CSS: `max-width: 100%; height: auto;`
- Consider responsive design with media queries

### Dark Mode Not Working

**Problem:** Badge doesn't adapt to dark mode
**Solution:**
- Use adaptive badges (standard, compact, large)
- Ensure browser supports `prefers-color-scheme`
- Fallback: provide separate light/dark variants

### Accessibility Issues

**Problem:** Screen reader not reading licence info
**Solution:**
- Add `aria-label` to containers
- Use `role="contentinfo"` for licence blocks
- Ensure alt text is descriptive
- Test with NVDA or JAWS screen readers

## Contributing

Found an issue or have a suggestion for licence block templates?

1. Open an issue at [GitHub Issues](https://github.com/palimpsest-license/issues)
2. Submit improvements via pull request
3. Share your implementation for the "Examples in the Wild" section

## Licence

These templates and snippets are provided under **CC0 (Public Domain)** to facilitate adoption of the Palimpsest License. You are free to use, modify, and distribute them without restriction.

However, use of these templates should accurately represent that your work is licensed under the Palimpsest License.

## See Also

- `assets/badges/` — Badge image files in multiple formats
- `assets/conversion-scripts/` — Tools for converting badges to different formats
- `GUIDES_v0.4/User_Guide.md` — How to apply the Palimpsest License
- [palimpsestlicense.org](https://palimpsestlicense.org/) — Official website

## Support

Need help implementing licence blocks?

- 📖 Read the [User Guide](https://palimpsestlicense.org/guides/user/)
- 💬 Ask in [Discussions](https://github.com/palimpsest-license/discussions)
- 🐛 Report bugs in [Issues](https://github.com/palimpsest-license/issues)
- 📧 Email: support@palimpsestlicense.org

---

**Last updated:** 2025-11-22
**Maintainer:** Palimpsest Stewardship Council
