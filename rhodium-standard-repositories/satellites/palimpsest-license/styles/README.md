# Palimpsest License Styling System

A comprehensive, accessible, and modular SCSS framework for the Palimpsest License project.

## Quick Start

```bash
# Install dependencies
npm install

# Compile SCSS (development with watch)
npm run watch

# Build for production
npm run build
```

## Structure

```
styles/
├── scss/              # SCSS source files (edit these)
├── css/               # Compiled CSS (auto-generated, gitignored)
└── docs/              # Comprehensive documentation
```

## Key Features

- ✅ **WCAG 2.3 Compliant** - Accessibility first
- 🌓 **Multiple Themes** - Light, dark, high contrast, and custom
- 🌍 **Bilingual Support** - Dutch and English
- 📱 **Responsive Design** - Mobile-first approach
- 🎨 **Modular Components** - Reusable, self-contained components
- 🔧 **Customisable** - Comprehensive design tokens and variables

## Components

- **Licence Display** - Structured licence presentation
- **Badges** - Status indicators and labels
- **Metadata Display** - Various metadata presentation formats
- **Compliance Indicators** - Visual compliance status

## Documentation

See the [docs/](./docs/) directory for comprehensive documentation:

- [README.md](./docs/README.md) - Complete overview and getting started
- [STYLING_GUIDE.md](./docs/STYLING_GUIDE.md) - Design principles and best practices
- [COMPONENTS.md](./docs/COMPONENTS.md) - Detailed component documentation

## Scripts

| Command | Description |
|---------|-------------|
| `npm run scss` | Compile SCSS once (development) |
| `npm run scss:watch` | Watch and recompile on changes |
| `npm run scss:build` | Production build (minified) |
| `npm run build` | Alias for production build |
| `npm run watch` | Alias for watch mode |
| `npm run clean` | Remove compiled CSS files |

## Browser Support

- Chrome/Edge (last 2 versions)
- Firefox (last 2 versions)
- Safari (last 2 versions)
- iOS Safari (last 2 versions)
- Android Chrome (last 2 versions)

## Contributing

When contributing to the styling system:

1. Edit SCSS files in `styles/scss/`, never edit compiled CSS directly
2. Follow the existing code structure and naming conventions
3. Use British English for comments and documentation
4. Test accessibility with keyboard navigation and screen readers
5. Verify colour contrast meets WCAG 2.3 standards
6. Run `npm run format` before committing
7. Compile and test before committing: `npm run build`

## License

Part of the Palimpsest License project. Available under the MIT License.

---

**Version**: 0.4.0
**Maintained by**: Palimpsest Stewardship Council
