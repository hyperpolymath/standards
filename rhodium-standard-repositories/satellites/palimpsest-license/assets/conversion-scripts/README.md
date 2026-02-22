# Image Asset Conversion Guide

## Overview

This directory contains scripts and documentation for converting SVG assets to various formats (TIFF, PNG, JPG) for different use cases. The Palimpsest License project maintains multiple image formats to ensure maximum compatibility across platforms, print media, and archival needs.

## Directory Structure

```
assets/
├── badges/
│   ├── svg/       # SVG badges in various styles and sizes
│   ├── png/       # PNG exports (512px, 1024px, 2048px)
│   ├── jpg/       # JPG exports (high quality, web optimised)
│   └── tiff/      # TIFF exports (archival, high quality)
├── branding/      # Original SVG source files
└── conversion-scripts/
    ├── convert.sh        # Shell script using ImageMagick/Inkscape
    ├── convert.js        # Node.js script using sharp
    └── README.md         # This file
```

## Available Scripts

### 1. Shell Script (convert.sh)

Uses ImageMagick and/or Inkscape for conversion.

**Usage:**
```bash
# Convert single file
./convert.sh input.svg

# Convert all SVGs in a directory
./convert.sh -a /path/to/svgs/

# Convert to specific format only
./convert.sh -f png input.svg
./convert.sh -f jpg input.svg
./convert.sh -f tiff input.svg

# Specify custom sizes
./convert.sh -s 256,512,1024 input.svg
```

### 2. Node.js Script (convert.js)

Uses the sharp library for high-quality conversions.

**Usage:**
```bash
# Install dependencies first
npm install

# Convert single file
node convert.js input.svg

# Convert all SVGs
node convert.js --all

# Convert to specific format
node convert.js --format png input.svg

# Custom sizes
node convert.js --sizes 256,512,1024 input.svg
```

### 3. Makefile Targets

Batch conversion using make.

**Usage:**
```bash
# Convert all assets to all formats
make convert-all

# Convert to PNG only
make convert-png

# Convert to JPG only
make convert-jpg

# Convert to TIFF only
make convert-tiff

# Clean generated files
make clean-converted
```

## Format Specifications

### TIFF (Archival Quality)

**Purpose:** Long-term archival, print production, professional use

**Settings:**
- Colour space: sRGB
- Bit depth: 24-bit (8 bits per channel)
- Compression: LZW (lossless)
- DPI: 300 (print quality)
- Sizes: Original dimensions from SVG

**Usage examples:**
- Print media (brochures, posters)
- Professional documentation
- Legal archival
- Museum/library collections

**Command (ImageMagick):**
```bash
convert -density 300 -background none -colorspace sRGB \
  -compress lzw input.svg output.tiff
```

**Command (Inkscape):**
```bash
inkscape --export-type=png --export-dpi=300 --export-background-opacity=0 \
  input.svg -o temp.png
convert temp.png -compress lzw output.tiff
rm temp.png
```

### PNG (Web Optimised)

**Purpose:** Web display, documentation, transparent backgrounds

**Settings:**
- Colour space: sRGB
- Bit depth: 24-bit + 8-bit alpha (RGBA)
- Compression: PNG (lossless)
- Sizes: 512px, 1024px, 2048px (width)
- Background: Transparent
- Optimisation: Enabled (pngquant/oxipng)

**Usage examples:**
- Website assets
- GitHub README badges
- Documentation images
- Social media (transparent)

**Command (ImageMagick):**
```bash
# 512px width
convert -density 300 -background none -resize 512x \
  input.svg output-512.png

# 1024px width
convert -density 300 -background none -resize 1024x \
  input.svg output-1024.png

# 2048px width
convert -density 300 -background none -resize 2048x \
  input.svg output-2048.png

# Optimise
pngquant --quality=80-95 --ext .png --force output-*.png
```

**Command (Inkscape):**
```bash
inkscape --export-type=png --export-width=512 \
  --export-background-opacity=0 input.svg -o output-512.png

inkscape --export-type=png --export-width=1024 \
  --export-background-opacity=0 input.svg -o output-1024.png

inkscape --export-type=png --export-width=2048 \
  --export-background-opacity=0 input.svg -o output-2048.png
```

### JPG (High Quality, Web Optimised)

**Purpose:** Web display without transparency, smaller file sizes

**Settings:**
- Colour space: sRGB
- Bit depth: 24-bit (8 bits per channel)
- Compression: JPEG (lossy, quality 90-95%)
- Sizes: 512px, 1024px, 2048px (width)
- Background: White (or specified colour)
- Optimisation: Enabled (jpegoptim/mozjpeg)

**Usage examples:**
- Social media posts (non-transparent)
- Email signatures
- Blog posts
- Documentation (where transparency not needed)

**Command (ImageMagick):**
```bash
# 512px width, white background
convert -density 300 -background white -flatten -resize 512x \
  -quality 95 input.svg output-512.jpg

# 1024px width
convert -density 300 -background white -flatten -resize 1024x \
  -quality 95 input.svg output-1024.jpg

# 2048px width
convert -density 300 -background white -flatten -resize 2048x \
  -quality 95 input.svg output-2048.jpg

# Optimise
jpegoptim --max=95 --strip-all output-*.jpg
```

**Command (Inkscape):**
```bash
# Inkscape exports PNG first, then convert to JPG
inkscape --export-type=png --export-width=512 \
  --export-background=white input.svg -o temp-512.png

convert temp-512.png -quality 95 output-512.jpg
rm temp-512.png
```

## Tool Requirements

### ImageMagick

**Installation:**
```bash
# Debian/Ubuntu
sudo apt-get install imagemagick

# macOS (Homebrew)
brew install imagemagick

# Arch Linux
sudo pacman -S imagemagick

# Verify installation
convert --version
```

**Note:** ImageMagick 7+ recommended for better SVG handling.

### Inkscape

**Installation:**
```bash
# Debian/Ubuntu
sudo apt-get install inkscape

# macOS (Homebrew)
brew install --cask inkscape

# Arch Linux
sudo pacman -S inkscape

# Verify installation
inkscape --version
```

**Note:** Inkscape 1.0+ required for command-line export features.

### Node.js and sharp

**Installation:**
```bash
# Install Node.js (if not already installed)
# Debian/Ubuntu
sudo apt-get install nodejs npm

# macOS (Homebrew)
brew install node

# Install sharp
npm install sharp sharp-cli

# Verify installation
node --version
npm list sharp
```

**Note:** sharp requires Node.js 14+ and supports SVG via librsvg.

### Optional Optimisation Tools

**PNG Optimisation:**
```bash
# pngquant (lossy, excellent quality)
sudo apt-get install pngquant

# oxipng (lossless)
cargo install oxipng
```

**JPG Optimisation:**
```bash
# jpegoptim
sudo apt-get install jpegoptim

# mozjpeg (better compression)
# Requires compilation from source
# See: https://github.com/mozilla/mozjpeg
```

## Batch Conversion Examples

### Convert All Branding Assets

```bash
cd /path/to/palimpsest-license/assets

# Using shell script
./conversion-scripts/convert.sh -a branding/

# Using Node.js
node conversion-scripts/convert.js --all

# Using Makefile
make convert-all
```

### Convert Single Badge to All Formats

```bash
# Shell script
./convert.sh branding/badge.svg

# Node.js
node convert.js branding/badge.svg
```

### Generate Web-Ready PNGs Only

```bash
# Shell script
./convert.sh -f png -s 512,1024,2048 branding/*.svg

# Makefile
make convert-png
```

## Naming Conventions

Generated files follow this naming pattern:

```
{original-name}-{size}w.{ext}

Examples:
- badge-512w.png
- badge-1024w.png
- badge-2048w.png
- logo-512w.jpg
- icon-1024w.tiff
```

For TIFF files (full resolution):
```
{original-name}-archival.tiff
```

## Quality Checklist

Before committing converted assets:

- [ ] Verify all sizes are correctly generated
- [ ] Check transparency in PNG files
- [ ] Verify colours match original SVG
- [ ] Check file sizes are reasonable
- [ ] Test display on light and dark backgrounds
- [ ] Verify metadata is preserved (title, description)
- [ ] Run optimisation tools
- [ ] Validate accessibility (alt text, ARIA labels)

## Troubleshooting

### ImageMagick SVG Issues

If ImageMagick produces poor SVG conversions:

1. **Use Inkscape instead:**
   ```bash
   inkscape --export-type=png input.svg -o output.png
   ```

2. **Install better delegates:**
   ```bash
   sudo apt-get install librsvg2-bin
   ```

3. **Increase density:**
   ```bash
   convert -density 600 input.svg output.png
   ```

### Sharp SVG Support

Sharp requires librsvg for SVG support:

```bash
# Debian/Ubuntu
sudo apt-get install librsvg2-dev

# macOS
brew install librsvg

# Reinstall sharp
npm rebuild sharp
```

### Colour Space Issues

Ensure sRGB colour space:

```bash
convert input.svg -colorspace sRGB -profile sRGB.icc output.png
```

### File Size Too Large

For PNGs:
```bash
pngquant --quality=80-90 input.png
oxipng -o 6 input.png
```

For JPGs:
```bash
jpegoptim --max=90 input.jpg
```

## Best Practices

1. **Always keep original SVGs:** Never delete source SVG files
2. **Version control:** Commit converted assets separately from SVGs
3. **Optimise for web:** Always run optimisation tools for web assets
4. **Test rendering:** Verify output on multiple devices/browsers
5. **Document changes:** Update CHANGELOG.md when regenerating assets
6. **Accessibility:** Include alt text and ARIA labels in embed code
7. **Transparency:** PNG for transparency, JPG for solid backgrounds
8. **Archival:** Use TIFF for long-term storage and print

## Further Reading

- [ImageMagick Documentation](https://imagemagick.org/index.php)
- [Inkscape Command Line](https://inkscape.org/doc/inkscape-man.html)
- [Sharp Documentation](https://sharp.pixelplumbing.com/)
- [PNG Optimisation Guide](https://css-tricks.com/snippets/html/base64-encode-of-1x1px-transparent-gif/)
- [TIFF Specification](https://www.adobe.io/open/standards/TIFF.html)
- [sRGB Colour Space](https://en.wikipedia.org/wiki/SRGB)

## Licence

These scripts and documentation are part of the Palimpsest License project and are licensed under the same terms. See the main LICENSE.md for details.

---

**Last updated:** 2025-11-22
**Maintainer:** Palimpsest Stewardship Council
