#!/usr/bin/env bash
#
# convert.sh - SVG Asset Conversion Script for Palimpsest License
#
# This script converts SVG files to PNG, JPG, and TIFF formats using
# ImageMagick and/or Inkscape. It supports multiple sizes and quality
# settings optimised for web, print, and archival use.
#
# Usage:
#   ./convert.sh [options] <input.svg>
#   ./convert.sh -a <directory>
#   ./convert.sh -f png -s 512,1024 badge.svg
#
# Options:
#   -a, --all <dir>        Convert all SVG files in directory
#   -f, --format <fmt>     Convert to specific format (png|jpg|tiff|all)
#   -s, --sizes <sizes>    Comma-separated sizes (e.g., 512,1024,2048)
#   -o, --output <dir>     Output directory (default: ../badges/)
#   -t, --tool <tool>      Conversion tool (imagemagick|inkscape|auto)
#   -O, --optimise         Run optimisation tools (pngquant, jpegoptim)
#   -v, --verbose          Verbose output
#   -h, --help             Show this help message
#
# Examples:
#   ./convert.sh badge.svg
#   ./convert.sh -a ../branding/
#   ./convert.sh -f png -s 256,512,1024 logo.svg
#   ./convert.sh -O badge.svg
#
# Requirements:
#   - ImageMagick (convert command) OR Inkscape
#   - Optional: pngquant, oxipng, jpegoptim for optimisation
#
# Licence: Part of the Palimpsest License project
# Author: Palimpsest Stewardship Council
# Version: 1.0.0

set -euo pipefail

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# Default settings
FORMAT="all"
SIZES=(512 1024 2048)
OUTPUT_DIR="$PROJECT_ROOT/assets/badges"
TOOL="auto"
OPTIMISE=false
VERBOSE=false
CONVERT_ALL=false
INPUT_DIR=""

# Colour codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Colour

# Helper functions
log() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

error() {
    echo -e "${RED}[ERROR]${NC} $*" >&2
}

verbose() {
    if [[ "$VERBOSE" == true ]]; then
        echo -e "${BLUE}[VERBOSE]${NC} $*"
    fi
}

show_help() {
    sed -n '/^# Usage:/,/^$/p' "$0" | sed 's/^# //g' | sed 's/^#//g'
    exit 0
}

# Check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Detect available conversion tool
detect_tool() {
    if [[ "$TOOL" == "auto" ]]; then
        if command_exists inkscape; then
            TOOL="inkscape"
            verbose "Detected Inkscape"
        elif command_exists convert; then
            TOOL="imagemagick"
            verbose "Detected ImageMagick"
        else
            error "Neither Inkscape nor ImageMagick found. Please install one."
            exit 1
        fi
    fi

    verbose "Using conversion tool: $TOOL"
}

# Check dependencies
check_dependencies() {
    local missing=()

    if [[ "$TOOL" == "inkscape" ]] && ! command_exists inkscape; then
        missing+=("inkscape")
    fi

    if [[ "$TOOL" == "imagemagick" ]] && ! command_exists convert; then
        missing+=("imagemagick")
    fi

    if [[ ${#missing[@]} -gt 0 ]]; then
        error "Missing required tools: ${missing[*]}"
        error "Install with: sudo apt-get install ${missing[*]}"
        exit 1
    fi

    if [[ "$OPTIMISE" == true ]]; then
        local opt_missing=()
        ! command_exists pngquant && opt_missing+=("pngquant")
        ! command_exists jpegoptim && opt_missing+=("jpegoptim")

        if [[ ${#opt_missing[@]} -gt 0 ]]; then
            warn "Optimisation tools not found: ${opt_missing[*]}"
            warn "Install with: sudo apt-get install ${opt_missing[*]}"
        fi
    fi
}

# Convert SVG to PNG using Inkscape
convert_to_png_inkscape() {
    local input="$1"
    local size="$2"
    local output="$3"

    verbose "Converting $input to PNG (${size}px) using Inkscape"

    inkscape \
        --export-type=png \
        --export-width="$size" \
        --export-background-opacity=0 \
        "$input" \
        -o "$output" >/dev/null 2>&1
}

# Convert SVG to PNG using ImageMagick
convert_to_png_imagemagick() {
    local input="$1"
    local size="$2"
    local output="$3"

    verbose "Converting $input to PNG (${size}px) using ImageMagick"

    convert \
        -density 300 \
        -background none \
        -resize "${size}x" \
        "$input" \
        "$output"
}

# Convert SVG to JPG using Inkscape + ImageMagick
convert_to_jpg_inkscape() {
    local input="$1"
    local size="$2"
    local output="$3"
    local temp_png="${output%.jpg}.temp.png"

    verbose "Converting $input to JPG (${size}px) using Inkscape"

    # Export to PNG first
    inkscape \
        --export-type=png \
        --export-width="$size" \
        --export-background=white \
        "$input" \
        -o "$temp_png" >/dev/null 2>&1

    # Convert to JPG
    if command_exists convert; then
        convert "$temp_png" -quality 95 "$output"
        rm "$temp_png"
    else
        warn "ImageMagick not found, keeping PNG: $temp_png"
        mv "$temp_png" "${output%.jpg}.png"
    fi
}

# Convert SVG to JPG using ImageMagick
convert_to_jpg_imagemagick() {
    local input="$1"
    local size="$2"
    local output="$3"

    verbose "Converting $input to JPG (${size}px) using ImageMagick"

    convert \
        -density 300 \
        -background white \
        -flatten \
        -resize "${size}x" \
        -quality 95 \
        "$input" \
        "$output"
}

# Convert SVG to TIFF using ImageMagick
convert_to_tiff() {
    local input="$1"
    local output="$2"

    verbose "Converting $input to TIFF using ImageMagick"

    if [[ "$TOOL" == "inkscape" ]]; then
        # Use Inkscape to export high-res PNG first
        local temp_png="${output%.tiff}.temp.png"
        inkscape \
            --export-type=png \
            --export-dpi=300 \
            --export-background-opacity=0 \
            "$input" \
            -o "$temp_png" >/dev/null 2>&1

        if command_exists convert; then
            convert "$temp_png" -compress lzw "$output"
            rm "$temp_png"
        else
            warn "ImageMagick not found for TIFF conversion"
            rm "$temp_png"
            return 1
        fi
    else
        convert \
            -density 300 \
            -background none \
            -colorspace sRGB \
            -compress lzw \
            "$input" \
            "$output"
    fi
}

# Optimise PNG file
optimise_png() {
    local file="$1"

    if command_exists pngquant; then
        verbose "Optimising PNG with pngquant: $file"
        pngquant --quality=80-95 --ext .png --force "$file" 2>/dev/null || true
    fi

    if command_exists oxipng; then
        verbose "Optimising PNG with oxipng: $file"
        oxipng -o 6 "$file" 2>/dev/null || true
    fi
}

# Optimise JPG file
optimise_jpg() {
    local file="$1"

    if command_exists jpegoptim; then
        verbose "Optimising JPG with jpegoptim: $file"
        jpegoptim --max=95 --strip-all "$file" 2>/dev/null || true
    fi
}

# Process single SVG file
process_svg() {
    local input="$1"
    local basename
    basename="$(basename "$input" .svg)"

    log "Processing: $input"

    # Create output directories
    mkdir -p "$OUTPUT_DIR"/{png,jpg,tiff,svg}

    # Copy original SVG
    cp "$input" "$OUTPUT_DIR/svg/$basename.svg"
    success "Copied SVG to $OUTPUT_DIR/svg/$basename.svg"

    # Convert to PNG
    if [[ "$FORMAT" == "all" || "$FORMAT" == "png" ]]; then
        for size in "${SIZES[@]}"; do
            local output="$OUTPUT_DIR/png/${basename}-${size}w.png"

            if [[ "$TOOL" == "inkscape" ]]; then
                convert_to_png_inkscape "$input" "$size" "$output"
            else
                convert_to_png_imagemagick "$input" "$size" "$output"
            fi

            if [[ -f "$output" ]]; then
                success "Created PNG: $output"

                if [[ "$OPTIMISE" == true ]]; then
                    optimise_png "$output"
                fi
            else
                error "Failed to create PNG: $output"
            fi
        done
    fi

    # Convert to JPG
    if [[ "$FORMAT" == "all" || "$FORMAT" == "jpg" ]]; then
        for size in "${SIZES[@]}"; do
            local output="$OUTPUT_DIR/jpg/${basename}-${size}w.jpg"

            if [[ "$TOOL" == "inkscape" ]]; then
                convert_to_jpg_inkscape "$input" "$size" "$output"
            else
                convert_to_jpg_imagemagick "$input" "$size" "$output"
            fi

            if [[ -f "$output" ]]; then
                success "Created JPG: $output"

                if [[ "$OPTIMISE" == true ]]; then
                    optimise_jpg "$output"
                fi
            else
                error "Failed to create JPG: $output"
            fi
        done
    fi

    # Convert to TIFF
    if [[ "$FORMAT" == "all" || "$FORMAT" == "tiff" ]]; then
        local output="$OUTPUT_DIR/tiff/${basename}-archival.tiff"

        if convert_to_tiff "$input" "$output"; then
            success "Created TIFF: $output"
        else
            error "Failed to create TIFF: $output"
        fi
    fi
}

# Parse command line arguments
parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -a|--all)
                CONVERT_ALL=true
                INPUT_DIR="$2"
                shift 2
                ;;
            -f|--format)
                FORMAT="$2"
                shift 2
                ;;
            -s|--sizes)
                IFS=',' read -ra SIZES <<< "$2"
                shift 2
                ;;
            -o|--output)
                OUTPUT_DIR="$2"
                shift 2
                ;;
            -t|--tool)
                TOOL="$2"
                shift 2
                ;;
            -O|--optimise)
                OPTIMISE=true
                shift
                ;;
            -v|--verbose)
                VERBOSE=true
                shift
                ;;
            -h|--help)
                show_help
                ;;
            -*)
                error "Unknown option: $1"
                show_help
                ;;
            *)
                INPUT_FILE="$1"
                shift
                ;;
        esac
    done
}

# Main function
main() {
    parse_args "$@"

    detect_tool
    check_dependencies

    log "Palimpsest License Asset Converter v1.0.0"
    log "Format: $FORMAT"
    log "Sizes: ${SIZES[*]}"
    log "Output: $OUTPUT_DIR"
    log "Tool: $TOOL"
    log "Optimise: $OPTIMISE"
    echo

    if [[ "$CONVERT_ALL" == true ]]; then
        if [[ ! -d "$INPUT_DIR" ]]; then
            error "Directory not found: $INPUT_DIR"
            exit 1
        fi

        local svg_files=()
        while IFS= read -r -d '' file; do
            svg_files+=("$file")
        done < <(find "$INPUT_DIR" -name "*.svg" -print0)

        if [[ ${#svg_files[@]} -eq 0 ]]; then
            warn "No SVG files found in: $INPUT_DIR"
            exit 0
        fi

        log "Found ${#svg_files[@]} SVG files"
        echo

        for svg in "${svg_files[@]}"; do
            process_svg "$svg"
            echo
        done
    else
        if [[ -z "${INPUT_FILE:-}" ]]; then
            error "No input file specified"
            show_help
        fi

        if [[ ! -f "$INPUT_FILE" ]]; then
            error "File not found: $INPUT_FILE"
            exit 1
        fi

        process_svg "$INPUT_FILE"
    fi

    echo
    success "Conversion complete!"
    log "Output directory: $OUTPUT_DIR"
}

# Run main function
main "$@"
