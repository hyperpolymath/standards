# Seven Tentacles - Justfile
# Teaching Compiler Architecture Ages 8-18

# Default recipe: show help
default:
    @just --list

# Build all components
build: build-docs build-agents
    @echo "✅ Build complete!"

# Build documentation
build-docs:
    @echo "📚 Building documentation..."
    @mkdir -p dist/docs
    @asciidoctor -D dist/docs README.adoc VISION.adoc 2>/dev/null || echo "Note: Install asciidoctor for full docs build"
    @echo "✅ Documentation built in dist/docs/"

# Build ReScript agents (when rescript is installed)
build-agents:
    @echo "🐙 Building agents..."
    @if [ -f "node_modules/.bin/rescript" ]; then \
        npx rescript build; \
    else \
        echo "Note: Run 'npm install' to enable ReScript builds"; \
    fi

# Start development server
dev:
    @echo "🚀 Starting development server..."
    @if command -v live-server >/dev/null 2>&1; then \
        live-server --port=3000 --open=tools/reveal-demo.html; \
    elif command -v npx >/dev/null 2>&1; then \
        npx live-server --port=3000 --open=tools/reveal-demo.html; \
    else \
        echo "Starting basic HTTP server..."; \
        python3 -m http.server 3000 || python -m SimpleHTTPServer 3000; \
    fi

# Open the reveal demo
demo:
    @echo "🎭 Opening reveal demo..."
    @if command -v xdg-open >/dev/null 2>&1; then \
        xdg-open tools/reveal-demo.html; \
    elif command -v open >/dev/null 2>&1; then \
        open tools/reveal-demo.html; \
    else \
        echo "Open tools/reveal-demo.html in your browser"; \
    fi

# Open the lesson player
lesson:
    @echo "📖 Opening lesson player..."
    @if command -v xdg-open >/dev/null 2>&1; then \
        xdg-open tools/lesson-player.html; \
    elif command -v open >/dev/null 2>&1; then \
        open tools/lesson-player.html; \
    else \
        echo "Open tools/lesson-player.html in your browser"; \
    fi

# Run tests
test:
    @echo "🧪 Running tests..."
    @if [ -f "package.json" ] && grep -q "\"test\"" package.json; then \
        npm test; \
    else \
        echo "No tests configured yet"; \
    fi

# Watch for changes and rebuild
watch:
    @echo "👀 Watching for changes..."
    @watchexec -e res,adoc,html,css,js "just build"

# Clean build artifacts
clean:
    @echo "🧹 Cleaning..."
    @rm -rf dist lib node_modules/.cache
    @echo "✅ Clean complete!"

# Install dependencies
install:
    @echo "📦 Installing dependencies..."
    @npm install
    @echo "✅ Dependencies installed!"

# Format code
fmt:
    @echo "✨ Formatting code..."
    @if [ -f "node_modules/.bin/prettier" ]; then \
        npx prettier --write "**/*.{js,html,css,json}"; \
    else \
        echo "Note: Run 'npm install' to enable formatting"; \
    fi

# Count curriculum lessons
count-lessons:
    @echo "📊 Counting curriculum lessons..."
    @echo ""
    @echo "Cuttle (Ages 8-12):"
    @for color in red orange yellow green blue indigo violet; do \
        count=$$(find curriculum/cuttle/$$color -name "*.adoc" 2>/dev/null | wc -l); \
        printf "  %-10s %d lessons\n" "$$color:" "$$count"; \
    done
    @echo ""
    @total=$$(find curriculum -name "*.adoc" 2>/dev/null | wc -l)
    @echo "Total: $$total lessons"

# Generate lesson template
new-lesson agent stage number:
    @echo "📝 Creating new lesson..."
    @mkdir -p curriculum/{{stage}}/{{agent}}
    @echo "= Lesson {{number}}: [Title]" > curriculum/{{stage}}/{{agent}}/lesson_{{number}}.adoc
    @echo ":lesson-id: {{stage}}-{{agent}}-{{number}}" >> curriculum/{{stage}}/{{agent}}/lesson_{{number}}.adoc
    @echo ":agent: {{agent}}" >> curriculum/{{stage}}/{{agent}}/lesson_{{number}}.adoc
    @echo ":stage: {{stage}}" >> curriculum/{{stage}}/{{agent}}/lesson_{{number}}.adoc
    @echo "" >> curriculum/{{stage}}/{{agent}}/lesson_{{number}}.adoc
    @echo "== Welcome!" >> curriculum/{{stage}}/{{agent}}/lesson_{{number}}.adoc
    @echo "" >> curriculum/{{stage}}/{{agent}}/lesson_{{number}}.adoc
    @echo "[Add lesson content here]" >> curriculum/{{stage}}/{{agent}}/lesson_{{number}}.adoc
    @echo "✅ Created curriculum/{{stage}}/{{agent}}/lesson_{{number}}.adoc"

# Show project status
status:
    @echo "🐙 Seven Tentacles Project Status"
    @echo "=================================="
    @echo ""
    @echo "📁 Structure:"
    @echo "  Agents:     $(find agents -name '*.res' 2>/dev/null | wc -l) files"
    @echo "  Tools:      $(find tools -name '*.res' -o -name '*.html' 2>/dev/null | wc -l) files"
    @echo "  Languages:  $(find languages -name '*.res' -o -name '*.js' 2>/dev/null | wc -l) files"
    @echo ""
    @echo "📚 Curriculum:"
    @just count-lessons
    @echo ""
    @echo "📖 Documentation:"
    @ls -1 *.adoc docs/*.adoc 2>/dev/null | wc -l | xargs echo "  AsciiDoc files:"
