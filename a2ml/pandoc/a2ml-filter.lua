-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- a2ml-filter.lua — Pandoc Lua filter for A2ML documents.
--
-- Provides post-processing passes over the Pandoc AST produced by the
-- A2ML reader (a2ml.lua). Designed to run as a filter (--lua-filter):
--
--   pandoc -f a2ml.lua input.a2ml --lua-filter=a2ml-filter.lua -o output.html
--
-- Capabilities:
--   1. Cross-reference resolver: @ref(id) → clickable internal links
--   2. Include directive: @include(file.a2ml) → pull in external file content
--   3. TOC generator: auto-generate table of contents from sections
--   4. Diagram rendering: mermaid/graphviz code blocks → images via pandoc.pipe()
--   5. SPDX validator: check SPDX header present, warn if missing
--   6. Metadata enrichment: populate version/date from git if absent
--
-- Extensions table:
--   +smart       (typographic quotes)
--   +includes    (file inclusion)
--   +diagrams    (render diagrams)
--   +validate    (structural validation)
--
-- Spec: /standards/a2ml/SPEC-v1.0.adoc

-- ============================================================================
-- Configuration
-- ============================================================================

--- Map of heading IDs collected during the first pass. Used for cross-ref
--- resolution so that @ref(id) links can be verified against actual targets.
local heading_ids = {}

--- Accumulated TOC entries (each: {level, id, text}).
local toc_entries = {}

--- Track whether an SPDX header was found in the source.
local spdx_found = false

--- Extension flags. Pandoc does not pass extension state to Lua filters, so
--- we enable all capabilities by default and allow users to disable them via
--- metadata fields (e.g. a2ml-includes: false).
local ext = {
  smart    = true,
  includes = true,
  diagrams = true,
  validate = true,
}

-- ============================================================================
-- Utility helpers
-- ============================================================================

--- Read extension flags from document metadata, allowing users to disable
--- individual capabilities. Metadata keys: a2ml-smart, a2ml-includes,
--- a2ml-diagrams, a2ml-validate.
local function read_extension_flags(meta)
  for key, default in pairs(ext) do
    local meta_key = "a2ml-" .. key
    if meta[meta_key] ~= nil then
      local val = meta[meta_key]
      if type(val) == "boolean" then
        ext[key] = val
      elseif pandoc.utils.type(val) == "MetaBool" then
        ext[key] = val
      end
    end
  end
end

--- Issue a warning message via pandoc's logging facility.
--- Falls back to io.stderr if pandoc.log is unavailable (Pandoc < 3.1).
local function warn(msg)
  if pandoc.log and pandoc.log.warn then
    pandoc.log.warn(msg)
  else
    io.stderr:write("WARNING [a2ml-filter]: " .. msg .. "\n")
  end
end

--- Attempt to run a shell command and capture stdout.
--- Returns the output string, or nil on failure.
local function shell_capture(cmd)
  local handle = io.popen(cmd .. " 2>/dev/null")
  if not handle then return nil end
  local result = handle:read("*a")
  handle:close()
  if result and result ~= "" then
    return result:match("^%s*(.-)%s*$")
  end
  return nil
end

-- ============================================================================
-- Pass 1: Collect heading IDs for cross-reference validation
-- ============================================================================

--- First-pass traversal that collects all heading anchors. Run before the
--- main filter so that @ref(id) targets can be verified.
local function collect_headings(doc)
  for _, block in ipairs(doc.blocks) do
    if block.t == "Header" then
      local id = block.attr.identifier
      if id and id ~= "" then
        heading_ids[id] = true
        table.insert(toc_entries, {
          level = block.level,
          id    = id,
          text  = pandoc.utils.stringify(block.content),
        })
      end
    end
  end
end

-- ============================================================================
-- Filter 1: Cross-reference resolver
-- ============================================================================

--- Resolve @ref(id) links. The A2ML reader already converts these to Link
--- elements with class "a2ml-ref". This filter verifies the target exists
--- and adds a title attribute for hover text. Unresolved refs get a warning.
local function resolve_crossrefs(link)
  if link.t ~= "Link" then return nil end

  -- Check if this is an a2ml-ref link (class set by the reader)
  local is_ref = false
  for _, cls in ipairs(link.attr.classes) do
    if cls == "a2ml-ref" then
      is_ref = true
      break
    end
  end
  if not is_ref then return nil end

  -- Extract the target ID (strip leading #)
  local target_id = link.target:match("^#(.+)$")
  if not target_id then return nil end

  if heading_ids[target_id] then
    -- Valid reference: add title for tooltip
    link.title = "Section: " .. target_id
    link.attr.classes = {"a2ml-ref", "a2ml-ref-resolved"}
    return link
  else
    -- Unresolved reference: warn and mark visually
    if ext.validate then
      warn("Unresolved cross-reference: @ref(" .. target_id .. ")")
    end
    link.attr.classes = {"a2ml-ref", "a2ml-ref-unresolved"}
    return link
  end
end

-- ============================================================================
-- Filter 2: Include directive
-- ============================================================================

--- Process @include(file.a2ml) directives. These appear as Div elements
--- with class "a2ml-include" (produced by the reader for @include directives).
--- The included file is read and parsed via pandoc.read(), then its blocks
--- replace the Div in-place.
local function process_includes(div)
  if div.t ~= "Div" then return nil end
  if not ext.includes then return nil end

  -- Check for a2ml-include class
  local is_include = false
  for _, cls in ipairs(div.attr.classes) do
    if cls == "a2ml-include" then
      is_include = true
      break
    end
  end
  if not is_include then return nil end

  -- Extract the file path from the data-a2ml attribute or from the content
  local file_path = nil
  for _, kv in ipairs(div.attr.attributes) do
    if kv[1] == "file" or kv[1] == "src" then
      file_path = kv[2]
      break
    end
  end

  -- Fallback: try to extract path from the Div's text content
  if not file_path then
    local text = pandoc.utils.stringify(div.content)
    file_path = text:match("^%s*(.-)%s*$")
  end

  if not file_path or file_path == "" then
    warn("@include directive has no file path")
    return nil
  end

  -- Read the external file
  local fh = io.open(file_path, "r")
  if not fh then
    warn("@include: cannot open file: " .. file_path)
    -- Return a warning block instead of silently dropping
    return pandoc.Div(
      pandoc.Blocks{pandoc.Para(pandoc.Inlines{
        pandoc.Emph(pandoc.Inlines{
          pandoc.Str("[include failed: " .. file_path .. "]")
        })
      })},
      pandoc.Attr("", {"a2ml-include-error"}, {})
    )
  end

  local content = fh:read("*a")
  fh:close()

  -- Parse the included content as A2ML if the file ends in .a2ml,
  -- otherwise let Pandoc auto-detect the format.
  local format = "markdown"
  if file_path:match("%.a2ml$") then
    -- Use our custom reader if available via pandoc.read with format option
    -- Fallback to markdown parsing for broad compatibility
    format = "markdown"
  end

  local ok, included_doc = pcall(pandoc.read, content, format)
  if ok and included_doc then
    -- Return the included blocks wrapped in a Div for provenance tracking
    return pandoc.Div(
      included_doc.blocks,
      pandoc.Attr("", {"a2ml-included"}, {{"data-source", file_path}})
    )
  else
    warn("@include: failed to parse file: " .. file_path)
    return nil
  end
end

-- ============================================================================
-- Filter 3: TOC generator
-- ============================================================================

--- Generate a table of contents from collected heading entries.
--- Inserts a BulletList at the beginning of the document (after the title
--- heading, if present). The TOC is wrapped in a Div with id "a2ml-toc".
local function generate_toc(doc)
  if #toc_entries == 0 then return doc end

  -- Build nested list items from TOC entries
  local items = {}
  for _, entry in ipairs(toc_entries) do
    local link = pandoc.Link(
      pandoc.Inlines{pandoc.Str(entry.text)},
      "#" .. entry.id,
      "",
      pandoc.Attr("", {"a2ml-toc-link"}, {})
    )
    -- Indent visually by prepending spaces based on level
    local prefix = ""
    if entry.level > 1 then
      prefix = string.rep("  ", entry.level - 1)
    end
    table.insert(items, pandoc.Blocks{
      pandoc.Plain(pandoc.Inlines{pandoc.Str(prefix), link})
    })
  end

  local toc_block = pandoc.Div(
    pandoc.Blocks{
      pandoc.Header(2, pandoc.Inlines{pandoc.Str("Table of Contents")},
        pandoc.Attr("toc-heading", {"a2ml-toc-heading"}, {})),
      pandoc.BulletList(items),
    },
    pandoc.Attr("a2ml-toc", {"a2ml-toc"}, {})
  )

  -- Insert TOC after the first heading (if any), otherwise at the start
  local insert_pos = 1
  for idx, block in ipairs(doc.blocks) do
    if block.t == "Header" and block.level == 1 then
      insert_pos = idx + 1
      break
    end
  end

  table.insert(doc.blocks, insert_pos, toc_block)
  return doc
end

-- ============================================================================
-- Filter 4: Diagram rendering
-- ============================================================================

--- Render code blocks tagged with "mermaid" or "graphviz" (or "dot") into
--- inline SVG or image elements. Uses pandoc.pipe() to invoke the external
--- tool. Gracefully falls back to displaying the source code if the tool
--- is not available.
local function render_diagrams(codeblock)
  if codeblock.t ~= "CodeBlock" then return nil end
  if not ext.diagrams then return nil end

  local lang = nil
  for _, cls in ipairs(codeblock.attr.classes) do
    if cls == "mermaid" or cls == "graphviz" or cls == "dot" then
      lang = cls
      break
    end
  end
  if not lang then return nil end

  local source = codeblock.text
  local svg = nil

  if lang == "mermaid" then
    -- Attempt to render via mmdc (Mermaid CLI)
    local ok, result = pcall(pandoc.pipe, "mmdc", {"-i", "/dev/stdin", "-o", "/dev/stdout", "-e", "svg"}, source)
    if ok and result and result ~= "" then
      svg = result
    end
  elseif lang == "graphviz" or lang == "dot" then
    -- Attempt to render via dot (Graphviz)
    local ok, result = pcall(pandoc.pipe, "dot", {"-Tsvg"}, source)
    if ok and result and result ~= "" then
      svg = result
    end
  end

  if svg then
    -- Return as a raw HTML block containing the SVG
    return pandoc.Div(
      pandoc.Blocks{pandoc.RawBlock("html", svg)},
      pandoc.Attr("", {"a2ml-diagram", "a2ml-diagram-" .. lang}, {})
    )
  else
    -- Graceful fallback: keep the code block but add a note
    warn("Diagram tool not available for '" .. lang .. "'; keeping source code")
    codeblock.attr.classes = {lang, "a2ml-diagram-source"}
    return codeblock
  end
end

-- ============================================================================
-- Filter 5: SPDX validator
-- ============================================================================

--- Check the document for an SPDX-License-Identifier. The A2ML reader may
--- set this in metadata; alternatively, we scan the first few blocks for
--- a comment or Para containing "SPDX-License-Identifier".
local function validate_spdx(doc)
  if not ext.validate then return doc end

  -- Check metadata
  if doc.meta["spdx-license"] then
    spdx_found = true
    return doc
  end

  -- Scan first 5 blocks for SPDX mention
  local limit = math.min(#doc.blocks, 5)
  for idx = 1, limit do
    local block = doc.blocks[idx]
    local text = pandoc.utils.stringify(block)
    if text:find("SPDX%-License%-Identifier") then
      spdx_found = true
      break
    end
  end

  if not spdx_found then
    warn("A2ML document is missing SPDX-License-Identifier header")
  end

  return doc
end

-- ============================================================================
-- Filter 6: Metadata enrichment
-- ============================================================================

--- Populate version and date metadata from git if not already set in the
--- document. Uses git log and git describe for version/date extraction.
local function enrich_metadata(meta)
  -- Populate date from git if missing
  if not meta["date"] then
    local git_date = shell_capture("git log -1 --format=%ci 2>/dev/null")
    if git_date then
      -- Extract just the YYYY-MM-DD portion
      local date_str = git_date:match("^(%d%d%d%d%-%d%d%-%d%d)")
      if date_str then
        meta["date"] = pandoc.MetaString(date_str)
      end
    end
  end

  -- Populate version from git describe if missing
  if not meta["version"] then
    local git_version = shell_capture("git describe --tags --always 2>/dev/null")
    if git_version then
      meta["version"] = pandoc.MetaString(git_version)
    end
  end

  -- Populate author from git config if missing
  if not meta["author"] then
    local git_author = shell_capture("git config user.name 2>/dev/null")
    if git_author then
      meta["author"] = pandoc.MetaString(git_author)
    end
  end

  return meta
end

-- ============================================================================
-- Filter pipeline
-- ============================================================================

--- Pandoc runs filter traversals in the order listed in the returned table.
--- We use multiple passes:
---   1. Meta pass: read extension flags and enrich metadata
---   2. Pandoc pass: run all block/inline filters
---   3. Final pass: generate TOC and validate SPDX

-- Return the filter as a list of traversals (Pandoc 3.0+ filter protocol).
return {
  -- Pass 1: Read config, collect headings, enrich metadata
  {
    Pandoc = function(doc)
      read_extension_flags(doc.meta)
      collect_headings(doc)
      doc.meta = enrich_metadata(doc.meta)
      return doc
    end,
  },

  -- Pass 2: Process includes, resolve cross-refs, render diagrams
  {
    Div       = process_includes,
    Link      = resolve_crossrefs,
    CodeBlock = render_diagrams,
  },

  -- Pass 3: Generate TOC, validate SPDX
  {
    Pandoc = function(doc)
      doc = validate_spdx(doc)
      doc = generate_toc(doc)
      return doc
    end,
  },
}

-- ============================================================================
-- Extensions table
-- ============================================================================

--- Extensions advertised by this filter. These are informational; Pandoc does
--- not enforce them for filters (only for readers). Documented here for
--- tooling that inspects available capabilities.
Extensions = {
  smart    = true,   -- Typographic quotes (SmartyPants)
  includes = true,   -- File inclusion via @include directive
  diagrams = true,   -- Diagram rendering (mermaid, graphviz)
  validate = true,   -- Structural validation (SPDX, references)
}
