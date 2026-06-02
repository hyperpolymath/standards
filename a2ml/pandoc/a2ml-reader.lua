-- SPDX-License-Identifier: AGPL-3.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- a2ml-reader.lua — Pandoc custom reader for A2ML (Attested Markup Language)
--
-- Parses .a2ml files into Pandoc's internal AST, enabling conversion
-- to any Pandoc output format (HTML, PDF, DOCX, Markdown, etc.).
--
-- Usage:
--   pandoc -f a2ml-reader.lua input.a2ml -o output.html
--   pandoc -f a2ml-reader.lua input.a2ml -t markdown
--
-- A2ML Syntax:
--   # Heading          → Header
--   ## Sub-heading     → Header (level 2)
--   - list item        → BulletList
--   * list item        → BulletList
--   **bold**           → Strong
--   *italic*           → Emph
--   [text](url)        → Link
--   @ref(id)           → Link to #id
--   ```lang ... ```    → CodeBlock
--   @directive: ... @end → Div with class "directive"
--   ;; comment         → stripped (Scheme-style comments)
--   Paragraphs         → Para (separated by blank lines)
--
-- Media type: application/vnd.a2ml (IANA registration pending)

-- Reader entry point — Pandoc calls this with the file content
function Reader(input, reader_options)
  local source = tostring(input)
  local blocks = {}
  local lines = {}

  -- Split into lines
  for line in source:gmatch("([^\n]*)\n?") do
    table.insert(lines, line)
  end

  local i = 1
  local para_buffer = {}

  -- Flush accumulated paragraph text
  local function flush_para()
    if #para_buffer > 0 then
      local text = table.concat(para_buffer, "\n")
      local inlines = parse_inlines(text)
      table.insert(blocks, pandoc.Para(inlines))
      para_buffer = {}
    end
  end

  while i <= #lines do
    local line = lines[i]

    -- Skip Scheme-style comments
    if line:match("^%s*;;") then
      i = i + 1

    -- Headings: # to #####
    elseif line:match("^(#+)%s+(.+)$") then
      flush_para()
      local hashes, title = line:match("^(#+)%s+(.+)$")
      local level = #hashes
      local inlines = parse_inlines(title)
      table.insert(blocks, pandoc.Header(level, inlines))
      i = i + 1

    -- Directive blocks: @name: ... @end
    elseif line:match("^@(%w+)(.-):%s*$") then
      flush_para()
      local name = line:match("^@(%w+)")
      local content_lines = {}
      i = i + 1
      while i <= #lines and not lines[i]:match("^@end%s*$") do
        table.insert(content_lines, lines[i])
        i = i + 1
      end
      i = i + 1 -- skip @end
      local content_text = table.concat(content_lines, "\n")
      local content_inlines = parse_inlines(content_text)
      local div = pandoc.Div(
        { pandoc.Para(content_inlines) },
        pandoc.Attr("", { "a2ml-directive", "a2ml-" .. name })
      )
      table.insert(blocks, div)

    -- Code blocks: ```lang ... ```
    elseif line:match("^```(.*)$") then
      flush_para()
      local lang = line:match("^```(.*)$") or ""
      lang = lang:match("^%s*(.-)%s*$") -- trim
      local code_lines = {}
      i = i + 1
      while i <= #lines and not lines[i]:match("^```%s*$") do
        table.insert(code_lines, lines[i])
        i = i + 1
      end
      i = i + 1 -- skip closing ```
      local code = table.concat(code_lines, "\n")
      local classes = {}
      if lang ~= "" then classes = { lang } end
      table.insert(blocks, pandoc.CodeBlock(code, pandoc.Attr("", classes)))

    -- Bullet lists: - item or * item
    elseif line:match("^%s*[%-*]%s+(.+)$") then
      flush_para()
      local items = {}
      while i <= #lines and lines[i]:match("^%s*[%-*]%s+(.+)$") do
        local item_text = lines[i]:match("^%s*[%-*]%s+(.+)$")
        local item_inlines = parse_inlines(item_text)
        table.insert(items, { pandoc.Plain(item_inlines) })
        i = i + 1
      end
      table.insert(blocks, pandoc.BulletList(items))

    -- Blank lines: flush paragraph
    elseif line:match("^%s*$") then
      flush_para()
      i = i + 1

    -- Regular text: accumulate into paragraph
    else
      table.insert(para_buffer, line)
      i = i + 1
    end
  end

  flush_para()

  return pandoc.Pandoc(blocks)
end

-- Parse inline markup within a text string
function parse_inlines(text)
  local inlines = {}
  local pos = 1

  while pos <= #text do
    -- Strong: **text**
    local s, e, content = text:find("%*%*(.-)%*%*", pos)
    if s == pos then
      table.insert(inlines, pandoc.Strong(pandoc.Inlines(content)))
      pos = e + 1

    -- Emphasis: *text*
    else
      s, e, content = text:find("%*(.-)%*", pos)
      if s == pos then
        table.insert(inlines, pandoc.Emph(pandoc.Inlines(content)))
        pos = e + 1

      -- Link: [text](url)
      else
        s, e, link_text, url = text:find("%[(.-)%]%((.-)%)", pos)
        if s == pos then
          table.insert(inlines, pandoc.Link(pandoc.Inlines(link_text), url))
          pos = e + 1

        -- Reference: @ref(id)
        else
          s, e, ref_id = text:find("@ref%((.-)%)", pos)
          if s == pos then
            table.insert(inlines, pandoc.Link(pandoc.Inlines(ref_id), "#" .. ref_id))
            pos = e + 1

        -- Inline code: `code`
          else
            s, e, code = text:find("`(.-)`", pos)
            if s == pos then
              table.insert(inlines, pandoc.Code(code))
              pos = e + 1

            -- Regular character
            else
              -- Grab text up to next special character
              local next_special = text:find("[%*%[`@]", pos + 1) or (#text + 1)
              local chunk = text:sub(pos, next_special - 1)
              table.insert(inlines, pandoc.Str(chunk))
              pos = next_special
            end
          end
        end
      end
    end
  end

  return pandoc.Inlines(inlines)
end
