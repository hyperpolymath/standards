-- SPDX-License-Identifier: PMPL-1.0-or-later
--
-- a2ml.lua — Pandoc custom reader for A2ML (Attested Markup Language).
--
-- Converts A2ML surface syntax into the Pandoc AST, preserving:
--   - Headings (# through #####)
--   - Directive blocks (@name(attrs): ... @end) as Divs with attributes
--   - Inline formatting (**bold**, *italic*, [link](url), @ref(id))
--   - Bullet lists (- or *)
--   - Fenced code blocks (```)
--   - Opaque payloads (byte-preserved)
--   - Paragraphs (blank-line separated)
--
-- Usage:
--   pandoc -f a2ml.lua input.a2ml -o output.html
--   pandoc -f a2ml.lua input.a2ml -t markdown
--   pandoc -f a2ml.lua input.a2ml -t json
--
-- Spec: /standards/a2ml/SPEC-v1.0.adoc
-- Test vectors: /standards/a2ml/tests/vectors/

-- Declare this as a custom reader for Pandoc.
-- The Reader function receives the input string and returns a Pandoc document.

local PANDOC_VERSION = PANDOC_VERSION or pandoc.types and {0,0,0}

--- Parse A2ML inline formatting within a text string.
--- Handles: **bold**, *italic*, [link](url), @ref(id), escape sequences.
--- Returns a pandoc.Inlines list.
local function parse_inline(text)
  local inlines = pandoc.Inlines{}
  local pos = 1
  local len = #text

  while pos <= len do
    -- Escape sequence: \x produces literal x
    if text:sub(pos, pos) == "\\" and pos < len then
      inlines:insert(pandoc.Str(text:sub(pos + 1, pos + 1)))
      pos = pos + 2

    -- Strong (bold): **...**
    elseif text:sub(pos, pos + 1) == "**" then
      local close = text:find("%*%*", pos + 2, false)
      if close then
        local content = text:sub(pos + 2, close - 1)
        inlines:insert(pandoc.Strong(pandoc.Inlines{pandoc.Str(content)}))
        pos = close + 2
      else
        inlines:insert(pandoc.Str("**"))
        pos = pos + 2
      end

    -- Emphasis (italic): *...*
    elseif text:sub(pos, pos) == "*" and text:sub(pos + 1, pos + 1) ~= "*" then
      local close = text:find("%*", pos + 1, false)
      if close and text:sub(close - 1, close - 1) ~= "*" then
        local content = text:sub(pos + 1, close - 1)
        inlines:insert(pandoc.Emph(pandoc.Inlines{pandoc.Str(content)}))
        pos = close + 1
      else
        inlines:insert(pandoc.Str("*"))
        pos = pos + 1
      end

    -- Link: [label](url)
    elseif text:sub(pos, pos) == "[" then
      local label_end = text:find("]", pos + 1, true)
      if label_end and text:sub(label_end + 1, label_end + 1) == "(" then
        local url_end = text:find(")", label_end + 2, true)
        if url_end then
          local label = text:sub(pos + 1, label_end - 1)
          local url = text:sub(label_end + 2, url_end - 1)
          inlines:insert(pandoc.Link(pandoc.Inlines{pandoc.Str(label)}, url))
          pos = url_end + 1
        else
          inlines:insert(pandoc.Str("["))
          pos = pos + 1
        end
      else
        inlines:insert(pandoc.Str("["))
        pos = pos + 1
      end

    -- Reference: @ref(identifier)
    elseif text:sub(pos, pos + 4) == "@ref(" then
      local ref_end = text:find(")", pos + 5, true)
      if ref_end then
        local ref_id = text:sub(pos + 5, ref_end - 1)
        -- Render as a Pandoc Link targeting #id (internal ref)
        inlines:insert(pandoc.Link(
          pandoc.Inlines{pandoc.Str(ref_id)},
          "#" .. ref_id,
          "",
          pandoc.Attr("", {"a2ml-ref"}, {})
        ))
        pos = ref_end + 1
      else
        inlines:insert(pandoc.Str("@ref("))
        pos = pos + 5
      end

    -- Plain text: accumulate until next special character
    else
      local next_special = text:find("[%*%[\\@]", pos)
      if next_special then
        if next_special > pos then
          inlines:insert(pandoc.Str(text:sub(pos, next_special - 1)))
        end
        pos = next_special
        -- If we matched @ but it's not @ref(, consume the @ as text
        if text:sub(pos, pos) == "@" and text:sub(pos, pos + 4) ~= "@ref(" then
          inlines:insert(pandoc.Str("@"))
          pos = pos + 1
        end
      else
        inlines:insert(pandoc.Str(text:sub(pos)))
        pos = len + 1
      end
    end
  end

  return inlines
end

--- Parse a directive's attribute string, e.g. "id=fig:one,ref=sec:intro".
--- Returns a table of key-value pairs.
local function parse_directive_attrs(attr_str)
  local attrs = {}
  if not attr_str or attr_str == "" then
    return attrs
  end

  for pair in attr_str:gmatch("[^,]+") do
    pair = pair:match("^%s*(.-)%s*$")  -- trim whitespace
    local key, value = pair:match("^(%S+)%s*=%s*(.+)$")
    if key and value then
      -- Strip quotes from value if present
      value = value:match('^"(.*)"$') or value:match("^'(.*)'$") or value
      attrs[key] = value
    end
  end

  return attrs
end

--- Build a Pandoc Attr from directive attributes.
--- Uses 'id' as the element ID, directive name as class,
--- and remaining attrs as key-value pairs.
local function build_attr(directive_name, attrs)
  local id = attrs["id"] or ""
  local classes = {"a2ml-" .. directive_name}
  local kv_pairs = {}

  -- Add data-a2ml attribute for HTML compatibility with test vectors
  table.insert(kv_pairs, {"data-a2ml", directive_name})

  for key, value in pairs(attrs) do
    if key ~= "id" then
      table.insert(kv_pairs, {key, value})
    end
  end

  return pandoc.Attr(id, classes, kv_pairs)
end

--- Main reader function. Pandoc calls this with the raw input.
--- Returns a pandoc.Pandoc document.
function Reader(input, opts)
  local raw = tostring(input)
  local lines = {}
  for line in raw:gmatch("([^\n]*)\n?") do
    table.insert(lines, line)
  end
  -- Remove trailing empty string from final newline split
  if #lines > 0 and lines[#lines] == "" then
    table.remove(lines)
  end

  local blocks = pandoc.Blocks{}
  local i = 1
  local total = #lines

  --- Collect paragraph lines (non-blank, non-special) into a Para block.
  local function collect_paragraph()
    local para_lines = {}
    while i <= total do
      local line = lines[i]
      -- Stop on blank lines, headings, directives, lists, code fences
      if line:match("^%s*$") or
         line:match("^#+%s") or
         line:match("^@%w") or
         line:match("^```") or
         line:match("^%s*[%-*]%s") then
        break
      end
      table.insert(para_lines, line)
      i = i + 1
    end
    if #para_lines > 0 then
      local text = table.concat(para_lines, " ")
      blocks:insert(pandoc.Para(parse_inline(text)))
    end
  end

  while i <= total do
    local line = lines[i]

    -- Blank line: skip
    if line:match("^%s*$") then
      i = i + 1

    -- Heading: # through #####
    elseif line:match("^(#+)%s+(.+)$") then
      local hashes, title = line:match("^(#+)%s+(.+)$")
      local level = math.min(#hashes, 5)
      -- Generate an ID from the title (lowercase, hyphens for spaces)
      local heading_id = title:lower():gsub("[^%w%s%-]", ""):gsub("%s+", "-")
      blocks:insert(pandoc.Header(level, parse_inline(title), pandoc.Attr(heading_id)))
      i = i + 1

    -- Fenced code block: ```lang ... ```
    elseif line:match("^```") then
      local lang = line:match("^```(%S*)")
      if lang == "" then lang = nil end
      i = i + 1
      local code_lines = {}
      while i <= total and not lines[i]:match("^```%s*$") do
        table.insert(code_lines, lines[i])
        i = i + 1
      end
      if i <= total then i = i + 1 end  -- skip closing ```
      local code_text = table.concat(code_lines, "\n")
      local attr = pandoc.Attr("", lang and {lang} or {}, {})
      blocks:insert(pandoc.CodeBlock(code_text, attr))

    -- Directive block: @name(attrs): ... @end
    elseif line:match("^@(%w[%w_%-]*)") then
      local directive_name = line:match("^@(%w[%w_%-]*)")
      local attr_str = line:match("^@%w[%w_%-]*%((.-)%)")
      local attrs = parse_directive_attrs(attr_str)

      i = i + 1

      -- Special handling for @opaque: preserve bytes exactly
      if directive_name == "opaque" then
        local opaque_lines = {}
        while i <= total and lines[i] ~= "@end" do
          table.insert(opaque_lines, lines[i])
          i = i + 1
        end
        if i <= total then i = i + 1 end  -- skip @end
        local opaque_text = table.concat(opaque_lines, "\n")
        local lang = attrs["lang"]

        -- Render opaque as a CodeBlock (preserves content exactly)
        local code_attr = build_attr(directive_name, attrs)
        if lang then
          table.insert(code_attr.classes, lang)
        end
        blocks:insert(pandoc.CodeBlock(opaque_text, code_attr))

      else
        -- General directive: parse body as nested blocks
        local body_lines = {}
        while i <= total and lines[i] ~= "@end" do
          table.insert(body_lines, lines[i])
          i = i + 1
        end
        if i <= total then i = i + 1 end  -- skip @end

        -- Parse the body content into inline elements
        local body_text = table.concat(body_lines, "\n"):match("^%s*(.-)%s*$")
        local body_blocks = pandoc.Blocks{}

        if body_text and body_text ~= "" then
          -- Check if body contains list items
          local has_list = false
          for _, bl in ipairs(body_lines) do
            if bl:match("^%s*[%-*]%s") then
              has_list = true
              break
            end
          end

          if has_list then
            local items = {}
            for _, bl in ipairs(body_lines) do
              local item_text = bl:match("^%s*[%-*]%s+(.+)$")
              if item_text then
                table.insert(items, pandoc.Blocks{pandoc.Plain(parse_inline(item_text))})
              elseif bl:match("^%s*$") then
                -- skip blank lines in list
              else
                -- Non-list line inside directive body
                table.insert(items, pandoc.Blocks{pandoc.Plain(parse_inline(bl))})
              end
            end
            if #items > 0 then
              body_blocks:insert(pandoc.BulletList(items))
            end
          else
            -- Treat as paragraph(s)
            local para_chunks = {}
            local current_chunk = {}
            for _, bl in ipairs(body_lines) do
              if bl:match("^%s*$") then
                if #current_chunk > 0 then
                  table.insert(para_chunks, table.concat(current_chunk, " "))
                  current_chunk = {}
                end
              else
                table.insert(current_chunk, bl)
              end
            end
            if #current_chunk > 0 then
              table.insert(para_chunks, table.concat(current_chunk, " "))
            end
            for _, chunk in ipairs(para_chunks) do
              body_blocks:insert(pandoc.Para(parse_inline(chunk)))
            end
          end
        end

        -- Wrap body in a Div with directive attributes
        local div_attr = build_attr(directive_name, attrs)
        blocks:insert(pandoc.Div(body_blocks, div_attr))
      end

    -- Bullet list: - item or * item
    elseif line:match("^%s*[%-*]%s+") then
      local items = {}
      while i <= total and lines[i]:match("^%s*[%-*]%s+") do
        local item_text = lines[i]:match("^%s*[%-*]%s+(.+)$")
        if item_text then
          table.insert(items, pandoc.Blocks{pandoc.Plain(parse_inline(item_text))})
        end
        i = i + 1
      end
      if #items > 0 then
        blocks:insert(pandoc.BulletList(items))
      end

    -- Paragraph (default: accumulate non-special lines)
    else
      collect_paragraph()
    end
  end

  -- Build document metadata
  local meta = {}
  meta["a2ml-format"] = pandoc.MetaString("1.0.0")

  return pandoc.Pandoc(blocks, pandoc.Meta(meta))
end

--- Extensions table (required by Pandoc custom reader protocol).
Extensions = {
  -- A2ML supports these structural elements
  smart = true,
}
