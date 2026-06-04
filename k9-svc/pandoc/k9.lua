-- SPDX-License-Identifier: MPL-2.0
--
-- k9.lua — Pandoc custom reader for K9 Self-Validating Components.
--
-- Converts K9 files (.k9 and .k9.ncl) into the Pandoc AST, extracting:
--   - Magic number (K9!) and SPDX header
--   - Pedigree metadata (name, version, description, author)
--   - Security level (Kennel/Yard/Hunt) with permission flags
--   - Target platform (OS, edge mode, memory, podman)
--   - Validation block (checksum, pedigree version)
--   - Recipes (install, validate, deploy, migrate)
--   - Configuration records with Nickel contracts
--   - Multiline strings and shell scripts
--   - Comments as annotations
--
-- Handles two K9 syntaxes:
--   .k9     — YAML-style pure data (Kennel level)
--   .k9.ncl — Nickel with contracts and types (Yard/Hunt level)
--
-- Usage:
--   pandoc -f k9.lua input.k9 -o output.html
--   pandoc -f k9.lua input.k9.ncl -t markdown
--   pandoc -f k9.lua config.k9.ncl -t json
--
-- Spec: /standards/k9-svc/SPEC.adoc
-- Pedigree: /standards/k9-svc/pedigree.ncl

--- Detect whether this is YAML-style K9 (.k9) or Nickel (.k9.ncl).
--- Returns "yaml" or "nickel".
local function detect_syntax(raw)
  -- Check for Nickel indicators
  if raw:find("^%s*let%s") or raw:find("\nlet%s") or
     raw:find("import%s") or raw:find("std%.") then
    return "nickel"
  end
  -- Check for YAML separator
  if raw:find("\n%-%-%-\n") or raw:find("^%-%-%-\n") then
    return "yaml"
  end
  -- Default to nickel if .ncl extension patterns found
  if raw:find("'Kennel") or raw:find("'Yard") or raw:find("'Hunt") then
    return "nickel"
  end
  return "yaml"
end

--- Detect the security level from file content.
--- Returns "Kennel", "Yard", or "Hunt".
local function detect_security_level(raw)
  -- Explicit trust_level declaration
  local level = raw:match("trust_level%s*=%s*'(%w+)")
  if level then return level end

  -- Comment annotation
  level = raw:match("Security%s+[Ll]evel:%s*'(%w+)")
  if level then return level end

  -- Infer from content
  if raw:find("allow_subprocess%s*=%s*true") or
     raw:find("allow_network%s*=%s*true") or
     raw:find("signature%s*=") then
    return "Hunt"
  end
  if raw:find("let%s") or raw:find("std%.contract") then
    return "Yard"
  end
  return "Kennel"
end

--- Extract the SPDX license identifier from comments.
local function extract_spdx(raw)
  return raw:match("SPDX%-License%-Identifier:%s*(%S+)")
end

--- Extract K9! magic number presence.
local function has_magic(raw)
  return raw:sub(1, 3) == "K9!"
end

--- Parse a Nickel record block into key-value pairs.
--- Handles simple assignments: key = value, key = "string", key = 'Variant.
--- Returns a table of {key, value, contract, comment} entries.
local function parse_nickel_record(lines, start_idx, end_idx)
  local entries = {}
  local i = start_idx

  while i <= end_idx do
    local line = lines[i]

    -- Skip blank lines and pure comments
    if line:match("^%s*$") or line:match("^%s*#") then
      -- Extract comment for annotation
      local comment = line:match("^%s*#%s*(.+)")
      if comment and #entries > 0 then
        entries[#entries].comment = comment
      end
      i = i + 1

    -- Simple assignment: key = value
    elseif line:match("^%s*(%w[%w_%-]*)%s*=") then
      local key = line:match("^%s*(%w[%w_%-]*)")
      local value = line:match("=%s*(.+)%s*,?%s*$")
      if value then
        value = value:gsub(",%s*$", "")  -- strip trailing comma
        value = value:match('^"(.*)"$') or value  -- strip quotes
      end
      table.insert(entries, {key = key, value = value or ""})
      i = i + 1

    -- Contract-annotated field: key | Contract = value
    elseif line:match("^%s*(%w[%w_%-]*)%s*|") then
      local key = line:match("^%s*(%w[%w_%-]*)")
      local contract = line:match("|%s*(.-)%s*[=|]")
      local value = line:match("=%s*(.+)%s*,?%s*$")
      if value then
        value = value:gsub(",%s*$", "")
        value = value:match('^"(.*)"$') or value
      end
      table.insert(entries, {
        key = key,
        value = value or "",
        contract = contract or "",
      })
      i = i + 1

    -- Nested record opening: key = {  or  key | Contract = {
    elseif line:match("{%s*$") then
      local key = line:match("^%s*(%w[%w_%-]*)")
      -- Find matching close brace
      local depth = 1
      local nested_start = i + 1
      local j = i + 1
      while j <= end_idx and depth > 0 do
        local l = lines[j]
        depth = depth + select(2, l:gsub("{", "")) - select(2, l:gsub("}", ""))
        j = j + 1
      end
      local nested_end = j - 1
      if key then
        local sub_entries = parse_nickel_record(lines, nested_start, nested_end - 1)
        table.insert(entries, {key = key, children = sub_entries})
      end
      i = j

    else
      i = i + 1
    end
  end

  return entries
end

--- Render a record's entries as a Pandoc DefinitionList.
--- Each entry becomes a term (key) with definition (value + contract).
local function entries_to_deflist(entries, depth)
  depth = depth or 0
  local items = {}

  for _, entry in ipairs(entries) do
    local term_inlines = pandoc.Inlines{}
    term_inlines:insert(pandoc.Code(entry.key or "?"))

    if entry.contract and entry.contract ~= "" then
      term_inlines:insert(pandoc.Space())
      term_inlines:insert(pandoc.Span(
        pandoc.Inlines{pandoc.Str("|"), pandoc.Space(), pandoc.Str(entry.contract)},
        pandoc.Attr("", {"k9-contract"}, {})
      ))
    end

    local def_blocks = pandoc.Blocks{}

    if entry.children then
      -- Nested record: recurse
      def_blocks:insert(entries_to_deflist(entry.children, depth + 1))
    elseif entry.value and entry.value ~= "" then
      def_blocks:insert(pandoc.Plain(pandoc.Inlines{pandoc.Str(entry.value)}))
    end

    if entry.comment then
      def_blocks:insert(pandoc.Plain(pandoc.Inlines{
        pandoc.Emph(pandoc.Inlines{pandoc.Str(entry.comment)})
      }))
    end

    table.insert(items, {term_inlines, {def_blocks}})
  end

  if #items > 0 then
    return pandoc.DefinitionList(items)
  else
    return pandoc.Null()
  end
end

--- Parse YAML-style K9 (.k9) content after the --- separator.
--- Returns blocks representing the structured data.
local function parse_yaml_k9(lines, start_idx)
  local blocks = pandoc.Blocks{}
  local current_section = nil
  local current_items = {}
  local i = start_idx

  while i <= #lines do
    local line = lines[i]

    -- Top-level key (no indentation, ends with colon)
    if line:match("^(%w[%w_%-]*):%s*$") then
      -- Flush previous section
      if current_section then
        blocks:insert(pandoc.Header(3, pandoc.Inlines{pandoc.Code(current_section)},
          pandoc.Attr("k9-" .. current_section, {"k9-section"}, {})))
        if #current_items > 0 then
          blocks:insert(entries_to_deflist(current_items))
        end
      end
      current_section = line:match("^(%w[%w_%-]*):")
      current_items = {}
      i = i + 1

    -- Top-level key with inline value: key: value
    elseif line:match("^(%w[%w_%-]*):%s+(.+)$") then
      if current_section then
        -- Flush section first
        blocks:insert(pandoc.Header(3, pandoc.Inlines{pandoc.Code(current_section)},
          pandoc.Attr("k9-" .. current_section, {"k9-section"}, {})))
        if #current_items > 0 then
          blocks:insert(entries_to_deflist(current_items))
        end
        current_section = nil
        current_items = {}
      end
      local key, value = line:match("^(%w[%w_%-]*):%s+(.+)$")
      value = value:match('^"(.*)"$') or value
      blocks:insert(pandoc.Para(pandoc.Inlines{
        pandoc.Code(key), pandoc.Str(": "), pandoc.Str(value)
      }))
      i = i + 1

    -- Indented key-value: "  key: value"
    elseif line:match("^%s+(%w[%w_%-]*):%s+(.+)$") then
      local key, value = line:match("^%s+(%w[%w_%-]*):%s+(.+)$")
      value = value:match('^"(.*)"$') or value
      table.insert(current_items, {key = key, value = value})
      i = i + 1

    -- Indented key with block scalar: "  key: |"
    elseif line:match("^%s+(%w[%w_%-]*):%s*|%s*$") then
      local key = line:match("^%s+(%w[%w_%-]*):")
      i = i + 1
      local scalar_lines = {}
      local indent = nil
      while i <= #lines do
        local sl = lines[i]
        if sl:match("^%s*$") then
          table.insert(scalar_lines, "")
          i = i + 1
        else
          local si = #(sl:match("^(%s*)"))
          if indent == nil then indent = si end
          if si >= indent then
            table.insert(scalar_lines, sl:sub(indent + 1))
            i = i + 1
          else
            break
          end
        end
      end
      local scalar_text = table.concat(scalar_lines, "\n"):match("^(.-)%s*$")
      table.insert(current_items, {key = key, value = scalar_text})

    -- List item: "    - item"
    elseif line:match("^%s+%-+%s+(.+)$") then
      local item = line:match("^%s+%-+%s+(.+)$")
      item = item:match('^"(.*)"$') or item
      table.insert(current_items, {key = "-", value = item})
      i = i + 1

    -- Comment
    elseif line:match("^%s*#") then
      i = i + 1

    -- Blank
    elseif line:match("^%s*$") then
      i = i + 1

    else
      i = i + 1
    end
  end

  -- Flush final section
  if current_section then
    blocks:insert(pandoc.Header(3, pandoc.Inlines{pandoc.Code(current_section)},
      pandoc.Attr("k9-" .. current_section, {"k9-section"}, {})))
    if #current_items > 0 then
      blocks:insert(entries_to_deflist(current_items))
    end
  end

  return blocks
end

--- Parse Nickel-style K9 (.k9.ncl) content.
--- Extracts pedigree blocks, let bindings, record structure, multiline strings.
--- Returns blocks representing the structured data.
local function parse_nickel_k9(lines)
  local blocks = pandoc.Blocks{}
  local i = 1
  local total = #lines

  -- Track let-binding names for section headers
  local in_record = false
  local record_name = nil
  local record_lines = {}
  local record_start = nil
  local brace_depth = 0

  while i <= total do
    local line = lines[i]

    -- Comment block (collect consecutive comments as a note)
    if line:match("^#") and not line:match("^#!") then
      local comment_lines = {}
      while i <= total and lines[i]:match("^#") do
        local text = lines[i]:match("^#%s?(.*)$") or ""
        -- Skip SPDX header (handled in metadata)
        if not text:match("^SPDX%-") then
          table.insert(comment_lines, text)
        end
        i = i + 1
      end
      if #comment_lines > 0 then
        local comment_text = table.concat(comment_lines, "\n"):match("^%s*(.-)%s*$")
        if comment_text ~= "" then
          blocks:insert(pandoc.Div(
            pandoc.Blocks{pandoc.Para(pandoc.Inlines{pandoc.Str(comment_text)})},
            pandoc.Attr("", {"k9-comment"}, {})
          ))
        end
      end

    -- Import statement
    elseif line:match("^%s*let%s+.-%s*=%s*import%s") then
      local binding = line:match("^%s*let%s+(.-)%s*=")
      local path = line:match('import%s+"(.-)"') or line:match("import%s+(%S+)")
      if binding and path then
        blocks:insert(pandoc.Para(pandoc.Inlines{
          pandoc.Strong(pandoc.Inlines{pandoc.Str("import")}),
          pandoc.Space(),
          pandoc.Code(binding),
          pandoc.Str(" = "),
          pandoc.Code(path),
        }))
      end
      i = i + 1

    -- Let binding with record: let name = { ... } in
    elseif line:match("^%s*let%s+(%w[%w_%-]*)%s*[|=]") and not in_record then
      record_name = line:match("^%s*let%s+(%w[%w_%-]*)")
      local contract = line:match("|%s*(.-)%s*=")

      -- Check if record opens on this line
      if line:match("{%s*$") then
        in_record = true
        brace_depth = 1
        record_lines = {}
        record_start = i + 1

        -- Emit section header
        local header_inlines = pandoc.Inlines{pandoc.Code(record_name)}
        if contract then
          header_inlines:insert(pandoc.Space())
          header_inlines:insert(pandoc.Span(
            pandoc.Inlines{pandoc.Str("| " .. contract)},
            pandoc.Attr("", {"k9-contract"}, {})
          ))
        end
        blocks:insert(pandoc.Header(2, header_inlines,
          pandoc.Attr("k9-" .. record_name, {"k9-binding"}, {})))
      else
        -- Single-line let binding
        local value = line:match("=%s*(.+)%s+in%s*$") or line:match("=%s*(.+)$")
        if value then
          blocks:insert(pandoc.Para(pandoc.Inlines{
            pandoc.Strong(pandoc.Inlines{pandoc.Str("let")}),
            pandoc.Space(),
            pandoc.Code(record_name),
            pandoc.Str(" = "),
            pandoc.Code(value:gsub("%s+in%s*$", "")),
          }))
        end
      end
      i = i + 1

    -- Inside a record: track brace depth
    elseif in_record then
      table.insert(record_lines, line)
      brace_depth = brace_depth
        + select(2, line:gsub("{", ""))
        - select(2, line:gsub("}", ""))

      if brace_depth <= 0 then
        -- Record closed — parse contents
        -- Remove closing brace line
        if #record_lines > 0 then
          local last = record_lines[#record_lines]
          record_lines[#record_lines] = last:gsub("}.-$", "")
          if record_lines[#record_lines]:match("^%s*$") then
            table.remove(record_lines)
          end
        end

        local entries = parse_nickel_record(record_lines, 1, #record_lines)
        if #entries > 0 then
          blocks:insert(entries_to_deflist(entries))
        end

        in_record = false
        record_name = nil
        record_lines = {}
      end
      i = i + 1

    -- Top-level export record: { pedigree = ..., layout, }
    elseif line:match("^{%s*$") or line:match("^%s*{%s*$") then
      in_record = true
      brace_depth = 1
      record_lines = {}
      record_name = "export"

      blocks:insert(pandoc.Header(2,
        pandoc.Inlines{pandoc.Str("Export")},
        pandoc.Attr("k9-export", {"k9-export"}, {})))
      i = i + 1

    -- Multiline string: m%"..."% — capture as code block
    elseif line:match('m%%"') then
      local ml_lines = {line}
      if not line:match('"%%') then
        i = i + 1
        while i <= total do
          table.insert(ml_lines, lines[i])
          if lines[i]:match('"%%') then break end
          i = i + 1
        end
      end
      local ml_text = table.concat(ml_lines, "\n")
      blocks:insert(pandoc.CodeBlock(ml_text, pandoc.Attr("", {"k9-multiline"}, {})))
      i = i + 1

    -- Blank line
    elseif line:match("^%s*$") then
      i = i + 1

    -- Catch-all: skip lines we can't parse (e.g. "in", closing parens)
    else
      i = i + 1
    end
  end

  return blocks
end

--- Main reader function. Pandoc calls this with the raw input.
--- Returns a pandoc.Pandoc document.
function Reader(input, opts)
  local raw = tostring(input)
  local lines = {}
  for line in raw:gmatch("([^\n]*)\n?") do
    table.insert(lines, line)
  end
  if #lines > 0 and lines[#lines] == "" then
    table.remove(lines)
  end

  -- Detect format variant
  local syntax = detect_syntax(raw)
  local magic = has_magic(raw)
  local security = detect_security_level(raw)
  local spdx = extract_spdx(raw) or "unknown"

  -- Build document metadata
  local meta = {}
  meta["k9-format"] = pandoc.MetaString(syntax)
  meta["k9-security-level"] = pandoc.MetaString(security)
  meta["k9-magic"] = pandoc.MetaBool(magic)
  meta["spdx-license"] = pandoc.MetaString(spdx)

  -- Extract metadata fields for the title block
  local name = raw:match('name%s*[=:]%s*"([^"]+)"') or raw:match("name%s*[=:]%s*(%S+)")
  local version = raw:match('version%s*[=:]%s*"([^"]+)"') or raw:match("version%s*[=:]%s*(%S+)")
  local description = raw:match('description%s*[=:]%s*"([^"]+)"')
    or raw:match("description%s*[=:]%s*([^\n]+)")

  if name then meta["title"] = pandoc.MetaString(name) end
  if version then meta["version"] = pandoc.MetaString(version) end

  -- Start building the document
  local blocks = pandoc.Blocks{}

  -- Title header with component name and security badge
  local title_text = name or "K9 Component"
  local security_badge = " [" .. security .. "]"

  blocks:insert(pandoc.Header(1,
    pandoc.Inlines{
      pandoc.Str(title_text),
      pandoc.Space(),
      pandoc.Span(
        pandoc.Inlines{pandoc.Str(security_badge)},
        pandoc.Attr("", {"k9-security-badge", "k9-" .. security:lower()}, {})
      ),
    },
    pandoc.Attr("k9-title")))

  -- Pedigree summary block
  local pedigree_items = {}
  if magic then
    table.insert(pedigree_items, pandoc.Inlines{
      pandoc.Strong(pandoc.Inlines{pandoc.Str("Magic")}),
      pandoc.Str(": K9!"),
    })
  end
  if version then
    table.insert(pedigree_items, pandoc.Inlines{
      pandoc.Strong(pandoc.Inlines{pandoc.Str("Version")}),
      pandoc.Str(": " .. version),
    })
  end
  table.insert(pedigree_items, pandoc.Inlines{
    pandoc.Strong(pandoc.Inlines{pandoc.Str("Security")}),
    pandoc.Str(": " .. security),
  })
  table.insert(pedigree_items, pandoc.Inlines{
    pandoc.Strong(pandoc.Inlines{pandoc.Str("Syntax")}),
    pandoc.Str(": " .. syntax),
  })
  table.insert(pedigree_items, pandoc.Inlines{
    pandoc.Strong(pandoc.Inlines{pandoc.Str("License")}),
    pandoc.Str(": " .. spdx),
  })
  if description then
    table.insert(pedigree_items, pandoc.Inlines{
      pandoc.Strong(pandoc.Inlines{pandoc.Str("Description")}),
      pandoc.Str(": " .. description),
    })
  end

  -- Render pedigree as a simple list
  local pedigree_list_items = {}
  for _, item in ipairs(pedigree_items) do
    table.insert(pedigree_list_items, pandoc.Blocks{pandoc.Plain(item)})
  end
  blocks:insert(pandoc.Div(
    pandoc.Blocks{pandoc.BulletList(pedigree_list_items)},
    pandoc.Attr("k9-pedigree-summary", {"k9-pedigree"}, {})
  ))

  -- Parse body content based on syntax
  if syntax == "yaml" then
    -- Find the --- separator and parse YAML content after it
    local yaml_start = nil
    for idx, line in ipairs(lines) do
      if line:match("^%-%-%-") then
        yaml_start = idx + 1
        break
      end
    end
    if yaml_start then
      local yaml_blocks = parse_yaml_k9(lines, yaml_start)
      for _, block in ipairs(yaml_blocks) do
        blocks:insert(block)
      end
    end
  else
    -- Parse Nickel content
    local nickel_blocks = parse_nickel_k9(lines)
    for _, block in ipairs(nickel_blocks) do
      blocks:insert(block)
    end
  end

  -- Append raw source as a collapsible code block for reference
  blocks:insert(pandoc.Header(2,
    pandoc.Inlines{pandoc.Str("Source")},
    pandoc.Attr("k9-source", {"k9-source"}, {})))
  blocks:insert(pandoc.CodeBlock(raw, pandoc.Attr("", {"nickel"}, {})))

  return pandoc.Pandoc(blocks, pandoc.Meta(meta))
end

--- Extensions table (required by Pandoc custom reader protocol).
Extensions = {
  smart = true,
}
