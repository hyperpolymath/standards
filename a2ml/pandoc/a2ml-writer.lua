-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- a2ml-writer.lua — Pandoc custom writer for A2ML (Attested Markup Language)
--
-- Converts Pandoc's internal AST to A2ML format.
--
-- Usage:
--   pandoc input.md -t a2ml-writer.lua -o output.a2ml
--   pandoc input.html -t a2ml-writer.lua

-- Writer entry point
function Writer(doc, opts)
  local buffer = {}
  local function add(s) table.insert(buffer, s) end

  -- SPDX header
  add(";; SPDX-License-Identifier: AGPL-3.0-or-later")
  add("")

  for _, block in ipairs(doc.blocks) do
    add(render_block(block))
  end

  return table.concat(buffer, "\n")
end

-- Render a block element to A2ML
function render_block(block)
  if block.t == "Header" then
    local hashes = string.rep("#", block.level)
    return hashes .. " " .. render_inlines(block.content) .. "\n"

  elseif block.t == "Para" then
    return render_inlines(block.content) .. "\n"

  elseif block.t == "BulletList" then
    local items = {}
    for _, item in ipairs(block.content) do
      local text = ""
      for _, b in ipairs(item) do
        text = text .. render_block_inline(b)
      end
      table.insert(items, "- " .. text)
    end
    return table.concat(items, "\n") .. "\n"

  elseif block.t == "CodeBlock" then
    local lang = ""
    if block.classes and #block.classes > 0 then
      lang = block.classes[1]
    end
    return "```" .. lang .. "\n" .. block.text .. "\n```\n"

  elseif block.t == "Div" then
    -- Convert Div to A2ML directive
    local classes = block.classes or {}
    local directive_name = "section"
    for _, cls in ipairs(classes) do
      local name = cls:match("^a2ml%-(.+)$")
      if name and name ~= "directive" then
        directive_name = name
      end
    end
    local content = ""
    for _, b in ipairs(block.content) do
      content = content .. render_block(b)
    end
    return "@" .. directive_name .. ":\n" .. content .. "@end\n"

  elseif block.t == "BlockQuote" then
    local content = ""
    for _, b in ipairs(block.content) do
      content = content .. render_block(b)
    end
    return "@abstract:\n" .. content .. "@end\n"

  elseif block.t == "OrderedList" then
    local items = {}
    for idx, item in ipairs(block.content) do
      local text = ""
      for _, b in ipairs(item) do
        text = text .. render_block_inline(b)
      end
      table.insert(items, tostring(idx) .. ". " .. text)
    end
    return table.concat(items, "\n") .. "\n"

  elseif block.t == "HorizontalRule" then
    return "---\n"

  elseif block.t == "RawBlock" then
    return block.text .. "\n"

  else
    return ""
  end
end

-- Render a block as inline text (for list items)
function render_block_inline(block)
  if block.t == "Plain" or block.t == "Para" then
    return render_inlines(block.content)
  else
    return render_block(block)
  end
end

-- Render inline elements to A2ML text
function render_inlines(inlines)
  local result = {}
  for _, inline in ipairs(inlines) do
    table.insert(result, render_inline(inline))
  end
  return table.concat(result)
end

-- Render a single inline element
function render_inline(inline)
  if inline.t == "Str" then
    return inline.text
  elseif inline.t == "Space" then
    return " "
  elseif inline.t == "SoftBreak" then
    return "\n"
  elseif inline.t == "LineBreak" then
    return "\n"
  elseif inline.t == "Strong" then
    return "**" .. render_inlines(inline.content) .. "**"
  elseif inline.t == "Emph" then
    return "*" .. render_inlines(inline.content) .. "*"
  elseif inline.t == "Code" then
    return "`" .. inline.text .. "`"
  elseif inline.t == "Link" then
    local text = render_inlines(inline.content)
    local url = inline.target
    if url:match("^#") then
      -- Internal reference → @ref()
      return "@ref(" .. url:sub(2) .. ")"
    else
      return "[" .. text .. "](" .. url .. ")"
    end
  elseif inline.t == "RawInline" then
    return inline.text
  else
    return ""
  end
end

-- Template (optional — Pandoc uses this for standalone output)
function Template()
  return "$body$"
end
