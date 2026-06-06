-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- k9-reader.lua — Pandoc custom reader for K9 (Self-Validating Components)
--
-- Parses .k9.ncl files into Pandoc's internal AST for documentation
-- generation. K9 files are Nickel configuration with a specific schema —
-- this reader extracts the structured metadata and presents it as
-- human-readable documentation.
--
-- Usage:
--   pandoc -f k9-reader.lua component.k9.ncl -o docs.html
--   pandoc -f k9-reader.lua component.k9.ncl -t markdown
--
-- Media type: application/vnd.k9+nickel (IANA registration pending)

function Reader(input, reader_options)
  local source = tostring(input)
  local blocks = {}

  -- Extract key-value pairs from Nickel-style config
  local name = source:match('name%s*=%s*"(.-)"') or "Unknown Component"
  local version = source:match('version%s*=%s*"(.-)"') or "0.0.0"
  local description = source:match('description%s*=%s*"(.-)"') or ""
  local author = source:match('author%s*=%s*"(.-)"') or ""
  local trust_level = source:match("trust_level%s*=%s*'(%w+)") or "Kennel"

  -- Security settings
  local allow_network = source:match("allow_network%s*=%s*(%w+)") or "false"
  local allow_fs_write = source:match("allow_filesystem_write%s*=%s*(%w+)") or "false"
  local allow_subprocess = source:match("allow_subprocess%s*=%s*(%w+)") or "false"

  -- Title
  table.insert(blocks, pandoc.Header(1, pandoc.Inlines("K9 Component: " .. name)))

  -- Metadata table
  table.insert(blocks, pandoc.Header(2, pandoc.Inlines("Metadata")))
  local meta_items = {
    { pandoc.Plain(pandoc.Inlines("**Version:** " .. version)) },
    { pandoc.Plain(pandoc.Inlines("**Author:** " .. author)) },
    { pandoc.Plain(pandoc.Inlines("**Security Level:** " .. trust_level:upper())) },
  }
  if description ~= "" then
    table.insert(meta_items, 1,
      { pandoc.Plain(pandoc.Inlines("**Description:** " .. description)) })
  end
  table.insert(blocks, pandoc.BulletList(meta_items))

  -- Security profile
  table.insert(blocks, pandoc.Header(2, pandoc.Inlines("Security Profile")))
  local security_items = {
    { pandoc.Plain(pandoc.Inlines("Network access: " .. allow_network)) },
    { pandoc.Plain(pandoc.Inlines("Filesystem write: " .. allow_fs_write)) },
    { pandoc.Plain(pandoc.Inlines("Subprocess execution: " .. allow_subprocess)) },
  }
  table.insert(blocks, pandoc.BulletList(security_items))

  -- Security level explanation
  local level_desc = ""
  if trust_level == "Kennel" then
    level_desc = "Data-only. No execution capabilities. Strict sandbox."
  elseif trust_level == "Yard" then
    level_desc = "Nickel evaluation with limited I/O. Capability-based sandbox."
  elseif trust_level == "Hunt" then
    level_desc = "Full execution with shell commands. Signature REQUIRED. Minimal sandbox."
  end
  if level_desc ~= "" then
    table.insert(blocks, pandoc.Para(pandoc.Inlines(
      pandoc.Emph(pandoc.Inlines(level_desc)))))
  end

  -- Recipes (if present)
  local recipes_found = false
  for recipe_name, recipe_cmd in source:gmatch('(%w+)%s*=%s*"(.-)"') do
    if recipe_name == "install" or recipe_name == "validate" or
       recipe_name == "deploy" or recipe_name == "migrate" or
       recipe_name == "rollback" then
      if not recipes_found then
        table.insert(blocks, pandoc.Header(2, pandoc.Inlines("Recipes")))
        recipes_found = true
      end
      table.insert(blocks, pandoc.Header(3, pandoc.Inlines(recipe_name)))
      table.insert(blocks, pandoc.CodeBlock(recipe_cmd, pandoc.Attr("", { "bash" })))
    end
  end

  -- Raw source (for reference)
  table.insert(blocks, pandoc.Header(2, pandoc.Inlines("Source")))
  table.insert(blocks, pandoc.CodeBlock(source, pandoc.Attr("", { "nickel" })))

  return pandoc.Pandoc(blocks)
end
