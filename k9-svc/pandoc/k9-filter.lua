-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- k9-filter.lua — Pandoc Lua filter for K9 Self-Validating Component documents.
--
-- Provides post-processing passes over the Pandoc AST produced by the
-- K9 reader (k9.lua). Designed to run as a filter (--lua-filter):
--
--   pandoc -f k9.lua input.k9.ncl --lua-filter=k9-filter.lua -o output.html
--
-- Capabilities:
--   1. Security badge colouring: Kennel=green, Yard=amber, Hunt=red
--   2. Contract highlighting: Nickel contract annotations get distinct styling
--   3. Recipe validation: check recipe commands reference real tools
--   4. Pedigree completeness: warn if name/version/description missing
--
-- Extensions table:
--   +smart       (typographic quotes)
--   +includes    (file inclusion)
--   +diagrams    (render diagrams)
--   +validate    (structural validation)
--
-- Spec: /standards/k9-svc/SPEC.adoc

-- ============================================================================
-- Configuration
-- ============================================================================

--- Security level colour map for badge rendering.
local SECURITY_COLOURS = {
  kennel = { bg = "#2d6a2e", fg = "#ffffff", border = "#1a4d1b", label = "Kennel" },
  yard   = { bg = "#b8860b", fg = "#ffffff", border = "#8b6508", label = "Yard" },
  hunt   = { bg = "#c0392b", fg = "#ffffff", border = "#962d22", label = "Hunt" },
}

--- Known tools that K9 recipes commonly reference. Used for recipe validation.
local KNOWN_TOOLS = {
  -- System tools
  "bash", "sh", "zsh", "env",
  -- Package managers
  "deno", "cargo", "gleam", "mix", "opam", "cabal", "stack", "julia",
  "pip", "npm", "bun", "pnpm", "yarn", "apt", "dnf", "rpm-ostree",
  "nix", "guix", "brew",
  -- Container tools
  "podman", "docker", "buildah", "skopeo",
  -- Build tools
  "make", "just", "cmake", "meson", "ninja", "zig",
  -- Version control
  "git", "gh", "glab",
  -- Utilities
  "curl", "wget", "jq", "yq", "sed", "awk", "grep", "find",
  "tar", "gzip", "unzip", "cp", "mv", "rm", "mkdir", "chmod",
  "cat", "echo", "printf", "test", "true", "false",
  -- Security
  "cosign", "rekor-cli", "syft", "grype", "trivy", "trufflehog",
  -- K9-specific
  "k9-scan", "k9-sign", "panic-attack", "nickel",
  -- Language runtimes
  "node", "python", "python3", "ruby", "elixir", "erl", "idris2",
}

--- Convert KNOWN_TOOLS to a set for O(1) lookup.
local known_tools_set = {}
for _, tool in ipairs(KNOWN_TOOLS) do
  known_tools_set[tool] = true
end

--- Extension flags. Users can override via metadata (k9-validate: false, etc.)
local ext = {
  smart    = true,
  includes = true,
  diagrams = true,
  validate = true,
}

--- Track detected security level for badge rendering.
local security_level = nil

--- Track pedigree fields found.
local pedigree_fields = {
  name        = false,
  version     = false,
  description = false,
}

-- ============================================================================
-- Utility helpers
-- ============================================================================

--- Issue a warning message via pandoc's logging facility.
local function warn(msg)
  if pandoc.log and pandoc.log.warn then
    pandoc.log.warn(msg)
  else
    io.stderr:write("WARNING [k9-filter]: " .. msg .. "\n")
  end
end

--- Read extension flags from document metadata.
local function read_extension_flags(meta)
  for key, _ in pairs(ext) do
    local meta_key = "k9-" .. key
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

-- ============================================================================
-- Filter 1: Security badge colouring
-- ============================================================================

--- Detect the security level from document metadata and apply colour-coded
--- badge styling to Span elements with class "k9-security-badge".
--- Renders as inline HTML with background colour matching the trust level:
---   Kennel = green (safe, pure data)
---   Yard   = amber (contracts, types, moderate trust)
---   Hunt   = red   (subprocess, network, full trust required)
local function colour_security_badges(span)
  if span.t ~= "Span" then return nil end

  local is_badge = false
  local level_class = nil
  for _, cls in ipairs(span.attr.classes) do
    if cls == "k9-security-badge" then
      is_badge = true
    end
    if cls == "k9-kennel" then level_class = "kennel" end
    if cls == "k9-yard"   then level_class = "yard" end
    if cls == "k9-hunt"   then level_class = "hunt" end
  end

  if not is_badge then return nil end

  -- Use detected level if not specified by class
  local level = level_class or (security_level and security_level:lower()) or "kennel"
  local colours = SECURITY_COLOURS[level] or SECURITY_COLOURS.kennel

  -- Build inline-styled HTML badge
  local badge_html = string.format(
    '<span class="k9-security-badge k9-%s" style="' ..
    "display:inline-block;" ..
    "padding:2px 10px;" ..
    "border-radius:4px;" ..
    "font-weight:bold;" ..
    "font-size:0.85em;" ..
    "letter-spacing:0.05em;" ..
    "background-color:%s;" ..
    "color:%s;" ..
    "border:1px solid %s;" ..
    '">%s</span>',
    level,
    colours.bg, colours.fg, colours.border,
    pandoc.utils.stringify(span.content)
  )

  return pandoc.RawInline("html", badge_html)
end

-- ============================================================================
-- Filter 2: Contract highlighting
-- ============================================================================

--- Apply distinct styling to Nickel contract annotations. These appear as
--- Span elements with class "k9-contract" (produced by the K9 reader).
--- Renders contracts in monospace with a type-colour scheme.
local function highlight_contracts(span)
  if span.t ~= "Span" then return nil end

  local is_contract = false
  for _, cls in ipairs(span.attr.classes) do
    if cls == "k9-contract" then
      is_contract = true
      break
    end
  end
  if not is_contract then return nil end

  local contract_text = pandoc.utils.stringify(span.content)

  -- Determine colour based on contract type
  local colour = "#6c3483"  -- default: purple for type contracts
  if contract_text:find("String") or contract_text:find("Str") then
    colour = "#27ae60"  -- green for strings
  elseif contract_text:find("Number") or contract_text:find("Num") or
         contract_text:find("Integer") then
    colour = "#2980b9"  -- blue for numbers
  elseif contract_text:find("Bool") then
    colour = "#e67e22"  -- orange for booleans
  elseif contract_text:find("Array") or contract_text:find("List") then
    colour = "#8e44ad"  -- deep purple for collections
  elseif contract_text:find("Record") or contract_text:find("{") then
    colour = "#2c3e50"  -- dark blue for records
  end

  local html = string.format(
    '<span class="k9-contract" style="' ..
    "font-family:'JetBrains Mono','Fira Code',monospace;" ..
    "font-size:0.9em;" ..
    "color:%s;" ..
    "background-color:#f8f9fa;" ..
    "padding:1px 5px;" ..
    "border-radius:3px;" ..
    "border:1px solid #e0e0e0;" ..
    '">%s</span>',
    colour,
    contract_text
  )

  return pandoc.RawInline("html", html)
end

-- ============================================================================
-- Filter 3: Recipe validation
-- ============================================================================

--- Validate that recipe command blocks reference known tools. Recipes are
--- typically code blocks inside sections headed "recipes", "install",
--- "validate", "deploy", "migrate", etc.
---
--- Checks the first word of each line in recipe code blocks against the
--- known tools list. Warns on unrecognised commands.
local function validate_recipes(codeblock)
  if codeblock.t ~= "CodeBlock" then return nil end
  if not ext.validate then return nil end

  -- Check if this looks like a recipe block (class or surrounding context)
  local is_recipe = false
  for _, cls in ipairs(codeblock.attr.classes) do
    if cls == "bash" or cls == "sh" or cls == "shell" or
       cls == "k9-recipe" or cls == "nickel" then
      is_recipe = true
      break
    end
  end

  -- Also check for recipe-like content (lines starting with commands)
  if not is_recipe then
    local text = codeblock.text
    if text:find("^%s*#!/") or text:find("^%s*set %-") then
      is_recipe = true
    end
  end

  if not is_recipe then return nil end

  -- Extract and validate commands
  local unknown_commands = {}
  for line in codeblock.text:gmatch("[^\n]+") do
    -- Skip comments, blank lines, variable assignments, control flow
    local trimmed = line:match("^%s*(.-)%s*$")
    if trimmed ~= "" and
       not trimmed:match("^#") and
       not trimmed:match("^%w+=") and
       not trimmed:match("^if%s") and
       not trimmed:match("^then") and
       not trimmed:match("^else") and
       not trimmed:match("^fi") and
       not trimmed:match("^for%s") and
       not trimmed:match("^do") and
       not trimmed:match("^done") and
       not trimmed:match("^while") and
       not trimmed:match("^case") and
       not trimmed:match("^esac") and
       not trimmed:match("^%%") and
       not trimmed:match("^{") and
       not trimmed:match("^}") then

      -- Extract the command name (first word, strip env/sudo prefixes)
      local cmd = trimmed:match("^(%S+)")
      if cmd then
        -- Strip common prefixes
        if cmd == "sudo" or cmd == "env" then
          cmd = trimmed:match("^%S+%s+(%S+)")
        end
        -- Strip path prefixes
        if cmd then
          cmd = cmd:match("([^/]+)$") or cmd
        end

        if cmd and not known_tools_set[cmd] then
          unknown_commands[cmd] = true
        end
      end
    end
  end

  -- Warn about unrecognised commands
  for cmd, _ in pairs(unknown_commands) do
    warn("Recipe references unknown tool: '" .. cmd ..
         "' — verify this is available in the target environment")
  end

  return nil  -- do not modify the block
end

-- ============================================================================
-- Filter 4: Pedigree completeness
-- ============================================================================

--- Check that the K9 pedigree contains the required fields: name, version,
--- and description. Issues warnings for any missing fields.
local function check_pedigree_completeness(meta)
  if not ext.validate then return meta end

  -- Check metadata fields set by the K9 reader
  if meta["title"] then
    pedigree_fields.name = true
  end
  if meta["version"] then
    pedigree_fields.version = true
  end
  -- Description is not always in metadata; check blocks later

  return meta
end

--- Scan blocks for pedigree description content.
local function scan_pedigree_blocks(doc)
  if not ext.validate then return doc end

  -- Scan pedigree Div for description
  for _, block in ipairs(doc.blocks) do
    if block.t == "Div" then
      for _, cls in ipairs(block.attr.classes) do
        if cls == "k9-pedigree" then
          local text = pandoc.utils.stringify(block.content)
          if text:find("Description") then
            pedigree_fields.description = true
          end
        end
      end
    end
  end

  -- Issue warnings for missing fields
  local missing = {}
  for field, found in pairs(pedigree_fields) do
    if not found then
      table.insert(missing, field)
    end
  end

  if #missing > 0 then
    warn("K9 pedigree is incomplete. Missing fields: " ..
         table.concat(missing, ", "))
  end

  return doc
end

-- ============================================================================
-- Filter pipeline
-- ============================================================================

--- Pandoc runs filter traversals in the order listed in the returned table.
return {
  -- Pass 1: Read config, detect security level, check pedigree metadata
  {
    Pandoc = function(doc)
      read_extension_flags(doc.meta)

      -- Detect security level from metadata
      local level = doc.meta["k9-security-level"]
      if level then
        security_level = pandoc.utils.stringify(level)
      end

      doc.meta = check_pedigree_completeness(doc.meta)
      return doc
    end,
  },

  -- Pass 2: Apply badge colours, contract highlighting, recipe validation
  {
    Span      = function(span)
      -- Try badge colouring first, then contract highlighting
      local result = colour_security_badges(span)
      if result then return result end
      return highlight_contracts(span)
    end,
    CodeBlock = validate_recipes,
  },

  -- Pass 3: Scan for pedigree completeness
  {
    Pandoc = scan_pedigree_blocks,
  },
}

-- ============================================================================
-- Extensions table
-- ============================================================================

--- Extensions advertised by this filter. Informational for tooling.
Extensions = {
  smart    = true,   -- Typographic quotes (SmartyPants)
  includes = true,   -- File inclusion support
  diagrams = true,   -- Diagram rendering (mermaid, graphviz)
  validate = true,   -- Structural validation (pedigree, recipes, SPDX)
}
