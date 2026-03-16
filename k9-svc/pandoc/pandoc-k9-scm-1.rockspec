-- SPDX-License-Identifier: MIT
-- (PMPL-1.0-or-later preferred; MIT required for LuaRocks OSI-approved policy)
--
-- pandoc-k9-scm-1.rockspec — LuaRocks package spec for pandoc-k9.
--
-- Provides Pandoc reader, writer, and filter for K9 (Self-Validating Components).

rockspec_format = "3.0"
package = "pandoc-k9"
version = "scm-1"

source = {
   url = "git://github.com/hyperpolymath/pandoc-k9.git",
   branch = "main",
}

description = {
   summary = "Pandoc reader, writer, and filter for K9 (Self-Validating Components)",
   detailed = [[
      pandoc-k9 provides a custom Pandoc reader, writer, and filter for the
      K9 format. K9 is a self-validating component specification that embeds
      security levels, pedigree metadata, build recipes, and validation
      checksums directly in the document.

      Features:
        - Custom reader: parse .k9 and .k9.ncl files into the Pandoc AST
        - Custom writer: render Pandoc AST back to K9 format
        - Filter: transform K9 component blocks within Pandoc pipelines
        - Supports both YAML-like (.k9) and Nickel (.k9.ncl) variants
        - Extracts pedigree, security level, target platform, and validation data
   ]],
   homepage = "https://github.com/hyperpolymath/pandoc-k9",
   license = "MIT",
   maintainer = "Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>",
   labels = { "pandoc", "k9", "filter", "reader", "writer", "validation", "security" },
}

dependencies = {
   "lua >= 5.1",
}

build = {
   type = "builtin",
   modules = {
      ["pandoc-k9"]          = "k9.lua",
      ["pandoc-k9.reader"]   = "k9-reader.lua",
      ["pandoc-k9.writer"]   = "k9-writer.lua",
      ["pandoc-k9.filter"]   = "k9-filter.lua",
   },
   copy_directories = {
      "docs",
      "examples",
   },
}
