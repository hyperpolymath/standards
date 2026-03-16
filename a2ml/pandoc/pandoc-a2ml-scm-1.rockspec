-- SPDX-License-Identifier: MIT
-- (PMPL-1.0-or-later preferred; MIT required for LuaRocks OSI-approved policy)
--
-- pandoc-a2ml-scm-1.rockspec — LuaRocks package spec for pandoc-a2ml.
--
-- Provides Pandoc reader, writer, and filter for A2ML (Attested Markup Language).

rockspec_format = "3.0"
package = "pandoc-a2ml"
version = "scm-1"

source = {
   url = "git://github.com/hyperpolymath/pandoc-a2ml.git",
   branch = "main",
}

description = {
   summary = "Pandoc reader, writer, and filter for A2ML (Attested Markup Language)",
   detailed = [[
      pandoc-a2ml provides a custom Pandoc reader, writer, and filter for the
      A2ML (Attested Markup Language) format. A2ML is an attestation-native
      markup language designed for documents that carry trust metadata,
      cryptographic signatures, and provenance information.

      Features:
        - Custom reader: parse .a2ml files into the Pandoc AST
        - Custom writer: render Pandoc AST back to .a2ml format
        - Filter: transform attestation directives within Pandoc pipelines
        - Preserves headings, directive blocks, inline formatting, and lists
   ]],
   homepage = "https://github.com/hyperpolymath/pandoc-a2ml",
   license = "MIT",
   maintainer = "Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>",
   labels = { "pandoc", "a2ml", "markup", "filter", "reader", "writer", "attestation" },
}

dependencies = {
   "lua >= 5.1",
}

build = {
   type = "builtin",
   modules = {
      ["pandoc-a2ml"]          = "a2ml.lua",
      ["pandoc-a2ml.reader"]   = "a2ml-reader.lua",
      ["pandoc-a2ml.writer"]   = "a2ml-writer.lua",
      ["pandoc-a2ml.filter"]   = "a2ml-filter.lua",
   },
   copy_directories = {
      "docs",
      "examples",
   },
}
