-- SPDX-License-Identifier: MPL-2.0
-- (PMPL-1.0-or-later preferred; MPL-2.0 required for Hackage OSI-approved policy)
--
-- Data.A2ML.Renderer — Render A2ML AST back to A2ML surface syntax.
--
-- Converts the typed AST from Data.A2ML.Types into A2ML text format,
-- preserving structure and formatting conventions.

module Data.A2ML.Renderer
  ( -- * Rendering
    renderA2ML
  , renderBlock
  , renderInline
  ) where

import           Data.A2ML.Types

import           Data.Text       (Text)
import qualified Data.Text       as T

-- | Render a complete A2ML 'Document' to text.
--
-- ==== Examples
--
-- >>> renderA2ML (Document Nothing [Heading 1 [PlainText "Hello"]])
-- "# Hello\n"
renderA2ML :: Document -> Text
renderA2ML doc = T.intercalate "\n" (map renderBlock (documentBlocks doc)) <> "\n"

-- | Render a single block to A2ML text.
renderBlock :: Block -> Text
renderBlock (Heading level inlines) =
  T.replicate level "#" <> " " <> renderInlines inlines
renderBlock (Paragraph inlines) =
  renderInlines inlines
renderBlock (BulletList items) =
  T.intercalate "\n" (map (\is -> "- " <> renderInlines is) items)
renderBlock (DirectiveBlock dir) =
  renderDirective dir
renderBlock BlankLine =
  ""

-- | Render a directive block.
renderDirective :: Directive -> Text
renderDirective dir =
  "@" <> renderDirectiveName (directiveName dir) <> ":\n"
    <> directiveBody dir <> "\n"
    <> "@end"

-- | Render a directive name to its text representation.
renderDirectiveName :: DirectiveName -> Text
renderDirectiveName DirAbstract    = "abstract"
renderDirectiveName DirRefs        = "refs"
renderDirectiveName DirAttestation = "attestation"
renderDirectiveName DirMeta        = "meta"
renderDirectiveName (DirCustom n)  = n

-- | Render a list of inline elements to text.
renderInlines :: [Inline] -> Text
renderInlines = T.concat . map renderInline

-- | Render a single inline element to text.
renderInline :: Inline -> Text
renderInline (PlainText t)  = t
renderInline (Bold is)      = "**" <> renderInlines is <> "**"
renderInline (Italic is)    = "*" <> renderInlines is <> "*"
renderInline (Link text url) = "[" <> text <> "](" <> url <> ")"
renderInline (InlineRef ref) = "@ref(" <> ref <> ")"
renderInline (InlineCode c)  = "`" <> c <> "`"
