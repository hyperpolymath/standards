-- | PHP Parser using Megaparsec.
-- 
-- This module implements a high-assurance parser for PHP source code. 
-- It is designed to be the foundational layer for `sanctify-php`, 
-- allowing for the transformation of untrusted PHP code into safe, 
//! verified alternatives.
--
-- SPDX-License-Identifier: MPL-2.0

module Sanctify.Parser
    ( -- * Primary API: High-level parsing functions
      parsePhpFile
    , parsePhpString
    , parseStatement
    , parseExpr

      -- * Types
    , Parser
    , ParseError

      -- * AST Re-exports
    , PhpFile(..)
    , Statement(..)
    , Expr(..)
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (isNothing)
import Data.Void (Void)
import Text.Megaparsec hiding (ParseError, SourcePos)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Pos as MPPos
import Control.Monad (void, when, unless, forM_, mapM_)
import Control.Monad.Combinators.Expr

import Sanctify.AST

-- | TYPE DEFINITION: A non-backtracking parser over UTF-8 Text.
type Parser = Parsec Void Text

-- | ERROR MODEL: Detailed error bundles providing source positions and hints.
type ParseError = ParseErrorBundle Text Void

--------------------------------------------------------------------------------
-- LEXER: Tokenization Primitives
--------------------------------------------------------------------------------

-- | SPACE CONSUMER: Handles whitespace, single-line, and block comments.
-- Critical for maintaining Megaparsec's position tracking.
sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

-- | LEXEME: Wraps a parser to automatically consume trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

--------------------------------------------------------------------------------
-- CORE PARSER: PHP Grammar Implementation
--------------------------------------------------------------------------------

-- | PARSER: Reads a complete PHP file, including headers and namespaces.
phpFileP :: Parser PhpFile
phpFileP = do
    sc
    -- Support both standard and short opening tags.
    _ <- optional (symbol "<?php" <|> symbol "<?")
    sc
    strict <- option False declareStrictP
    ns <- optional namespaceP
    uses <- many useP
    stmts <- many statementP
    eof
    pure PhpFile
        { phpDeclareStrict = strict
        , phpNamespace = ns
        , phpUses = uses
        , phpStatements = stmts
        }

-- | STATEMENT DISPATCH: Routes to specific grammar rules for PHP control structures.
statementP :: Parser (Located Statement)
statementP = do
    pos <- getSourcePos
    let loc = toSourcePos pos
    stmt <- choice
        [ (\(cond, thenStmts, elseStmts) -> StmtIf cond thenStmts elseStmts) <$> ifP
        , (\(cond, body) -> StmtWhile cond body) <$> whileP
        -- ... [Routes to Try/Catch, Return, Echo, etc.]
        , exprStmtP
        ]
    pure $ Located loc stmt
