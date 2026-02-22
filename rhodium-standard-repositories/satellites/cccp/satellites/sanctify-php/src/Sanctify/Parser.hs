-- | PHP Parser using Megaparsec
-- SPDX-License-Identifier: AGPL-3.0-or-later
module Sanctify.Parser
    ( -- * Main parsing functions
      parsePhpFile
    , parsePhpString
    , parseStatement
    , parseExpr

      -- * Parser type
    , Parser
    , ParseError

      -- * Re-exports
    , PhpFile(..)
    , Statement(..)
    , Expr(..)
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)
import Control.Monad.Combinators.Expr

import Sanctify.AST

-- | Parser type
type Parser = Parsec Void Text

-- | Parse error type
type ParseError = ParseErrorBundle Text Void

-- | Parse a PHP file from a file path
parsePhpFile :: FilePath -> IO (Either ParseError PhpFile)
parsePhpFile path = do
    content <- T.pack <$> readFile path
    pure $ parsePhpString path content

-- | Parse PHP from a string
parsePhpString :: FilePath -> Text -> Either ParseError PhpFile
parsePhpString = parse phpFileP

-- | Parse a single statement
parseStatement :: Text -> Either ParseError (Located Statement)
parseStatement = parse statementP "<input>"

-- | Parse a single expression
parseExpr :: Text -> Either ParseError (Located Expr)
parseExpr = parse exprP "<input>"

-- | === Lexer === ---

-- | Space consumer (skips whitespace and comments)
sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

-- | Lexeme wrapper
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Symbol parser
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Reserved words
reserved :: Text -> Parser ()
reserved w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

-- | Parse between braces
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | Parse between parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parse between brackets
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | Parse a comma-separated list
commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` symbol ","

-- | Parse a semicolon
semi :: Parser ()
semi = void $ symbol ";"

-- | === PHP Parser === ---

-- | Parse complete PHP file
phpFileP :: Parser PhpFile
phpFileP = do
    sc
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

-- | Parse declare(strict_types=1)
declareStrictP :: Parser Bool
declareStrictP = do
    reserved "declare"
    _ <- parens $ do
        _ <- symbol "strict_types"
        _ <- symbol "="
        n <- L.decimal
        pure (n == (1 :: Int))
    semi
    pure True

-- | Parse namespace declaration
namespaceP :: Parser QualifiedName
namespaceP = do
    reserved "namespace"
    qn <- qualifiedNameP
    semi
    pure qn

-- | Parse use declaration
useP :: Parser UseDecl
useP = do
    reserved "use"
    kind <- option UseClass $ choice
        [ UseFunction <$ reserved "function"
        , UseConstant <$ reserved "const"
        ]
    name <- qualifiedNameP
    alias <- optional (reserved "as" *> nameP)
    semi
    pure UseDecl
        { useName = name
        , useAlias = alias
        , useKind = kind
        }

-- | Parse qualified name
qualifiedNameP :: Parser QualifiedName
qualifiedNameP = do
    absolute <- option False (True <$ symbol "\\")
    parts <- nameP `sepBy1` symbol "\\"
    pure QualifiedName { qnParts = parts, qnAbsolute = absolute }

-- | Parse simple name
nameP :: Parser Name
nameP = lexeme $ do
    first <- letterChar <|> char '_'
    rest <- many (alphaNumChar <|> char '_')
    pure $ Name $ T.pack (first : rest)

-- | Parse variable
variableP :: Parser Variable
variableP = lexeme $ do
    _ <- char '$'
    first <- letterChar <|> char '_'
    rest <- many (alphaNumChar <|> char '_')
    pure $ Variable $ T.pack (first : rest)

-- | Parse statement
statementP :: Parser (Located Statement)
statementP = do
    pos <- getSourcePos
    let loc = toSourcePos pos
    stmt <- choice
        [ StmtIf <$> ifP
        , StmtWhile <$> whileP
        , StmtFor <$> forP
        , StmtForeach <$> foreachP
        , StmtReturn <$> returnP
        , StmtEcho <$> echoP
        , StmtDecl <$> declarationP
        , exprStmtP
        ]
    pure $ Located loc stmt
  where
    ifP = do
        reserved "if"
        cond <- parens exprP
        thenStmts <- braces (many statementP)
        elseStmts <- optional (reserved "else" *> braces (many statementP))
        pure (cond, thenStmts, elseStmts)

    whileP = do
        reserved "while"
        cond <- parens exprP
        body <- braces (many statementP)
        pure (cond, body)

    forP = do
        reserved "for"
        (i, c, u) <- parens $ do
            i <- optional exprP
            semi
            c <- optional exprP
            semi
            u <- optional exprP
            pure (i, c, u)
        body <- braces (many statementP)
        pure (i, c, u, body)

    foreachP = do
        reserved "foreach"
        (e, v, k) <- parens $ do
            e <- exprP
            reserved "as"
            k <- optional $ try (variableP <* symbol "=>")
            v <- variableP
            pure (e, v, k)
        body <- braces (many statementP)
        pure (e, v, k, body)

    returnP = do
        reserved "return"
        e <- optional exprP
        semi
        pure e

    echoP = do
        reserved "echo"
        es <- commaSep exprP
        semi
        pure es

    exprStmtP = do
        e <- exprP
        semi
        pure $ StmtExpr e

-- | Pattern match helpers for statement construction
instance {-# OVERLAPPING #-} Functor ((,,) a b) where
    fmap f (a, b, c) = (a, b, f c)

-- | Parse declaration
declarationP :: Parser Declaration
declarationP = choice
    [ functionP
    , classP
    ]

-- | Parse function
functionP :: Parser Declaration
functionP = do
    reserved "function"
    name <- nameP
    params <- parens (commaSep parameterP)
    ret <- optional returnTypeP
    body <- braces (many statementP)
    pure DeclFunction
        { fnName = name
        , fnParams = params
        , fnReturnType = ret
        , fnBody = body
        , fnAttributes = []
        }

-- | Parse class
classP :: Parser Declaration
classP = do
    mods <- many modifierP
    reserved "class"
    name <- nameP
    ext <- optional (reserved "extends" *> qualifiedNameP)
    impls <- option [] (reserved "implements" *> qualifiedNameP `sepBy1` symbol ",")
    members <- braces (many classMemberP)
    pure DeclClass
        { clsName = name
        , clsModifiers = mods
        , clsExtends = ext
        , clsImplements = impls
        , clsMembers = members
        , clsAttributes = []
        }

-- | Parse modifier
modifierP :: Parser Modifier
modifierP = choice
    [ Static <$ reserved "static"
    , Final <$ reserved "final"
    , Abstract <$ reserved "abstract"
    , Readonly <$ reserved "readonly"
    ]

-- | Parse visibility
visibilityP :: Parser Visibility
visibilityP = choice
    [ Public <$ reserved "public"
    , Protected <$ reserved "protected"
    , Private <$ reserved "private"
    ]

-- | Parse class member
classMemberP :: Parser ClassMember
classMemberP = choice
    [ methodP
    , propertyP
    ]

-- | Parse method
methodP :: Parser ClassMember
methodP = do
    vis <- option Public visibilityP
    mods <- many modifierP
    reserved "function"
    name <- nameP
    params <- parens (commaSep parameterP)
    ret <- optional returnTypeP
    body <- optional (braces (many statementP))
    pure MemberMethod
        { methVisibility = vis
        , methModifiers = mods
        , methName = name
        , methParams = params
        , methReturn = ret
        , methBody = body
        , methAttributes = []
        }

-- | Parse property
propertyP :: Parser ClassMember
propertyP = do
    vis <- visibilityP
    mods <- many modifierP
    mType <- optional typeHintP
    name <- variableP
    def <- optional (symbol "=" *> exprP)
    semi
    pure MemberProperty
        { propVisibility = vis
        , propModifiers = mods
        , propType = mType
        , propName = Name (varName name)
        , propDefault = def
        }

-- | Parse parameter
parameterP :: Parser Parameter
parameterP = do
    mType <- optional typeHintP
    byRef <- option False (True <$ symbol "&")
    variadic <- option False (True <$ symbol "...")
    name <- variableP
    def <- optional (symbol "=" *> exprP)
    pure Parameter
        { paramType = mType
        , paramByRef = byRef
        , paramVariadic = variadic
        , paramName = name
        , paramDefault = def
        , paramVisibility = Nothing
        , paramReadonly = False
        , paramAttributes = []
        }

-- | Parse type hint
typeHintP :: Parser TypeHint
typeHintP = do
    nullable <- option False (True <$ symbol "?")
    t <- phpTypeP
    pure TypeHint { thType = t, thNullable = nullable }

-- | Parse return type
returnTypeP :: Parser ReturnType
returnTypeP = do
    _ <- symbol ":"
    nullable <- option False (True <$ symbol "?")
    t <- phpTypeP
    pure ReturnType { rtType = t, rtNullable = nullable }

-- | Parse PHP type
phpTypeP :: Parser PhpType
phpTypeP = choice
    [ TInt <$ reserved "int"
    , TFloat <$ reserved "float"
    , TString <$ reserved "string"
    , TBool <$ reserved "bool"
    , TArray Nothing <$ reserved "array"
    , TObject Nothing <$ reserved "object"
    , TCallable <$ reserved "callable"
    , TIterable <$ reserved "iterable"
    , TMixed <$ reserved "mixed"
    , TVoid <$ reserved "void"
    , TNever <$ reserved "never"
    , TNull <$ reserved "null"
    , TSelf <$ reserved "self"
    , TStatic <$ reserved "static"
    , TParent <$ reserved "parent"
    , TClass <$> qualifiedNameP
    ]

-- | Parse expression
exprP :: Parser (Located Expr)
exprP = do
    pos <- getSourcePos
    let loc = toSourcePos pos
    expr <- makeExprParser termP operatorTable
    pure $ Located loc expr

-- | Operator table for expression parser
operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [ prefix "!" (ExprUnary OpNot)
      , prefix "-" (ExprUnary OpNeg)
      , prefix "+" (ExprUnary OpPos)
      , prefix "++" (ExprUnary OpPreInc)
      , prefix "--" (ExprUnary OpPreDec)
      ]
    , [ infixL "**" (ExprBinary OpPow) ]
    , [ infixL "*" (ExprBinary OpMul)
      , infixL "/" (ExprBinary OpDiv)
      , infixL "%" (ExprBinary OpMod)
      ]
    , [ infixL "+" (ExprBinary OpAdd)
      , infixL "-" (ExprBinary OpSub)
      , infixL "." (ExprBinary OpConcat)
      ]
    , [ infixL "<<" (ExprBinary OpShiftL)
      , infixL ">>" (ExprBinary OpShiftR)
      ]
    , [ infixN "<" (ExprBinary OpLt)
      , infixN ">" (ExprBinary OpGt)
      , infixN "<=" (ExprBinary OpLte)
      , infixN ">=" (ExprBinary OpGte)
      ]
    , [ infixN "===" (ExprBinary OpIdentical)
      , infixN "!==" (ExprBinary OpNotIdentical)
      , infixN "==" (ExprBinary OpEq)
      , infixN "!=" (ExprBinary OpNeq)
      ]
    , [ infixL "&" (ExprBinary OpBitAnd) ]
    , [ infixL "^" (ExprBinary OpBitXor) ]
    , [ infixL "|" (ExprBinary OpBitOr) ]
    , [ infixL "&&" (ExprBinary OpAnd) ]
    , [ infixL "||" (ExprBinary OpOr) ]
    , [ infixR "??" (ExprBinary OpCoalesce) ]
    , [ infixR "=" ExprAssign ]
    ]
  where
    prefix name f = Prefix (f . wrapLoc <$ symbol name)
    infixL name f = InfixL ((\l r -> f (wrapLoc l) (wrapLoc r)) <$ symbol name)
    infixR name f = InfixR ((\l r -> f (wrapLoc l) (wrapLoc r)) <$ symbol name)
    infixN name f = InfixN ((\l r -> f (wrapLoc l) (wrapLoc r)) <$ symbol name)

    wrapLoc :: Expr -> Located Expr
    wrapLoc e = Located (SourcePos "" 0 0) e

-- | Parse a term (base expression)
termP :: Parser Expr
termP = choice
    [ ExprLiteral <$> literalP
    , ExprVariable <$> variableP
    , try callP
    , ExprConstant <$> qualifiedNameP
    , parens (locNode <$> exprP)
    ]

-- | Parse function call
callP :: Parser Expr
callP = do
    name <- qualifiedNameP
    args <- parens (commaSep argumentP)
    pure $ ExprCall (Located (SourcePos "" 0 0) (ExprConstant name)) args

-- | Parse argument
argumentP :: Parser Argument
argumentP = do
    name <- optional $ try (nameP <* symbol ":")
    unpack <- option False (True <$ symbol "...")
    value <- exprP
    pure Argument
        { argName = name
        , argValue = value
        , argUnpack = unpack
        }

-- | Parse literal
literalP :: Parser Literal
literalP = choice
    [ LitInt <$> lexeme L.decimal
    , LitFloat <$> lexeme L.float
    , LitString <$> stringP
    , LitBool True <$ reserved "true"
    , LitBool False <$ reserved "false"
    , LitNull <$ reserved "null"
    , LitArray <$> arrayP
    ]

-- | Parse string literal
stringP :: Parser Text
stringP = lexeme $ choice
    [ singleQuoted
    , doubleQuoted
    ]
  where
    singleQuoted = char '\'' *> (T.pack <$> manyTill L.charLiteral (char '\''))
    doubleQuoted = char '"' *> (T.pack <$> manyTill L.charLiteral (char '"'))

-- | Parse array literal
arrayP :: Parser [(Maybe (Located Expr), Located Expr)]
arrayP = brackets (commaSep arrayItemP) <|> (reserved "array" *> parens (commaSep arrayItemP))
  where
    arrayItemP = do
        key <- optional $ try (exprP <* symbol "=>")
        value <- exprP
        pure (key, value)

-- | Convert Megaparsec source position to our SourcePos
toSourcePos :: SourcePos -> Sanctify.AST.SourcePos
toSourcePos pos = Sanctify.AST.SourcePos
    { posFile = sourceName pos
    , posLine = unPos (sourceLine pos)
    , posColumn = unPos (sourceColumn pos)
    }
