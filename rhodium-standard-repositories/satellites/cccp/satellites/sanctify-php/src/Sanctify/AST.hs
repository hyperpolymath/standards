{-# LANGUAGE StrictData #-}
-- | PHP Abstract Syntax Tree
-- SPDX-License-Identifier: AGPL-3.0-or-later
module Sanctify.AST
    ( -- * Top-level structures
      PhpFile(..)
    , Statement(..)
    , Declaration(..)
    , Visibility(..)
    , Modifier(..)

      -- * Expressions
    , Expr(..)
    , Literal(..)
    , BinOp(..)
    , UnaryOp(..)

      -- * Types
    , PhpType(..)
    , TypeHint(..)

      -- * Names and identifiers
    , Name(..)
    , QualifiedName(..)
    , Variable(..)

      -- * Function/Method components
    , Parameter(..)
    , Argument(..)
    , ReturnType(..)

      -- * Control flow
    , MatchArm(..)
    , CatchClause(..)

      -- * Source location
    , SourcePos(..)
    , Located(..)
    , withLoc
    ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- | Source position for error reporting
data SourcePos = SourcePos
    { posFile   :: FilePath
    , posLine   :: Int
    , posColumn :: Int
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Wrapper for located AST nodes
data Located a = Located
    { locPos  :: SourcePos
    , locNode :: a
    }
    deriving stock (Eq, Show, Functor, Generic)
    deriving anyclass (ToJSON, FromJSON)

withLoc :: SourcePos -> a -> Located a
withLoc = Located

-- | A complete PHP file
data PhpFile = PhpFile
    { phpDeclareStrict :: Bool           -- ^ Has declare(strict_types=1)
    , phpNamespace     :: Maybe QualifiedName
    , phpUses          :: [UseDecl]
    , phpStatements    :: [Located Statement]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Use/import declaration
data UseDecl = UseDecl
    { useName  :: QualifiedName
    , useAlias :: Maybe Name
    , useKind  :: UseKind
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data UseKind = UseClass | UseFunction | UseConstant
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Variable reference
newtype Variable = Variable { varName :: Text }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Simple name/identifier
newtype Name = Name { unName :: Text }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Qualified name (namespaced)
data QualifiedName = QualifiedName
    { qnParts    :: [Name]
    , qnAbsolute :: Bool  -- ^ Starts with \
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | PHP type annotations
data PhpType
    = TInt
    | TFloat
    | TString
    | TBool
    | TArray (Maybe PhpType)      -- ^ array or array<T>
    | TObject (Maybe QualifiedName)
    | TCallable
    | TIterable
    | TMixed
    | TVoid
    | TNever
    | TNull
    | TUnion [PhpType]
    | TIntersection [PhpType]
    | TNullable PhpType
    | TClass QualifiedName
    | TSelf
    | TStatic
    | TParent
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Type hint with optional nullability
data TypeHint = TypeHint
    { thType     :: PhpType
    , thNullable :: Bool
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Return type declaration
data ReturnType = ReturnType
    { rtType     :: PhpType
    , rtNullable :: Bool
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Visibility modifiers
data Visibility = Public | Protected | Private
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Other modifiers
data Modifier = Static | Final | Abstract | Readonly
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Statement in PHP
data Statement
    = StmtExpr (Located Expr)
    | StmtDecl Declaration
    | StmtIf (Located Expr) [Located Statement] (Maybe [Located Statement])
    | StmtWhile (Located Expr) [Located Statement]
    | StmtFor (Maybe (Located Expr)) (Maybe (Located Expr)) (Maybe (Located Expr)) [Located Statement]
    | StmtForeach (Located Expr) Variable (Maybe Variable) [Located Statement]
    | StmtSwitch (Located Expr) [SwitchCase]
    | StmtMatch (Located Expr) [MatchArm]
    | StmtTry [Located Statement] [CatchClause] (Maybe [Located Statement])
    | StmtReturn (Maybe (Located Expr))
    | StmtThrow (Located Expr)
    | StmtBreak (Maybe Int)
    | StmtContinue (Maybe Int)
    | StmtEcho [Located Expr]
    | StmtGlobal [Variable]
    | StmtStatic [(Variable, Maybe (Located Expr))]
    | StmtUnset [Located Expr]
    | StmtDeclare [(Name, Literal)] [Located Statement]
    | StmtNoop
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Switch case
data SwitchCase = SwitchCase
    { caseExpr :: Maybe (Located Expr)  -- Nothing = default
    , caseBody :: [Located Statement]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Match arm (PHP 8.0+)
data MatchArm = MatchArm
    { matchConditions :: [Located Expr]  -- Empty = default
    , matchResult     :: Located Expr
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Catch clause
data CatchClause = CatchClause
    { catchTypes :: [QualifiedName]
    , catchVar   :: Maybe Variable
    , catchBody  :: [Located Statement]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Declarations
data Declaration
    = DeclFunction
        { fnName       :: Name
        , fnParams     :: [Parameter]
        , fnReturnType :: Maybe ReturnType
        , fnBody       :: [Located Statement]
        , fnAttributes :: [Attribute]
        }
    | DeclClass
        { clsName       :: Name
        , clsModifiers  :: [Modifier]
        , clsExtends    :: Maybe QualifiedName
        , clsImplements :: [QualifiedName]
        , clsMembers    :: [ClassMember]
        , clsAttributes :: [Attribute]
        }
    | DeclInterface
        { ifaceName    :: Name
        , ifaceExtends :: [QualifiedName]
        , ifaceMethods :: [InterfaceMethod]
        }
    | DeclTrait
        { traitName    :: Name
        , traitMembers :: [ClassMember]
        }
    | DeclEnum
        { enumName       :: Name
        , enumBackedType :: Maybe PhpType
        , enumCases      :: [EnumCase]
        , enumMethods    :: [ClassMember]
        }
    | DeclConst Name (Located Expr)
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Class member
data ClassMember
    = MemberProperty
        { propVisibility :: Visibility
        , propModifiers  :: [Modifier]
        , propType       :: Maybe TypeHint
        , propName       :: Name
        , propDefault    :: Maybe (Located Expr)
        }
    | MemberMethod
        { methVisibility :: Visibility
        , methModifiers  :: [Modifier]
        , methName       :: Name
        , methParams     :: [Parameter]
        , methReturn     :: Maybe ReturnType
        , methBody       :: Maybe [Located Statement]  -- Nothing = abstract
        , methAttributes :: [Attribute]
        }
    | MemberConst
        { constVisibility :: Visibility
        , constName       :: Name
        , constValue      :: Located Expr
        }
    | MemberTraitUse [QualifiedName] [TraitAdaptation]
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Interface method signature
data InterfaceMethod = InterfaceMethod
    { imethName   :: Name
    , imethParams :: [Parameter]
    , imethReturn :: Maybe ReturnType
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Enum case
data EnumCase = EnumCase
    { ecaseName  :: Name
    , ecaseValue :: Maybe Literal
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Trait adaptation
data TraitAdaptation
    = TraitPrecedence QualifiedName Name [QualifiedName]
    | TraitAlias (Maybe QualifiedName) Name (Maybe Visibility) (Maybe Name)
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Attribute (PHP 8.0+)
data Attribute = Attribute
    { attrName :: QualifiedName
    , attrArgs :: [Argument]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Function/method parameter
data Parameter = Parameter
    { paramType       :: Maybe TypeHint
    , paramByRef      :: Bool
    , paramVariadic   :: Bool
    , paramName       :: Variable
    , paramDefault    :: Maybe (Located Expr)
    , paramVisibility :: Maybe Visibility  -- Constructor promotion
    , paramReadonly   :: Bool              -- Constructor promotion
    , paramAttributes :: [Attribute]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Function call argument
data Argument = Argument
    { argName   :: Maybe Name  -- Named argument
    , argValue  :: Located Expr
    , argUnpack :: Bool        -- ... spread
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Literals
data Literal
    = LitInt Integer
    | LitFloat Double
    | LitString Text
    | LitBool Bool
    | LitNull
    | LitArray [(Maybe (Located Expr), Located Expr)]
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Binary operators
data BinOp
    = OpAdd | OpSub | OpMul | OpDiv | OpMod | OpPow
    | OpConcat
    | OpEq | OpNeq | OpIdentical | OpNotIdentical
    | OpLt | OpGt | OpLte | OpGte | OpSpaceship
    | OpAnd | OpOr | OpXor
    | OpBitAnd | OpBitOr | OpBitXor | OpShiftL | OpShiftR
    | OpCoalesce
    | OpInstanceOf
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Unary operators
data UnaryOp
    = OpNot | OpBitNot | OpNeg | OpPos
    | OpPreInc | OpPreDec | OpPostInc | OpPostDec
    | OpClone | OpPrint | OpSuppress  -- @ operator
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Expressions
data Expr
    = ExprLiteral Literal
    | ExprVariable Variable
    | ExprBinary BinOp (Located Expr) (Located Expr)
    | ExprUnary UnaryOp (Located Expr)
    | ExprAssign (Located Expr) (Located Expr)
    | ExprAssignOp BinOp (Located Expr) (Located Expr)
    | ExprTernary (Located Expr) (Maybe (Located Expr)) (Located Expr)
    | ExprCall (Located Expr) [Argument]
    | ExprMethodCall (Located Expr) Name [Argument]
    | ExprStaticCall QualifiedName Name [Argument]
    | ExprNullsafeMethodCall (Located Expr) Name [Argument]
    | ExprPropertyAccess (Located Expr) Name
    | ExprNullsafePropertyAccess (Located Expr) Name
    | ExprStaticPropertyAccess QualifiedName Name
    | ExprArrayAccess (Located Expr) (Maybe (Located Expr))
    | ExprNew QualifiedName [Argument]
    | ExprClosure
        { closureStatic :: Bool
        , closureParams :: [Parameter]
        , closureUses   :: [(Variable, Bool)]  -- Bool = by reference
        , closureReturn :: Maybe ReturnType
        , closureBody   :: [Located Statement]
        }
    | ExprArrowFunction
        { arrowParams :: [Parameter]
        , arrowReturn :: Maybe ReturnType
        , arrowExpr   :: Located Expr
        }
    | ExprCast PhpType (Located Expr)
    | ExprIsset [Located Expr]
    | ExprEmpty (Located Expr)
    | ExprEval (Located Expr)  -- Security: flag this!
    | ExprInclude IncludeType (Located Expr)
    | ExprYield (Maybe (Located Expr)) (Maybe (Located Expr))
    | ExprYieldFrom (Located Expr)
    | ExprThrow (Located Expr)
    | ExprClassConstAccess QualifiedName Name
    | ExprConstant QualifiedName
    | ExprShellExec Text  -- Security: flag this!
    | ExprHeredoc Text
    | ExprList [Maybe (Located Expr)]
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Include types
data IncludeType = Include | IncludeOnce | Require | RequireOnce
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
