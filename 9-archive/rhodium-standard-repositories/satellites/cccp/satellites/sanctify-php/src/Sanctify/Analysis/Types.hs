{-# LANGUAGE StrictData #-}
-- SPDX-License-Identifier: MPL-2.0
module Sanctify.Analysis.Types
  ( -- * Context
    TypeContext(..)
  , emptyTypeContext
  , extendVariable
  , extendFunction

    -- * Function types
  , FunctionType(..)

    -- * Inference results
  , InferredType(..)
  , Certainty(..)
  , InferenceSource(..)
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Sanctify.AST (PhpType, Name, Variable)

-- | Type inference context for a PHP file.
data TypeContext = TypeContext
  { ctxVariables :: Map Variable PhpType
  , ctxFunctions :: Map Name FunctionType
  }
  deriving stock (Eq, Show)

-- | Smart constructor for an empty type context.
emptyTypeContext :: TypeContext
emptyTypeContext = TypeContext Map.empty Map.empty

-- | Bind a variable to a type within the context.
extendVariable :: Variable -> PhpType -> TypeContext -> TypeContext
extendVariable var ty ctx =
  ctx { ctxVariables = Map.insert var ty (ctxVariables ctx) }

-- | Bind a function signature within the context.
extendFunction :: Name -> FunctionType -> TypeContext -> TypeContext
extendFunction name fn ctx =
  ctx { ctxFunctions = Map.insert name fn (ctxFunctions ctx) }

-- | Function signature captured by the analysis.
data FunctionType = FunctionType
  { ftParams :: [(Variable, PhpType)]
  , ftReturn :: PhpType
  }
  deriving stock (Eq, Show)

-- | Result of inferring a PHP expression or declaration.
data InferredType = InferredType
  { inferredPhpType  :: PhpType
  , inferredCertainty :: Certainty
  , inferredNullable :: Bool
  , inferredSource   :: InferenceSource
  }
  deriving stock (Eq, Show)

-- | How confident the inference is.
data Certainty = Certain | Likely | Unknown
  deriving stock (Eq, Show)

-- | Where the inferred type originated from.
data InferenceSource
  = FromLiteral
  | FromDefault
  | FromCallPattern
  | FromWpFunction
 deriving stock (Eq, Show)
