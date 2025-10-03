{-|
Module      : Analyser.Error
Description : Error types and success indicators for Moo language analysis
Copyright   : (c) 2025 Moo Language Team
License     : MIT
Maintainer  : joaoduos@gmail.com
Stability   : experimental
Portability : POSIX

This module defines error types used throughout the Moo language analyser.

= Error Categories

* Variable resolution: 'VarNotFound' for undefined variables
* Function resolution: 'FunNotFound' for undefined functions, 'IncorrectArity' for wrong argument counts
* Pattern matching: 'PatternNotFound' for unmatched patterns
* Success indication: 'Ok' for successful analysis

= Usage Pattern

The analyser uses these error types in 'Either' contexts:

@
analyseExpression :: Expr -> Either Error Type
analyseFunction :: FunCall -> Either Error Type  
analysePattern :: Pattern -> Either Error [VarBinding]
@

= Error Examples

@
-- Variable not in scope
varError = VarNotFound "Variable 'x' not found in current scope"

-- Function undefined
funError = FunNotFound "Function 'factorial' is not defined"

-- Wrong number of arguments
arityError = IncorrectArity "map" 2 1  -- Expected 2, got 1

-- Pattern matching failure
patternError = PatternNotFound "No pattern matches constructor 'InvalidOption'"
@
-}

module Analyser.Error where

import Data.Text

-- | Message type for error descriptions.
-- Uses 'Text' for efficient string handling in error reporting.
type Msg = Text

-- | Success indicator for analysis operations.
-- Represents successful completion of type checking or analysis.
data Ok 
  = Ok deriving (Eq, Show)

-- | Error types for Moo language analysis failures.
--
-- [@Error@] Generic error (fallback case)
--
-- [@VarNotFound@] Variable not found in scope:
--   'VarNotFound' \"Variable 'x' not declared\"
--
-- [@FunNotFound@] Function not found in scope:
--   'FunNotFound' \"Function 'factorial' is undefined\"
--
-- [@IncorrectArity@] Function called with wrong number of arguments:
--   'IncorrectArity' \"map\" 2 1 means function \"map\" expects 2 args but got 1
--
-- [@PatternNotFound@] Pattern matching failed:
--   'PatternNotFound' \"No pattern matches constructor 'InvalidCase'\"
data Error 
  = Error                     -- ^ Generic error case
  | VarNotFound Msg           -- ^ Variable not found with error message
  | FunNotFound Msg           -- ^ Function not found with error message  
  | IncorrectArity Msg Int Int -- ^ Function name, expected arity, actual arity
  | PatternNotFound Msg       -- ^ Pattern matching failure with message
  deriving (Eq, Show)

{-

[1,2,3]
LConstr "Cons" 
  [1, LCostr "Cons" 
  [2, LConstr 
  [3, LConstr "Nil" []]]]

-}