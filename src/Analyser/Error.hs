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
import Grammar.Type (Type)

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
--
-- [@TypeNotFound@] Type not found in scope:
--   'TypeNotFound' \"Type 'MyType' is not defined\"
--
-- [@TypeArityMismatch@] Type applied with wrong number of arguments:
--   'TypeArityMismatch' \"List\" 1 0 means type \"List\" expects 1 arg but got 0
--
-- [@TypeMismatch@] Expression type doesn't match expected type:
--   'TypeMismatch' TInt TBool means expected Int but got Bool
--
-- [@UnboundTypeVar@] Unbound type variable in concrete context:
--   'UnboundTypeVar' \"a\" means type variable 'a' is not allowed here
--
-- [@DuplicateDecl@] Variable already declared in current scope:
--   'DuplicateDecl' \"x\" means variable 'x' is already defined in this scope
--
-- [@GlobalNotFound@] Global variable not found:
--   'GlobalNotFound' \"PI\" means global 'PI' is not defined
--
-- [@ConstNotFound@] Constant not found:
--   'ConstNotFound' \"MAX_SIZE\" means constant 'MAX_SIZE' is not defined
--
-- [@ConstrNotFound@] Constructor not found:
--   'ConstrNotFound' \"Some\" means constructor 'Some' is not defined
--
-- [@WrongConstrType@] Constructor used with wrong type:
--   'WrongConstrType' \"Some\" \"Option\" \"Maybe\" means constructor 'Some' belongs to 'Option' but used with 'Maybe'
--
-- [@LiteralTypeMismatch@] Literal doesn't match expected type:
--   'LiteralTypeMismatch' means literal type conflicts with context
--
-- [@PatternTypeMismatch@] Pattern type doesn't match scrutinee:
--   'PatternTypeMismatch' means pattern structure incompatible with matched value
--
-- [@TypeUnificationFailed@] Failed to unify type parameters:
--   'TypeUnificationFailed' means generic type instantiation failed
--
-- [@NotIterableType@] Type cannot be iterated over:
--   'NotIterableType' TInt means Int is not iterable in for loop
--
-- [@ConditionNotBool@] Condition expression is not boolean:
--   'ConditionNotBool' TInt means condition has type Int instead of Bool
data Error 
  = Error                          -- ^ Generic error case
  | VarNotFound Msg                -- ^ Variable not found with error message
  | FunNotFound Msg                -- ^ Function not found with error message  
  | IncorrectArity Msg Int Int     -- ^ Function name, expected arity, actual arity
  | PatternNotFound Msg            -- ^ Pattern matching failure with message
  | TypeNotFound Msg               -- ^ Type not found with error message
  | TypeArityMismatch Msg Int Int  -- ^ Type name, expected arity, actual arity
  | TypeMismatch Type Type         -- ^ Expected type, actual type
  | UnboundTypeVar Msg             -- ^ Unbound type variable name
  | DuplicateDecl Msg              -- ^ Duplicate variable declaration
  | GlobalNotFound Msg             -- ^ Global variable not found
  | ConstNotFound Msg              -- ^ Constant not found
  | ConstrNotFound Msg             -- ^ Constructor not found
  | WrongConstrType Msg Msg Msg    -- ^ Constructor name, expected type, actual type
  | LiteralTypeMismatch            -- ^ Literal type doesn't match expected
  | PatternTypeMismatch            -- ^ Pattern type doesn't match scrutinee
  | TypeUnificationFailed          -- ^ Type parameter unification failed
  | NotIterableType Type           -- ^ Type is not iterable
  | ConditionNotBool Type          -- ^ Condition is not boolean
  | WrongEndName Msg Msg           -- ^ Function name, end-name (expected, actual)
  deriving (Eq, Show)

{-

[1,2,3]
LConstr "Cons" 
  [1, LCostr "Cons" 
  [2, LConstr 
  [3, LConstr "Nil" []]]]

-}