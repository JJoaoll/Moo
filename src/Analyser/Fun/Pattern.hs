{-|
Module      : Analyser.Fun.Pattern
Description : Pattern matching analysis and type checking for Moo language
Copyright   : (c) 2025 Moo Language Team
License     : GPL-3
Maintainer  : joaoduos@gmail.com
Stability   : experimental
Portability : POSIX

This module implements pattern matching analysis for Moo language constructs.

= Pattern Matching Validation

Validates that patterns correctly match against their expected types:

* Literal patterns ('PLit') match specific values
* Variable patterns ('PVar') bind to any value of the expected type
* Wildcard patterns ('PWildcard') accept any value without binding
* Constructor patterns ('PConstructor') destructure algebraic data types

= Type Checking Process

1. __Pattern Type Validation__: Ensure pattern structure matches expected type
2. __Constructor Validation__: Verify constructor exists and has correct arity
3. __Type Unification__: Unify generic type parameters with concrete types
4. __Recursive Checking__: Validate nested patterns with their inferred types

= Expression Case Analysis

'checkECase' validates pattern-expression pairs in match statements:

* Pattern must match the scrutinee type
* Expression must evaluate to the expected result type
* All case branches must have consistent result types

= Example Usage

@
-- Check literal pattern
checkPattern ctx (PLit (LInt 42)) TInt = Right Ok

-- Check constructor pattern  
checkPattern ctx (PConstructor \"Some\" [PVar \"x\"]) (TData \"Option\" [TInt])

-- Check case expression
checkECase ctx (PConstructor \"Some\" [PVar \"x\"], EVar \"x\") 
           (TData \"Option\" [TInt]) TInt
@
-}

module Analyser.Fun.Pattern where

import Grammar.Expr
import Grammar.Type
import Grammar.Sttm

import Analyser.Error
import Analyser.Fun.Expr
import Analyser.Fun.FunContext.Def
import Analyser.Fun.FunContext.Utils

import Control.Monad

-- | Check a pattern-expression case in match statements.
--
-- Validates that:
-- * Pattern correctly matches the scrutinee type ('pttrnType')
-- * Expression evaluates to the expected result type ('exprType')
--
-- Used in 'SMatch' statement analysis to ensure all cases are well-typed.
--
-- @
-- -- Check case: Some x -> x + 1
-- checkECase ctx (PConstructor \"Some\" [PVar \"x\"], 
--                EBinOp (EVar \"x\") Add (ELit (LInt 1)))
--            (TData \"Option\" [TInt]) TInt
-- @
checkECase :: FunContext -> (Pattern, Expr) -> Type -> Type -> Either Error Ok
checkECase ctx (pttrn, expr) pttrnType exprType = do
  void$checkPattern ctx pttrn pttrnType
  t <- ctx `checkExpr` expr
  if t == exprType then 
    pure Ok
  else
    Left Error -- TODO: Msg

-- | Check that a pattern matches the expected type.
--
-- Validates pattern structure against the scrutinee type:
--
-- [@PVar@] Variable pattern - always matches, binds variable to type
-- [@PWildcard@] Wildcard pattern - always matches, no binding
-- [@PLit@] Literal pattern - must match exact literal type  
-- [@PConstructor@] Constructor pattern - complex validation with type unification
--
-- For constructor patterns:
-- 1. Constructor must exist in the expected data type
-- 2. Number of sub-patterns must match constructor arity
-- 3. Type parameters are unified with concrete types
-- 4. Sub-patterns are recursively validated
--
-- @
-- checkPattern ctx (PVar \"x\") TInt                    -- Always Ok
-- checkPattern ctx (PLit (LInt 42)) TInt               -- Ok if literal matches
-- checkPattern ctx (PConstructor \"Cons\" [PVar \"h\", PVar \"t\"]) 
--              (TData \"List\" [TInt])                 -- Check list constructor
-- @
                                -- scrutinee type
checkPattern :: FunContext -> Pattern -> Type -> Either Error Ok
checkPattern _ (PVar _) _  = pure Ok
checkPattern _ PWildcard _ = pure Ok
checkPattern _ (PLit lit) expectedType = 
  case (lit, expectedType) of 
    (LInt   _, TInt)    -> pure Ok
    (LChar  _, TChar)   -> pure Ok
    (LFloat _, TFloat)  -> pure Ok
    _ -> Left Error -- TODO: Error msg

-- | Check constructor pattern against data type.
--
-- Performs complex validation for constructor patterns:
-- * Verifies constructor exists in the expected data type
-- * Checks arity (number of sub-patterns vs constructor parameters)
-- * Unifies generic type parameters with concrete types
-- * Recursively validates sub-patterns with their inferred types
--
-- The type unification ensures that generic constructors like 'Some :: a -> Option a'
-- are properly instantiated when matched against concrete types like 'Option Int'.
                                          -- so faz sentido ser um data aqui!
                                          --
-- sla se funfa :P
checkPattern ctx (PConstructor constrName pttrns) (TData name ts) = 
  case ctx `findConstrAndTypeDefsByName` constrName of
    Nothing -> Left $ PatternNotFound constrName
    Just (ConstrDef{cParams}, TypeDef{tName, tParams})  -- verifiquei os tamnhos certos? e o ts?
      | tName /= name                   -> Left Error -- TODO: Error Msg
      | length pttrns  /= length cParams -> Left Error -- TODO: Error Msg
      | length tParams /= length ts            -> Left Error -- TODO: Error Mstg
      -- case everythings ok:
      | otherwise -> do
          case unifyTParams (TVar <$> tParams) ts cParams of
            Nothing -> Left Error
            Just ts' ->
              forM_ (zip pttrns ts') $ \(pttrn, expectedType)-> do 
                checkPattern ctx pttrn expectedType

          pure Ok
checkPattern _ _ _ = Left Error -- TODO Msg