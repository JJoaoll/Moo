{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Analyser.Fun.FunDef
Description : Function definition analysis and type checking for Moo functions
Copyright   : (c) 2025 Moo Language Team
License     : MIT
Maintainer  : joaoduos@gmail.com
Stability   : experimental
Portability : POSIX

This module implements function-level analysis for Moo function definitions.

= Function Analysis

Validates function definitions by:

1. Creating local scope with function parameters
2. Setting up function context with parameter declarations
3. Type checking all statements in the function body
4. Ensuring return type consistency

= Function Context

Creates a 'FunCtx' containing:

* '__getStack__': Local variable scope stack (starts with parameters)
* '__getCtx__': Global analysis context  
* '__getLevel__': Current scope nesting level
* '__getRtrnType__': Expected return type for validation

= Return Type Checking

Currently validates that return statements have correct types, but does not
guarantee that all execution paths return a value (flow analysis TODO).

= Example Usage

@
-- Check function definition
checkFun ctx (FunDef \"factorial\" [Param \"n\" TInt] TInt body)

-- The function context will include:
-- - Parameter \"n\" of type TInt in local scope
-- - Return type TInt for validation
-- - Access to global definitions through ctx
@
-}

module Analyser.Fun.FunDef where

import Analyser.Context.Def
import Analyser.Error

import Grammar.Program

import Analyser.Fun.Sttm
import Analyser.Fun.FunContext.Def

import Data.List.NonEmpty
import Control.Monad

import Analyser.Context.Utils

-- | Check a function definition for type correctness.
--
-- Creates a function analysis context and validates all statements in the body:
--
-- 1. Converts function parameters to local declarations
-- 2. Creates initial scope stack with parameter bindings
-- 3. Sets up function context with return type information
-- 4. Type checks each statement in the function body
--
-- @
-- checkFun ctx (FunDef \"add\" [Param \"x\" TInt, Param \"y\" TInt] TInt 
--               [SReturn (EBinOp (EVar \"x\") Add (EVar \"y\"))])
-- @
--
-- The function ensures that:
-- * All variables are properly declared before use
-- * Return statements match the declared return type  
-- * Control flow statements are well-typed
--
-- Note: Currently does not guarantee that all execution paths return a value.
checkFun :: Context -> FunDef -> Either Error ()
checkFun ctx FunDef{..} = 
  let 
    initialStack = (paramToDecl <$> fParams) :| []
    funCtx = 
      FunCtx {
        _getStack    = initialStack,
        _getCtx      = ctx,
        _getLevel    = 0,
        _getRtrnType = rtrType
      }
  in 
    -- if returns, returs the right type
    -- but no garantee that will return something
    -- without more time to code..
    foldM_ checkSttm funCtx body



