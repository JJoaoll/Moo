{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Analyser.Program
Description : Program-level analysis and type checking for Moo programs
Copyright   : (c) 2025 Moo Language Team
License     : MIT
Maintainer  : joaoduos@gmail.com
Stability   : experimental
Portability : POSIX

This module implements the top-level program analyser for Moo.

= Analysis Pipeline

The program analysis follows these steps:

1. __Main Function Check__: Verify that a 'main' function exists
2. __Type Definition Check__: Validate all custom type definitions
3. __Context Creation__: Build analysis context from global definitions
4. __Global/Constant Check__: Verify global variables and constants
5. __Function Check__: Type check all function definitions

= Error Handling

Analysis uses 'Either' 'Error' for failure cases:

* Missing main function → 'FunNotFound' \"main\"
* Type mismatches → 'Error' (generic)
* Invalid constants → 'Error' (non-literal values)

= Example Usage

@
-- Analyse a complete program
result = checkProgram myProgram
case result of
  Left err -> putStrLn $ "Analysis failed: " ++ show err
  Right () -> putStrLn "Program is well-typed"

-- Check individual components  
checkGlobal ctx globalVar
checkConst ctx constantDef
@
-}

module Analyser.Program where
  
import Grammar.Expr
import Grammar.Program

import Analyser.Error

import Analyser.TypeDef
import Analyser.Context.Def
import Analyser.Context.Expr
import Analyser.Context.Utils
import Analyser.Fun.FunDef

import Control.Monad

import qualified Data.List as L

-- | Main program analysis function.
-- 
-- Performs complete type checking and validation of a Moo program:
--
-- 1. Checks for required 'main' function
-- 2. Validates all type definitions
-- 3. Creates analysis context
-- 4. Type checks global variables and constants  
-- 5. Type checks all function definitions
--
-- Returns 'Left' 'Error' on any analysis failure, 'Right' '()' on success.
--
-- @
-- checkProgram (Program [mainFun] [] [] []) = Right ()
-- checkProgram (Program [] [] [] []) = Left (FunNotFound \"main\")
-- @
  -- = L.find (fName ||> (==name)) getFunDefs
checkProgram :: Program -> Either Error ()
checkProgram Program{..} = do

  -- 0: is the main here?
  unless (L.any (("main"==) . fName) pFuns) 
    $ Left (FunNotFound "main")

  -- 1: types are ok assuming that the others are also ok. (so ⊥s're allowed..)
  forM_ pTypes (`checkTypeDef` pTypes)

  -- 2: create the Context
  let ctx = Ctx pGlobals pConsts pFuns pTypes

  -- 3: check globals and consts definitions
  forM_ pGlobals (ctx `checkGlobal`)
  forM_ pConsts  (ctx `checkConst`)

  -- 4: if everything is ok, then lets check all the funs!
  -- (also: thats alright if someones check the main [u can change it latter in the parsing process!!])
  forM_ pFuns (ctx `checkFun`)


-- | Check a global variable definition.
--
-- Validates that:
-- * The expression type matches the declared type
-- * Both expression and type are well-formed
--
-- @
-- checkGlobal ctx (Global \"x\" TInt (ELit (LInt 42)))  -- Valid (global x: Int := 42)
-- checkGlobal ctx (Global \"y\" TInt (ELit (LString \"hello\")))  -- Type mismatch
-- @
-- TODO: separate them
checkGlobal :: Context -> GlobalDef -> Either Error ()
ctx `checkGlobal` Global{..} = do
  exprType <- ctx `checkExpr` ELit gExpr
  ctx `checkType` gType 
  unless (gType == exprType) $ Left $ TypeMismatch gType exprType

-- | Check a constant definition.
--
-- Validates that:
-- * The value is a literal expression (compile-time constant)
-- * The expression type matches the declared type
--
-- @
-- checkConst ctx (Const \"PI\" TReal (ELit (LReal 3.14)))  -- Valid literal (const PI := 3.14)
-- checkConst ctx (Const \"X\" TInt (EVar \"someVar\"))     -- Invalid: not literal
-- @
-- only literal constants?
checkConst :: Context -> ConstDef -> Either Error ()
ctx `checkConst` Const{..} = do
    kType' <- ctx `checkExpr` ELit kVal
    unless (kType' == kType) (Left $ TypeMismatch kType kType')


  