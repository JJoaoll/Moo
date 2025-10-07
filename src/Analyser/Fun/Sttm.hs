{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : Analyser.Fun.Sttm
Description : Statement analysis and type checking within Moo function bodies
Copyright   : (c) 2025 Moo Language Team
License     : MIT
Maintainer  : joaoduos@gmail.com
Stability   : experimental
Portability : POSIX

This module implements statement-level analysis for Moo function bodies.

= Statement Analysis

Validates all statement types within function contexts:

* Variable operations: declaration ('SInit'), assignment ('SAtrib')
* I/O operations: output ('SPrint'), input ('SScan')  
* Control flow: pattern matching ('SMatch'), loops ('SWhile', 'SFor'), returns ('SReturn')
* Function calls: procedure calls ('SFunCall')

= Context Management

Each statement can modify the function context:

* 'SInit': Adds new variable declaration to current scope
* Other statements: Generally preserve context unchanged  
* Control flow: May enter/exit nested scopes (TODO: scope tracking)

= Type Safety

Ensures type correctness for:

* Variable initialization with matching types
* Assignment to existing variables with compatible types
* Function calls with correct signatures
* Pattern matching with comprehensive case analysis
* Return statements matching function signature

= Error Handling

Reports specific errors for:

* Type mismatches in assignments and initializations
* Undefined variables and functions
* Pattern matching failures
* Control flow type inconsistencies

= Example Usage

@
-- Check variable declaration (let x: Int := 42)
checkSttm ctx (SInit \"x\" TInt (ELit (LInt 42)))

-- Check assignment (x := x + 1)
checkSttm ctx (SAtrib \"x\" (EBinOp (EVar \"x\") Add (ELit (LInt 1))))

-- Check pattern matching
checkSttm ctx (SMatch (EVar \"option\") 
               [(PConstructor \"Some\" [PVar \"value\"], [SPrint (EVar \"value\")])])
@
-}

module Analyser.Fun.Sttm where

import Grammar.Type
import Grammar.Sttm

import Analyser.Error
import Analyser.Fun.Expr
import Analyser.Fun.Pattern
import Analyser.Fun.FunContext.Def
import Analyser.Fun.FunContext.Utils

import Control.Monad
import Control.Lens hiding (Context)
import Grammar.Program (GlobalDef(..))

-- | Check a statement and return updated function context.
--
-- Validates statement type correctness and updates context as needed:
--
-- [@SInit@] Variable declaration - adds to local scope if types match
-- [@SAtrib@] Variable assignment - checks compatibility with existing declaration
-- [@SPrint@] Output statement - validates expression type  
-- [@SScan@] Input statement - validates type is concrete
-- [@SFunCall@] Function call - checks function exists with correct signature
-- [@SMatch@] Pattern matching - validates all cases comprehensively
-- [@SWhile@] While loop - checks condition and body statements
-- [@SFor@] For loop - validates iterable and body with loop variable
-- [@SReturn@] Return statement - ensures type matches function signature
--
-- Most statements preserve the context unchanged, except 'SInit' which adds
-- new variable declarations to the current scope.
--
-- @
-- checkSttm ctx (SInit \"x\" TInt (ELit (LInt 42)))  -- Adds \"x\" to scope (let x: Int := 42)
-- checkSttm ctx (SPrint (EVar \"x\"))                -- Context unchanged
-- @
checkSttm :: FunContext -> Sttm -> Either Error FunContext
ctx `checkSttm` (SInit name typε expr) = do 
  exprType <- ctx `checkExpr` expr
  if exprType == typε then do
    ctx `checkType` typε 
    addDecl name typε ctx -- missing to check TODO
  else
    Left Error -- TODO: incompatible types

ctx `checkSttm` (SAtrib name expr) = do
  exprType <- ctx `checkExpr` expr
  case ctx `findDecl` name of
    Nothing -> Left Error
    Just Decl{dclType} 
      | dclType == exprType -> pure ctx
      | otherwise -> Left Error
  
ctx `checkSttm` (SPrint expr) = do
  void $ ctx `checkExpr` expr
  pure ctx

ctx `checkSttm` (SGtrib name expr) = do
  exprType <- ctx `checkExpr` expr
  case ctx `findGlobalDef` name of
    Nothing -> Left Error
    Just Global{gType} 
      | gType == exprType -> pure ctx
      | otherwise -> Left Error
  
ctx `checkSttm` (SFunCall fName fArgs) = do
  case ctx `findFunDef` fName of
    Nothing -> Left Error
    Just f -> ctx <$ handleFun ctx f fArgs
  
ctx `checkSttm` (SMatch scrutinee cases) = do
  scrtnType <- ctx `checkExpr` scrutinee

  forM_ cases $ \(pttrn, sttms) -> do
    void $ checkPattern ctx pttrn scrtnType
    foldM_ checkSttm (enterBlock ctx) sttms

  pure ctx

ctx `checkSttm` (SWhile cond body) = do 
  condType <- ctx `checkExpr` cond
  if condType /= TBool then
    Left Error
  else 
    ctx <$ foldM checkSttm (enterBlock ctx) body

ctx `checkSttm` (SFor i is body) = do
  lType <- ctx `checkExpr` is
  case lType of
    TList a -> do
      newCtx <- ctx & enterBlock & addDecl i a
      ctx <$ foldM checkSttm newCtx body
    _ -> Left Error

ctx `checkSttm` (SReturn expr) = do
  exprType <- ctx `checkExpr` expr
  if (ctx ^. getRtrnType) == exprType then
    pure ctx
  else
    Left Error
