-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : Analyser.Fun.Expr
Description : Expression type checking within function contexts for Moo language
Copyright   : (c) 2025 Moo Language Team
License     : GPL-3
Maintainer  : joaoduos@gmail.com
Stability   : experimental
Portability : POSIX

This module implements expression type checking within function bodies.

= Function Expression Analysis

Extends global expression checking with local variable support:

* Local variables ('EVar') are resolved through the scope stack
* All other expressions use similar logic to global context
* Type unification for generic constructor applications
* Proper error reporting with variable scope information

= Key Differences from Global Context

Unlike "Analyser.Context.Expr", this module:

* Supports local variable lookup through 'FunContext'
* Uses scope stack for variable resolution
* Handles function parameter access
* Provides detailed error messages for scope issues

= Variable Resolution Order

1. Search local scope stack (innermost to outermost)
2. Search function parameters  
3. Search global constants and variables
4. Report 'VarNotFound' error

= Example Usage

@
-- Check expression in function context
result = funCtx \`checkExpr\` (EVar \"localVar\")

-- Type check with local variables
result2 = funCtx \`checkExpr\` (EBinOp (EVar \"x\") Add (ELit (LInt 1)))

-- Constructor with local variables
result3 = funCtx \`checkExpr\` (EConstr \"Some\" [EVar \"value\"])
@
-}

module Analyser.Fun.Expr where

-- import Check.FunDef.Context

import Analyser.Fun.FunContext.Def
import Analyser.Fun.FunContext.Utils

import Grammar.Program
import Grammar.Expr
import Grammar.Type


import Analyser.Error
import Utils

-- import Grammar.Utils

import Control.Monad (forM, forM_)

-- | Type check an expression within function context.
--
-- Similar to global expression checking but with local variable support:
--
-- [@ELit@] Literal values with constructor type unification
-- [@EConstr@] Constructor applications with type inference  
-- [@EVar@] Local variables resolved through scope stack
-- [@EConst@] Global constants from context
-- [@EGlobal@] Global variables from context
-- [@EUnOp@] Unary operations with type validation
-- [@EBinOp@] Binary operations with type compatibility  
-- [@EFunCall@] Function calls with arity and type checking
--
-- The key difference is 'EVar' support - variables are looked up in:
-- 1. Local scope stack (innermost to outermost)
-- 2. Function parameters
-- 3. Global constants and variables  
--
-- @
-- checkExpr funCtx (EVar \"localVar\")  -- Look up in local scopes
-- checkExpr funCtx (ELit (LInt 42))     -- Same as global context
-- @
-- be right back soon
checkExpr :: FunContext -> Expr -> Either Error Type
ctx `checkExpr` (ELit lit) = 
  case lit of
    LInt   _ -> pure TInt
    LChar  _ -> pure TChar
    LFloat _ -> pure TFloat
    LConstr name lits -> 
      case ctx `findConstrAndTypeDefsByName` name of
        Nothing -> Left Error
        Just (ConstrDef{cParams}, TypeDef{tName, tParams})
          | length cParams /= length lits -> Left Error
          | otherwise -> do
              tLits <- forM lits $ \lit' -> do
                ctx `checkExpr` ELit lit'

              -- TODO: Lifing
              case unifyTParams (TVar <$> tParams) tLits cParams of
                Nothing -> Left Error
                Just ts -> pure $ TData tName ts
            

ctx `checkExpr` (EConstr name args) = 
  case ctx `findConstrAndTypeDefsByName` name of
    Nothing -> Left Error
    Just (ConstrDef{cParams}, TypeDef{tName, tParams}) 
      | length cParams /= length args -> Left Error
      | otherwise -> do
          tLits <- forM args $ \arg -> do
            ctx `checkExpr` arg

          -- TODO: Lifing
          case unifyTParams (TVar <$> tParams) tLits cParams of
            Nothing -> Left Error
            Just ts -> pure $ TData tName ts

-- | Variable lookup in function context (key difference from global context).
--
-- Resolves variables using lexical scoping through the scope stack:
-- 1. Search local scopes (innermost to outermost)  
-- 2. Search function parameters
-- 3. If not found, return 'VarNotFound' error
--
-- This enables proper local variable access within function bodies.
ctx `checkExpr` (EVar name) =
  case ctx `findDecl` name of 
    Nothing -> Left $ VarNotFound name
    Just (Decl{dclType}) -> pure dclType

-- | Global constant lookup (delegates to global context).
-- pre-process :PP TODO: -- maybe the parser?? no
ctx `checkExpr` (EConst name) = 
  case ctx `findConstDef` name of 
    Nothing -> Left Error
    Just (Const{kType}) ->       
      pure kType

-- | Global variable lookup (delegates to global context).
ctx `checkExpr` (EGlobal name) = 
  case ctx `findGlobalDef` name of 
    Nothing -> Left Error
    Just (Global{gType}) ->       
      pure gType

checkExpr ctx (EUnOp op expr) = do
  t <- ctx `checkExpr` expr
  case (op, t) of 
    (Neg, TInt)  -> pure TInt
    (Not, TBool) -> pure TBool
    _ -> Left Error -- TODO: Msg

-- improve typeErrors
checkExpr ctx (EBinOp lExpr op rExpr) = do
  tL <- ctx `checkExpr` lExpr
  tR <- ctx `checkExpr` rExpr

  if op `elem` bEqOps && tL == tR then
    pure TBool

  else case (tL, tR) of
    (TInt, TInt)
      | op `elem` intBinOps -> pure TInt
      | op `elem` badCompareBinOps -> pure TBool
    (TFloat, TFloat)
      | op `elem` floatBinOps -> pure TFloat
      | op `elem` badCompareBinOps -> pure TBool
    (TBool, TBool)
      | op `elem` boolBinOps -> pure TBool
    (TList a, TList b)
      | a == b && op `elem` listBinOps -> pure (TList a)
    _ -> Left Error
      
checkExpr ctx (EFunCall fName args) = 
  case ctx `findFunDef` fName of
    Nothing -> Left $ FunNotFound fName
    Just f  -> do
      forM_ args (ctx `checkExpr`)
      handleFun ctx f args

---- Handlers ----

handleFun :: FunContext -> FunDef -> [Expr] -> Either Error Type
handleFun ctx FunDef{fName, fParams, rtrType} args 
  | length fParams == length args = do
      ts <- forM args (ctx `checkExpr`) 
      if ts == (pType <$> fParams) then
        pure rtrType 
      else
        Left Error -- TODO: Improve this Error!

  | otherwise = Left $
     IncorrectArity fName 
       (length fParams) (length args)
      
    -- | zipWith (==) (paramType fParams) 
-- tparams (fake ForAll)          appTs                                    cParams 
-- [TVar a, TVar b] [BinTree(a, Float), Float, BinTree(a, Float)] [BinTree(a, b), a, BinTree(a, b)] --> [a, Float]

unifyTParams :: [Type] -> [Type] -> [Type] -> Maybe [Type]
unifyTParams tParams [] [] = Just tParams
unifyTParams tParams (appT:appTs) (cParam:cParams) =
  case (appT, cParam) of
    (TVar x, TVar x')
      | x /= x' -> error "The program has reach to a impossible state 🤡"
      | otherwise -> unifyTParams tParams appTs cParams

    (someType, TVar x) -> 
      unifyTParams (tParams `replacing` (x, someType)) appTs cParams

    (TData name ts, TData name' ts') 
      | name /= name' -> Nothing
      | length ts /= length ts' -> Nothing
      | otherwise -> do
          tParamsAfterArgs <- unifyTParams tParams ts ts'
          unifyTParams tParamsAfterArgs appTs cParams

    _ -> Nothing

    where 
        replacing :: [Type] -> (Name, Type) -> [Type]
        [] `replacing` _ = []
        (TVar x':ts) `replacing` m@(x, t) 
          | x == x'   = t:(ts `replacing` m)
          | otherwise = TVar x': (ts `replacing` m)
        (TData tName tArgs:ts) `replacing` m
          = TData tName (tArgs `replacing` m) : ts `replacing` m
        (t:ts) `replacing` m
          = t:(ts `replacing` m)
unifyTParams _ _ _ = Nothing
