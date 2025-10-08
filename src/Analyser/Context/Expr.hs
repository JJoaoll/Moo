{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Analyser.Context.Expr
Description : Expression type checking and inference for Moo language
Copyright   : (c) 2025 Moo Language Team
License     : MIT
Maintainer  : joaoduos@gmail.com
Stability   : experimental
Portability : POSIX

This module implements expression type checking for the Moo language.

= Expression Type Checking

Validates that expressions are well-typed according to Moo's type system:

* Literal values have their corresponding types
* Variables reference valid declarations  
* Function calls have correct signatures
* Operators are applied to compatible types
* Constructor applications match type definitions

= Type Inference

Performs type inference for:

* Generic constructor applications with type unification
* Binary operations with type compatibility checking
* Function calls with parameter type matching

= Error Handling

Returns detailed errors for common type violations:

* 'VarNotFound': Variable not in scope (should not occur in Context expressions)
* 'FunNotFound': Function not defined
* 'IncorrectArity': Wrong number of function arguments
* Generic 'Error': Type mismatches and other violations

= Example Usage

@
-- Check expression type
result = ctx \`checkExpr\` (EBinOp (ELit (LInt 1)) Add (ELit (LInt 2)))
-- Returns: Right TInt

-- Invalid expression
result2 = ctx \`checkExpr\` (EBinOp (ELit (LInt 1)) Add (ELit (LBool True)))  
-- Returns: Left Error
@
-}

module Analyser.Context.Expr where

import Analyser.Context.Def
import Analyser.Context.Utils
import Analyser.Error

import Grammar.Expr
import Grammar.Program
import Grammar.Type

import Control.Monad

import Utils

-- | Type check an expression in the given context.
--
-- Validates that the expression is well-typed and returns its inferred type:
--
-- [@ELit@] Literal values: integers, characters, floats, constructed literals
-- [@EConstr@] Constructor applications with type unification  
-- [@EVar@] Variables (not allowed in global context - returns error)
-- [@EConst@] Named constants with their declared types
-- [@EGlobal@] Global variables with their declared types
-- [@EUnOp@] Unary operations: negation, boolean not
-- [@EBinOp@] Binary operations: arithmetic, comparison, logical
-- [@EFunCall@] Function calls with arity and type checking
--
-- @
-- checkExpr ctx (ELit (LInt 42)) = Right TInt
-- checkExpr ctx (EBinOp (ELit (LInt 1)) Add (ELit (LInt 2))) = Right TInt
-- checkExpr ctx (EFunCall \"factorial\" [ELit (LInt 5)]) = Right TInt
-- @
-- be right back soon
checkExpr :: Context -> Expr -> Either Error Type
ctx `checkExpr` (ELit lit) = 
  case lit of
    LInt   _ -> pure TInt
    LChar  _ -> pure TChar
    LFloat _ -> pure TFloat
    LConstr name lits -> 
      case ctx `findConstrAndTypeDefsByName` name of
        Nothing -> Left $ ConstrNotFound name
        Just (ConstrDef{cParams}, TypeDef{tName, tParams})
          | length cParams /= length lits -> 
              Left $ IncorrectArity name (length cParams) (length lits)
          | otherwise -> do
              tLits <- forM lits $ \lit' -> do
                ctx `checkExpr` ELit lit'

              -- TODO: Lifing
              case unifyTParams (TVar <$> tParams) tLits cParams of
                Nothing -> Left TypeUnificationFailed
                Just ts -> pure $ TData tName ts
            

ctx `checkExpr` (EConstr name args) = 
  case ctx `findConstrAndTypeDefsByName` name of
    Nothing -> Left $ ConstrNotFound name
    Just (ConstrDef{cParams}, TypeDef{tName, tParams}) 
      | length cParams /= length args -> 
          Left $ IncorrectArity name (length cParams) (length args)
      | otherwise -> do
          tLits <- forM args $ \arg -> do
            ctx `checkExpr` arg

          -- TODO: Lifing
          case unifyTParams (TVar <$> tParams) tLits cParams of
            Nothing -> Left TypeUnificationFailed
            Just ts -> pure $ TData tName ts

-- | Variables are not allowed in global context expressions.
-- This should never occur in properly parsed programs.
-- NO EVARS HERE! NO NEED TO CHECK!
_ `checkExpr` (EVar name) = Left $ VarNotFound name

ctx `checkExpr` (EConst name) = 
  case ctx `findConstDef` name of 
    Nothing -> Left $ ConstNotFound name
    Just (Const{kType}) ->       
      pure kType

ctx `checkExpr` (EGlobal name) = 
  case ctx `findGlobalDef` name of 
    Nothing -> Left $ GlobalNotFound name
    Just (Global{gType}) ->       
      pure gType

checkExpr ctx (EUnOp op expr) = do
  t <- ctx `checkExpr` expr
  case (op, t) of 
    (Neg, TInt)  -> pure TInt
    (Not, TBool) -> pure TBool
    (Neg, actualType) -> Left $ TypeMismatch TInt actualType
    (Not, actualType) -> Left $ TypeMismatch TBool actualType

-- | Check binary operation type compatibility.
-- 
-- Validates operator application and returns result type:
-- * Equality operators work on any matching types
-- * Arithmetic operators require matching numeric types  
-- * Comparison operators work on comparable types
-- * Boolean operators require boolean operands
-- * List operators require matching list element types
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
    _ -> Left $ TypeMismatch tL tR
      
checkExpr ctx (EFunCall fName args) = 
  case ctx `findFunDef` fName of
    Nothing -> Left $ FunNotFound fName
    Just f  -> do
      forM_ args (ctx `checkExpr`)
      handleFun ctx f args

checkExpr _ (EScan _) = 
  Left $ VarNotFound "scan is not allowed in global context"


---- Handlers ----

-- | Handle function call type checking.
--
-- Validates:
-- * Correct number of arguments (arity check)
-- * Argument types match parameter types exactly
-- * Returns the function's declared return type
--
-- @
-- handleFun ctx factorial [ELit (LInt 5)]  -- Check factorial(5)
-- @

---- Handlers ----

handleFun :: Context -> FunDef -> [Expr] -> Either Error Type
handleFun ctx FunDef{fName, fParams, rtrType} args 
  | length fParams /= length args = Left $ 
     IncorrectArity fName 
       (length fParams) (length args)

  | otherwise = do
      ts <- forM args (ctx `checkExpr`) 
      let expectedTypes = pType <$> fParams
      if ts == expectedTypes then
        pure rtrType 
      else
        Left $ TypeMismatch (TData "FunctionParams" expectedTypes) (TData "GivenArgs" ts)

      
    -- | zipWith (==) (paramType fParams) 

-- | Unify type parameters with concrete types during constructor application.
--
-- Performs type unification for generic constructor applications:
--
-- [@tParams@] Template type variables from the type definition  
-- [@appTs@] Concrete types from the constructor application
-- [@cParams@] Constructor parameter types from the type definition
--
-- Returns unified type parameters or 'Nothing' on unification failure.
--
-- = Example Unification
--
-- @
-- -- Type: data BinTree a b = Node a (BinTree a b) (BinTree a b) | Leaf b
-- -- Constructor: Node (value :: Int) (left :: BinTree Int Float) (right :: BinTree Int Float)
-- -- unifyTParams [TVar \"a\", TVar \"b\"] [TInt, TData \"BinTree\" [TInt, TFloat], TData \"BinTree\" [TInt, TFloat]] 
-- --              [TVar \"a\", TData \"BinTree\" [TVar \"a\", TVar \"b\"], TData \"BinTree\" [TVar \"a\", TVar \"b\"]]
-- -- Result: Just [TInt, TFloat]
-- @
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
