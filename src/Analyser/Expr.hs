-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}

module Analyser.Expr where

-- import Check.FunDef.Context

import Analyser.FunContext

import Grammar.Program
import Grammar.Expr
import Grammar.Sttm
import Grammar.Type

import qualified Data.List as L

import Analyser.Error
import Utils

-- import Grammar.Utils

import Control.Monad (forM, forM_, void, when)

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

ctx `checkExpr` (EVar name) =
  case ctx `findDecl` name of 
    Nothing -> Left $ VarNotFound name
    Just (Decl{dclType}) -> pure dclType

-- pre-process :PP TODO: -- maybe the parser?? no
ctx `checkExpr` (EConst name) = 
  case ctx `findConstDef` name of 
    Nothing -> Left Error
    Just (Const{kType}) ->       
      pure kType

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
    Just f  -> handleFun ctx f args

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
-- [Var a, Var b] [BinTree(a, Float), Float, BinTree(a, Float)] [BinTree(a, b), a, BinTree(a, b)] --> [a, Float]

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
