-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Analyser.Expr where

-- import Check.FunDef.Context

import Analyser.FunContext

import Grammar.Program
import Grammar.Expr
import Grammar.Sttm
import Grammar.Type

import qualified Data.List as L
import Utils

-- import Grammar.Utils

import Control.Monad (forM, forM_, void)

checkExpr :: FunContext -> Expr -> Either Error Type
ctx `checkExpr` (ELit lit) = 
  case lit of
    LInt   _ -> pure TInt
    LChar  _ -> pure TChar
    LFloat _ -> pure TChar
    LConstr name lits -> undefined

ctx `checkExpr` (EConstr name args) = undefined

ctx `checkExpr` (EVar name) =
  case ctx `findDecl` name of 
    Nothing -> Left $ VarNotFound name
    Just (Decl{dclType}) -> pure dclType

-- pre-process :PP TODO:
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
    ((TList a), (TList b))
      | a == b && op `elem` listBinOps -> pure (TList a)
    _ -> Left Error
      
checkExpr ctx (EFunCall fName args) = 
  case ctx `findFunDef` fName of
    Nothing -> Left $ FunNotFound fName
    Just f  -> handleFun ctx f args
      

---- Handlers ----

handleFun :: FunContext -> FunDef -> [Expr] -> Either Error Type
handleFun = undefined
-- handleFun ctx FunDef{fName, fParams, rtrType} args 
--   | length fParams == length (args :: [Expr]) = do
--       ts <- forM args (ctx `checkExpr`) 
--       if ts == (paramType <$> fParams) then
--         pure rtrType 
--       else
--         Left Error -- TODO: Improve this Error!

--   | otherwise = Left $
--      IncorrectArity fName 
--        (length fParams) (length args)
      
--     -- | zipWith (==) (paramType fParams) 