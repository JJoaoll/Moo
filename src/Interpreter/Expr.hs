module Interpreter.Expr where

import Grammar.Expr
import Interpreter.InterpretT
import Interpreter.Context

import Control.Lens hiding (Context)

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import Data.List ((!?), find)
import Utils

evalExpr :: Expr -> InterpretT Value
evalExpr (ELit lit) = 
  case lit of 
    LInt a -> pure $ VInt a
    LChar c -> pure $ VChar c
    LFloat x -> pure $ VFloat x
    LConstr name lits ->
      VData name <$>
        forM (fmap ELit lits) evalExpr 

evalExpr (EConstr name args) = undefined

evalExpr (EVar name) = do
  ctx <- get
  liftEither $ runExcept (ctx `findVarVal` name)

evalExpr (EConst name) = undefined
evalExpr (EGlobal name) = undefined
evalExpr (EUnOp op ex) = undefined
evalExpr (EBinOp exL op exR) = undefined
evalExpr (EFunCall fun arg) = undefined

evalExpr _ = undefined

findVarVal :: Context -> Name -> Except InterpretError Value
ctx `findVarVal` name = 
  case (ctx ^. cStack) !? (ctx ^. cScope) of
    Nothing -> undefined
    Just localStack -> 
      case find ((name==) . (^. vName)) localStack of
        Just dcl -> pure $ dcl ^. vVal
        Nothing 
          | ctx ^. cScope == 0 -> undefined 
          | otherwise -> 
              (ctx & cScope %~ subtract 1) `findVarVal` name