module Interpreter.Expr where

import Grammar.Expr
import Interpreter.InterpretT
import Interpreter.Context

import Control.Lens hiding (Context, op)

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import Data.List ((!?), find)
import Utils

-- assume everything is well typed!
evalExpr :: Expr -> InterpretT Value
evalExpr (ELit lit) = 
  case lit of 
    LInt a -> pure $ VInt a
    LChar c -> pure $ VChar c
    LFloat x -> pure $ VFloat x
    LConstr name lits ->
      VData name <$>
        forM (fmap ELit lits) evalExpr 

evalExpr (EConstr name args) = 
  VData name <$> forM args evalExpr

evalExpr (EVar name) = do
  ctx <- get
  liftEither $ runExcept (ctx `findVarVal` name)

evalExpr (EGlobal name) = do
  ctx <- get
  case ctx `findGlobal` name of
    Nothing -> undefined
    Just val -> pure val

evalExpr (EUnOp op ex) =
  case op of 
    Neg -> do
      VInt x <- evalExpr ex -- what happens if it couldnt match with VInt?
      pure $ VInt (-x)
    Not -> do
      bool <- evalExpr ex
      case bool of
        VTrue -> pure VFalse
        VFalse -> pure VTrue
        _ -> undefined

evalExpr (EBinOp exL op exR)
  | op `elem` intBinOps = do
    VInt nL <- evalExpr exL
    VInt nR <- evalExpr exR
    pure $ VInt $ case op of
      Add -> nL + nR
      Sub -> nL - nR
      Mul -> nL * nR
      Div -> nL `quot` nR
      Rem -> nL `mod` nR
      _ -> undefined

  | op `elem` floatBinOps = do
    VFloat xL <- evalExpr exL
    VFloat xR <- evalExpr exR
    pure $ VFloat $ case op of
      Add_ -> xL + xR
      Sub_ -> xL - xR
      Mul_ -> xL * xR
      Div_ -> xL / xR
      _ -> undefined

  | op `elem` boolBinOps = do
    bL <- evalExpr exL
    bR <- evalExpr exR
    pure $ case (bL, op, bR) of
      (VTrue, And, VTrue)  -> VTrue
      (VFalse, Or, VFalse) -> VFalse
      (_, And, _)          -> VFalse
      (_, Or, _)           -> VTrue
      _ -> undefined

  | op `elem` listBinOps = do
    ls <- evalExpr exL
    rs <- evalExpr exR
    pure (mooCat ls rs)

  | op `elem` badCompareBinOps = undefined
  | op `elem` bEqOps = undefined

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


findGlobal :: Context -> Name -> Maybe Value
ctx `findGlobal` name =
  ctx ^. cGlobs  
  & find ((name==) . (^. vName))
  & fmap (^. vVal)
