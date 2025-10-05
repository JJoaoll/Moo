module Interpreter.Sttm where

import Grammar.Sttm
import Interpreter.InterpretT
import Interpreter.Context
import Interpreter.Expr

import Control.Monad.State
import Control.Monad
import qualified System.IO as IO

-- i'll consider that returns provides values
evalSttm :: Sttm -> InterpretT (Maybe Value)
evalSttm (SInit name typε expr) = do
  val <- evalExpr expr

  ctx <- get
  case ctx `withDVar` (name, typε, val) of
    Left msg -> error $ show msg -- TODO: treat this
    Right ctx' -> do
      put ctx'
      pure Nothing

-- does the attrib to the closest ref 
evalSttm (SAtrib name expr) = do
  val <- evalExpr expr

  ctx <- get
  case ctx `modifyDVarVal` (name, val) of
    Left msg -> error $ show msg -- TODO: treat this
    Right ctx' -> do
      put ctx'
      pure Nothing

evalSttm (SPrint expr) = do
  val <- evalExpr expr
  liftIO $ IO.print val
  pure Nothing

-- Option! Literal?
evalSttm (SScan typε) = undefined

evalSttm (SFunCall name args) = undefined
evalSttm (SMatch strutinee cases) = undefined
evalSttm (SWhile cond body) = undefined
evalSttm (SFor i is body) = undefined

evalSttm (SReturn expr) = do
  val <- evalExpr expr
  pure $ Just val

-- evalSttm _ = undefined