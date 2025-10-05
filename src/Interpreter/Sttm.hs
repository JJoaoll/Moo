module Interpreter.Sttm where

import Grammar.Sttm
import Interpreter.InterpretT
import Interpreter.Context
import Interpreter.Expr

import Control.Monad.State
import Control.Monad.Except
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

-- Option! Literal? -- I NEED a parser here
evalSttm (SScan typε) = undefined

evalSttm (SFunCall name args) = undefined
evalSttm (SMatch strutinee cases) = undefined

evalSttm (SWhile cond body) = do
  whileLoop
    where 
      whileLoop = do
        bool <- evalExpr cond 
        case bool of
          VFalse -> pure Nothing
          VTrue -> do 
            enterBlock
            result <- execBody body
            quitBlock
            case result of
              Nothing -> whileLoop  -- loop
              Just val -> pure (Just val)  -- break the loop
          _ -> throwError IError2
    
      execBody [] = pure Nothing
      execBody (stmt:stmts) = do
        mVal <- evalSttm stmt
        case mVal of
          Nothing -> execBody stmts   -- keep the loop
          Just val -> pure (Just val) -- the loop and returns

evalSttm (SFor i is body) = undefined

evalSttm (SReturn expr) = do
  val <- evalExpr expr
  pure $ Just val

-- evalSttm _ = undefined