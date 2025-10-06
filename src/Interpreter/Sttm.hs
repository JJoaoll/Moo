module Interpreter.Sttm where

import Grammar.Sttm
import Interpreter.InterpretT
import Interpreter.Context
import Interpreter.Expr

import Control.Monad.State
import Control.Monad.Except
import Control.Monad
import qualified System.IO as IO

import Utils

-- i'll consider that returns provides values
evalSttm :: Sttm -> InterpretT (Maybe Value)
evalSttm (SInit name _typε expr) = do
  val <- evalExpr expr
  addDecl (name, val)
  pure Nothing

  -- ctx <- get
  -- case ctx `withDVar` (name, val) of
  --   Left msg -> error $ show msg -- TODO: treat this
  --   Right ctx' -> do
  --     put ctx'
  --     pure Nothing

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
          Just val -> pure (Just val) -- breaks the loop and returns

evalSttm (SFor i is body) = do

  list <- evalExpr is

  -- typε <- ctx `checkType` is
  forLoop $ listoquio list
    where 
      forLoop [] = pure Nothing
      forLoop (val:vals) = do 
        enterBlock
        addDecl (i, val)
        result <- execBody body
        quitBlock
        case result of
          Nothing -> forLoop vals           -- keeps the loop
          Just retVal -> pure $ Just retVal -- breaks the loop and returns

      execBody [] = pure Nothing
      execBody (stmt:stmts) = do
        mVal <- evalSttm stmt
        case mVal of
          Nothing -> execBody stmts   -- keep the loop
          Just val -> pure (Just val) -- breaks the loop and returns


evalSttm (SReturn expr) = do
  val <- evalExpr expr
  pure $ Just val

-- evalSttm _ = undefined


-- no cycling
addDecl :: (Name, Value) -> InterpretT ()
addDecl d@(_name, _val) = do
  ctx <- get
  case ctx `withDVar` d of 
    Right ctx' -> put ctx'
    Left msg -> error $ show msg -- TODO: do this msg
