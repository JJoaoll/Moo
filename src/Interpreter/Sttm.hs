module Interpreter.Sttm where

import Grammar.Sttm
import Interpreter.InterpretT
import Interpreter.Context
import Interpreter.Expr

import Control.Monad.State
import Control.Monad.Except
import qualified System.IO as IO

import Utils
import Data.Text hiding (length)
import Grammar.Expr (Lit)
import Text.Megaparsec (runParser)
import Parser.Expr (literal)

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
--   ctx <- get
--   tDef <- ctx `findTypeDef` typε 
--   input <- liftIO IO.getLine

  
  
--   where
--   parseLiteral :: Text -> Maybe Lit
--   parseLiteral input =
--    case runParser literal "" input of
--     Right lit -> Just lit
--     Left _ -> Nothing



evalSttm (SFunCall name args) = undefined

evalSttm (SMatch scrutinee cases) = do
  val <- evalExpr scrutinee
  val `execFirstMatchIn` cases

  where 
    _ `execFirstMatchIn` [] = pure Nothing
    val `execFirstMatchIn` ((pattern,body):anotherCases) = do
      enterBlock
      didMatch <- bindPatterns [val] [pattern]
      if not didMatch then do
        quitBlock
        val `execFirstMatchIn` anotherCases
      else do
        result <- execBody body
        quitBlock
        pure result
    
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


execBody :: [Sttm] -> InterpretT (Maybe Value)
execBody [] = pure Nothing
execBody (stmt:stmts) = do
  mVal <- evalSttm stmt
  case mVal of
    Nothing -> execBody stmts   -- keep the loop
    Just val -> pure (Just val) -- breaks the loop and returns

-- also check?? 
-- ensure that they have the same size!
bindPatterns :: [Value] -> [Pattern] -> InterpretT Bool
bindPatterns [] [] = pure True
bindPatterns (v:vs) (p:ps) = 
  case p of
    PConstructor cName cPatterns -> 
      case v of
        VData vName vArgs 
          | length vArgs /= length cPatterns -> pure False
          | vName /= cName -> pure False
          | otherwise -> do
            innerOk <- bindPatterns vArgs cPatterns
            if innerOk then bindPatterns vs ps else pure False
        _ -> pure False

    PVar name -> do
      addDecl (name, v)
      bindPatterns vs ps
    
    _ -> bindPatterns vs ps

bindPatterns _ _ = pure False

