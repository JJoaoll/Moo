{-# LANGUAGE RecordWildCards #-}
module Main where

import Grammar.Program
import Interpreter.Program 
import Interpreter.Context
import Interpreter.InterpretT
import Control.Monad.State
import Control.Monad.Except

-- AI GENERATED Example!

initContext :: Program -> Context
initContext Program{..} = Context
  { _cTypes = pTypes
  , _cFuncs = pFuns
  , _cGlobs = globalToDVar <$> pGlobals
  , _cStack = []
  , _cScope = -1
  }
  where 
    globalToDVar (Global name _ lit) = DVar name (lit2Val lit)

main :: IO ()
main = do
  print "hello world"
  -- program <- parseProgram "file.moo"  -- seu parser aqui
  -- let ctx = initContext program
  -- result <- runExceptT $ evalStateT evalProgram ctx
  -- case result of
  --   Left err -> putStrLn $ "Error: " ++ show err
    -- Right val -> putStrLn $ "Result: " ++ show val
