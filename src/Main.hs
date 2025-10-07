{-# LANGUAGE RecordWildCards #-}
module Main where

import Grammar.Program
import Interpreter.Program 
import Interpreter.Context
import Interpreter.InterpretT
import Control.Monad.State
import Control.Monad.Except
import Grammar.Type 

-- AI GENERATED Example!

nativeTypes :: [TypeDef]
nativeTypes = 
  [ TDefEmpty
  , TDefOne
  , TDefBool
  , TDefOption
  , TDefTuple
  , TDefList ]

initContext :: Program -> Context
initContext Program{..} = Context
  { _cTypes = nativeTypes ++ pTypes 
  , _cFuncs = pFuns
  , _cGlobs = globalToDVar <$> pGlobals
  , _cStack = [[]]  -- fix these
  , _cScope = 0     -- weird bugs
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
