{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Interprer.Program where
import Interpreter.InterpretT (InterpretT, InterpretError)
import Grammar.Program
import Interpreter.Context
import Control.Monad.State
import Interpreter.Core (evalFun)

-- AI Generated
-- initContext :: Program -> Context
-- initContext Program{..} = Context
--   { _cTypes = pTypes    -- tipos definidos no programa
--   , _cFuncs = pFuns     -- funções definidas no programa
--   , _cGlobs = pGlobals' -- globais começam vazias 
--   , _cStack = []        -- stack começa vazia
--   , _cScope = -1        -- começa no escopo -1 (global)
--   }

--   where 
--     pGlobals' = globalToDVar <$> pGlobals
--     globalToDVar (Global name _ lit) = DVar name (lit2Val lit)

-- runInterpretT :: InterpretT a -> Context -> IO (Either InterpretError (a, Context))
-- runInterpretT action ctx = runExceptT $ runStateT action ctx

-- evalInterpretT :: InterpretT a -> Context -> IO (Either InterpretError a)
-- evalInterpretT action ctx = runExceptT $ evalStateT action ctx

-- execInterpretT :: InterpretT a -> Context -> IO (Either InterpretError Context)
-- execInterpretT action ctx = runExceptT $ execStateT action ctx

evalProgram :: InterpretT Value
evalProgram = do
  -- put $ initContext program

  -- analysing??

  -- preprocessing??

  -- running!
  ctx <- get
  case ctx `findFunDef` "main" of
    Nothing -> error "where's main?"
    Just main -> evalFun [] main
  


  



