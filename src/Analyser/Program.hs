{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Analyser.Program where
  
import Grammar.Program

import Analyser.Expr
import Analyser.FunDef
import Analyser.TypeDef

import Analyser.Error

import Control.Monad

import qualified Data.List as L
import Utils

  -- = L.find (fName ||> (==name)) getFunDefs
checkProgram :: Program -> Either Error ()
checkProgram Program{..} = do

  -- 0: is the main here?
  unless (L.any (fName ||> (=="main")) pFuns) 
    $ Left (FunNotFound "main")

  -- 1: types are ok assuming that the others are also ok. (so ⊥s're allowed..)
  forM_ pTypes (`checkTypeDef` pTypes)

  -- 2: create the Context
  let ctx = Ctx pGlobals pConsts pFuns pTypes

  -- 3: check globals and consts definitions
  forM_ pGlobals (ctx `checkGlobal`)
  forM_ pConsts  (ctx `checkConst`)

  -- 4: if everything is ok, then lets check all the funs!
  -- (also: thats alright if someones check the main [u can change it latter in the parsing process!!])
  forM_ pFuns (ctx `checkFun`)


-- TODO: separate them
checkGlobal :: Context -> GlobalDef -> Either Error ()
ctx `checkGlobal` Global{..} = do
  exprType <- ctx `checkExpr` gExpr 
  void $ ctx `checkType` gType 
  if gType == exprType then
    pure()
  else
    Left Error

checkConst :: Context -> ConstDef -> Either Error ()
ctx `checkConst` k = undefined