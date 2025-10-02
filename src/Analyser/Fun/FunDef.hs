{-# LANGUAGE RecordWildCards #-}

module Analyser.Fun.FunDef where

import Analyser.Context.Def
import Analyser.Error

import Grammar.Program

import Analyser.Fun.Sttm
import Analyser.Fun.FunContext

import Data.List.NonEmpty
import Control.Monad


checkFun :: Context -> FunDef -> Either Error ()
checkFun Ctx{..} FunDef{..} = 
  let 
    initialStack = (paramToDecl <$> fParams) :| []
    funCtx = 
      FunCtx {
        ctxDecls    = initialStack,
        ctxGlobals  = getGlobals,
        ctxConsts   = getConsts,
        ctxTypeDefs = getTypeDefs,
        ctxFunDefs  = getFunDefs,
        ctxLevel    = 0,
        ctxRtrnType = rtrType
      }
  in 
    -- if returns, returs the right type
    -- but no garantee that will return something
    -- without more time to code..
    foldM_ checkSttm funCtx body



