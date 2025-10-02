{-# LANGUAGE RecordWildCards #-}

module Analyser.Fun.FunDef where

import Analyser.Context.Def
import Analyser.Error

import Grammar.Program

import Analyser.Fun.Sttm
import Analyser.Fun.FunContext.Def

import Data.List.NonEmpty
import Control.Monad

import Analyser.Context.Utils


checkFun :: Context -> FunDef -> Either Error ()
checkFun ctx FunDef{..} = 
  let 
    initialStack = (paramToDecl <$> fParams) :| []
    funCtx = 
      FunCtx {
        _getStack    = initialStack,
        _getCtx      = ctx,
        _getLevel    = 0,
        _getRtrnType = rtrType
      }
  in 
    -- if returns, returs the right type
    -- but no garantee that will return something
    -- without more time to code..
    foldM_ checkSttm funCtx body



