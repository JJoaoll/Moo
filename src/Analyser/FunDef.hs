{-# LANGUAGE RecordWildCards #-}

module Analyser.FunDef where

import Analyser.Error

import Grammar.Program
import Grammar.Type

import Analyser.Sttm
import Analyser.FunContext

import Data.List.NonEmpty
import Control.Monad

-- the existence of this implies that u should refactor it
data Context = Ctx
  { getGlobals  :: [GlobalDef] -- could be just the bottom of the scopes
  , getConsts   :: [ConstDef]
  , getFunDefs  :: [FunDef]
  , getTypeDefs :: [TypeDef]
  } deriving (Eq, Show)

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

paramToDecl :: Param -> Decl
paramToDecl Param{..} =
  Decl pName pType



