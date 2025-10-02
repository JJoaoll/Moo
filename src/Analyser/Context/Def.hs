{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Analyser.Context.Def where

import Grammar.Program
import Grammar.Type

import Control.Lens hiding (element, Context)
import Control.Lens.TH

import Utils

-- the existence of this implies that u should refactor it
data Context = Ctx
  { getGlobals  :: [GlobalDef] -- could be just the bottom of the scopes
  , getConsts   :: [ConstDef]
  , getFunDefs  :: [FunDef]
  , getTypeDefs :: [TypeDef]
  } deriving (Eq, Show)

$(makeLenses ''Context)

-- Don't need values Here!
data Decl = Decl
  { dclName  :: Name
  , dclType  :: Type
  -- , dclLevel :: ScopeLevel --> im already handleing it in the matrix!
  } deriving (Eq, Show)

-- indicates theres to much
paramToDecl :: Param -> Decl
paramToDecl Param{..} =
  Decl pName pType