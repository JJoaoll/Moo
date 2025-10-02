{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Analyser.Context.Def where

import Grammar.Program
import Grammar.Type

import Control.Lens hiding (element, Context, has)

import Utils
-- import Analyser.Context.Def (getTypeDefs)

-- the existence of this implies that u should refactor it
data Context = Ctx
  { _getGlobals  :: [GlobalDef] -- could be just the bottom of the scopes
  , _getConsts   :: [ConstDef]
  , _getFunDefs  :: [FunDef]
  , _getTypeDefs :: [TypeDef]
  } deriving (Eq, Show)

$(makeLenses ''Context)

-- Don't need values Here!
data Decl = Decl
  { dclName  :: Name
  , dclType  :: Type
  -- , dclLevel :: ScopeLevel --> im already handleing it in the matrix!
  } deriving (Eq, Show)
