{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Analyser.Fun.FunContext.Def
  (module Analyser.Fun.FunContext.Def, 
   module Analyser.Context.Def) where

import Analyser.Context.Def

import Grammar.Type

import Prelude hiding ((!!), any)
import Data.List.NonEmpty

import Control.Lens hiding (has, Context)

-- ScopeLevel
type ScopeLevel = Int
type Scope = [Decl]

-- Just need to verify the type of the vars-4-now
-- Use Lens!!!!!!!!
-- data Context = Ctx
--   { getGlobals  :: [GlobalDef] -- could be just the bottom of the scopes
--   , getConsts   :: [ConstDef]
--   , getFunDefs  :: [FunDef]
--   , getTypeDefs :: [TypeDef]
--   } deriving (Eq, Show)

data FunContext = FunCtx 
  { _getCtx      :: Context      -- ✅ Underscore para lens
  , _getStack    :: NonEmpty Scope 
  , _getLevel    :: ScopeLevel
  , _getRtrnType :: Type
  } deriving (Eq, Show)

-- lens (https://hackage.haskell.org/package/lens-tutorial-1.0.5/docs/Control-Lens-Tutorial.html):
$(makeLenses ''FunContext)
