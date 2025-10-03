{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Analyser.Fun.FunContext.Def
Description : Function analysis context definition with scope management
Copyright   : (c) 2025 Moo Language Team
License     : GPL-3
Maintainer  : joaoduos@gmail.com
Stability   : experimental
Portability : POSIX

This module defines the function analysis context used during function body type checking.

= Function Context

The 'FunContext' extends the global 'Context' with function-specific information:

* Local variable scope stack for nested scoping
* Current scope level tracking  
* Expected return type for validation

= Scope Management

Uses a 'NonEmpty' stack of scopes where:

* Each scope contains local variable declarations  
* Stack grows/shrinks with block entry/exit
* Variable lookup searches from top to bottom of stack

= Lens Integration

Template Haskell generates lenses for convenient field access:

@
-- Access function context components  
funCtx ^. getCtx         -- Get global context
funCtx ^. getStack       -- Get scope stack
funCtx ^. getLevel       -- Get current scope level
funCtx ^. getRtrnType    -- Get expected return type

-- Update function context
funCtx & getLevel %~ (+1)                    -- Increment scope level
funCtx & getStack %~ (newScope <|)           -- Push new scope
@

= Example Usage

@
-- Create function context for analysis
let funCtx = FunCtx globalCtx parameterScope 0 TInt

-- Enter new block scope
let newCtx = funCtx & getLevel %~ (+1) 
                    & getStack %~ ([] <|)

-- Exit block scope  
let exitCtx = newCtx & getLevel %~ subtract 1
                     & getStack %~ NE.tail
@
-}

module Analyser.Fun.FunContext.Def
  (module Analyser.Fun.FunContext.Def, 
   module Analyser.Context.Def) where

import Analyser.Context.Def

import Grammar.Type

import Prelude hiding ((!!), any)
import Data.List.NonEmpty

import Control.Lens hiding (has, Context)

-- | Scope nesting level (0 = function parameter scope, 1+ = nested blocks)
type ScopeLevel = Int

-- | A scope containing local variable declarations
type Scope = [Decl]

-- | Function analysis context with local scope management.
--
-- Extends the global 'Context' with function-specific state:
--
-- [@_getCtx@] Global analysis context (functions, types, globals, constants)
-- [@_getStack@] Stack of local scopes (inner scopes at the head)  
-- [@_getLevel@] Current nesting level (0 = function parameters)
-- [@_getRtrnType@] Expected return type for function validation
--
-- The scope stack uses 'NonEmpty' to ensure at least the parameter scope exists.
-- Variable lookup searches from the top (most nested) to bottom (parameters).
-- Just need to verify the type of the vars-4-now
-- Use Lens!!!!!!!!
-- data Context = Ctx
--   { getGlobals  :: [GlobalDef] -- could be just the bottom of the scopes
--   , getConsts   :: [ConstDef]
--   , getFunDefs  :: [FunDef]
--   , getTypeDefs :: [TypeDef]
--   } deriving (Eq, Show)

data FunContext = FunCtx 
  { _getCtx      :: Context      -- ^ Global analysis context (✅ Underscore para lens)
  , _getStack    :: NonEmpty Scope  -- ^ Local scope stack (innermost first)
  , _getLevel    :: ScopeLevel      -- ^ Current scope nesting level  
  , _getRtrnType :: Type            -- ^ Expected function return type
  } deriving (Eq, Show)

-- | Generate lenses for FunContext fields.
-- Creates: getCtx, getStack, getLevel, getRtrnType
-- lens (https://hackage.haskell.org/package/lens-tutorial-1.0.5/docs/Control-Lens-Tutorial.html):
$(makeLenses ''FunContext)
