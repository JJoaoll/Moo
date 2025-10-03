{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Analyser.Context.Def
Description : Analysis context and declaration definitions for Moo type checking
Copyright   : (c) 2025 Moo Language Team
License     : MIT
Maintainer  : joaoduos@gmail.com
Stability   : experimental
Portability : POSIX

This module defines the analysis context used throughout Moo type checking.

= Analysis Context

The 'Context' holds all program-level definitions needed for analysis:

* Global variables and constants
* Function definitions  
* Custom type definitions

= Declaration Management  

The 'Decl' type represents variable declarations in local scopes:

* Variable name and type information
* Used in function parameter lists and local bindings

= Lens Integration

Uses Template Haskell to generate lenses for 'Context' field access:

@
-- Access context components
ctx ^. getGlobals    -- Get global variables
ctx ^. getFunDefs    -- Get function definitions  
ctx & getTypeDefs .~ newTypes  -- Update type definitions
@

= Example Usage

@
-- Create analysis context
let ctx = Ctx globals constants functions types

-- Look up function definition
findFunction \"factorial\" (ctx ^. getFunDefs)

-- Add local declaration
let localDecl = Decl \"x\" TInt
@
-}

module Analyser.Context.Def where

import Grammar.Program
import Grammar.Type

import Control.Lens hiding (element, Context, has)

import Utils
-- import Analyser.Context.Def (getTypeDefs)

-- | Analysis context containing all program-level definitions.
--
-- The context provides access to all definitions needed for type checking:
--
-- [@_getGlobals@] Global variable definitions with their types and values
-- [@_getConsts@] Compile-time constant definitions  
-- [@_getFunDefs@] Function definitions with signatures and bodies
-- [@_getTypeDefs@] Custom type definitions (algebraic data types)
--
-- Lenses are generated automatically via Template Haskell for convenient access.
-- the existence of this implies that u should refactor it
data Context = Ctx
  { _getGlobals  :: [GlobalDef] -- ^ Global variable definitions (could be just the bottom of the scopes)
  , _getConsts   :: [ConstDef]  -- ^ Constant definitions
  , _getFunDefs  :: [FunDef]    -- ^ Function definitions
  , _getTypeDefs :: [TypeDef]   -- ^ Custom type definitions
  } deriving (Eq, Show)

-- | Generate lenses for Context fields.
-- Creates: getGlobals, getConsts, getFunDefs, getTypeDefs
$(makeLenses ''Context)

-- | Variable declaration in local scope.
--
-- Represents a variable binding with its name and type:
--
-- [@dclName@] Variable identifier  
-- [@dclType@] Declared type of the variable
--
-- Used for function parameters, local variables, and pattern match bindings.
--
-- @
-- -- Function parameter declaration
-- paramDecl = Decl \"x\" TInt
--
-- -- Pattern match binding  
-- matchDecl = Decl \"result\" (TData \"Option\" [TInt])
-- @
-- Don't need values Here!
data Decl = Decl
  { dclName  :: Name  -- ^ Variable name identifier
  , dclType  :: Type  -- ^ Type of the declared variable
  -- , dclLevel :: ScopeLevel --> im already handleing it in the matrix!
  } deriving (Eq, Show)
