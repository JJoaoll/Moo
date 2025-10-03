{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Analyser.Context.Utils
Description : Utility functions for Moo language analysis context
Copyright   : (c) 2025 Moo Language Team
License     : GPL-3
Maintainer  : joaoduos@gmail.com
Stability   : experimental
Portability : POSIX

This module provides utility functions for working with analysis contexts.

= Context Utilities

Functions for:

* Converting parameters to declarations
* Type validation (concrete vs abstract types)
* Looking up definitions by name in context

= Lookup Functions  

The context provides several specialized lookup functions:

* 'findFunDef': Find function definitions by name
* 'findTypeDef': Find custom type definitions  
* 'findGlobalDef': Find global variable definitions
* 'findConstDef': Find constant definitions
* 'findConstrAndTypeDefsByName': Find constructor and its enclosing type

= Type Checking

'checkType' validates that types are concrete (no unbound type variables):

@
checkType ctx TInt                    -- Valid: built-in type
checkType ctx (TData \"List\" [TInt])   -- Valid: concrete type application  
checkType ctx (TVar \"a\")              -- Invalid: unbound type variable
@

= Example Usage

@
-- Convert function parameter to local declaration
let decl = paramToDecl (Param \"x\" TInt)

-- Look up definitions
maybeFun = findFunDef ctx \"factorial\"
maybeType = findTypeDef ctx \"Option\"
@
-}

module Analyser.Context.Utils where

import Analyser.Error 
import Analyser.Context.Def

import Grammar.Program
import Grammar.Type

import Control.Lens hiding (element, Context, has)
import qualified Data.List as L

import Control.Monad (forM_)

import Utils

-- | Convert a function parameter to a local declaration.
--
-- Used when entering function scope to create local variable declarations
-- from function parameter lists.
--
-- @
-- paramToDecl (Param \"x\" TInt) = Decl \"x\" TInt
-- @
-- indicates theres to much
paramToDecl :: Param -> Decl
paramToDecl Param{..} =
  Decl pName pType

-- | Check that a type is concrete (contains no unbound type variables).
--
-- Validates that types are fully instantiated:
-- * 'TVar': Always invalid (unbound type variables not allowed)
-- * 'TData': Must exist and have correct arity, recursively check arguments  
-- * Built-in types: Always valid ('TInt', 'TChar', 'TFloat', etc.)
--
-- @
-- checkType ctx TInt = Right ()                   -- Built-in type
-- checkType ctx (TData \"List\" [TInt]) = Right ()  -- Valid type application
-- checkType ctx (TVar \"a\") = Left Error         -- Unbound type variable  
-- @
-- concrete: 👍; abstract: 👎;
checkType :: Context -> Type -> Either Error ()
checkType _ (TVar _) = Left Error
ctx `checkType` (TData tName tArgs) = do
  case ctx `findTypeDef` tName of 
    Nothing -> Left Error
    Just TypeDef{tParams} 
      | L.length tArgs /= L.length tParams -> Left Error
      | otherwise -> forM_ tArgs (ctx `checkType`)
checkType _ _ = pure ()

---- Finders ----
-- no findDecl because we're not inside a function

-- | Find a function definition by name.
--
-- @
-- findFunDef ctx \"main\" = Just mainFunction
-- findFunDef ctx \"undefined\" = Nothing  
-- @
findFunDef :: Context -> Name -> Maybe FunDef
findFunDef ctx name = 
  L.find ((name==) . fName) (ctx ^. getFunDefs)

-- | Find a custom type definition by name.
findTypeDef :: Context -> Name -> Maybe TypeDef
findTypeDef ctx name -- {ctxTypeDefs} name
  = L.find ((name==) . tName) (ctx ^. getTypeDefs)

-- | Find a global variable definition by name.
findGlobalDef :: Context -> Name -> Maybe GlobalDef
findGlobalDef ctx name -- {ctxGlobals} name
  = L.find ((name==) . gName) (ctx ^. getGlobals)

-- | Find a constant definition by name.
findConstDef :: Context -> Name -> Maybe ConstDef
findConstDef ctx name --{ctxConsts} name
  = L.find ((name==) . kName) (ctx ^. getConsts)

-- | Find a constructor and its enclosing type definition by constructor name.
--
-- Searches all type definitions for a constructor with the given name,
-- returning both the constructor and the type that contains it.
--
-- @
-- findConstrAndTypeDefsByName ctx \"Some\" = Just (someConstructor, optionType)
-- findConstrAndTypeDefsByName ctx \"Unknown\" = Nothing
-- @
-- needs a fix
findConstrAndTypeDefsByName :: Context -> Name -> Maybe (ConstrDef, TypeDef)
findConstrAndTypeDefsByName ctx constrName = do

  typeDef  <- L.find (has constrName . tConstrs) (ctx ^. getTypeDefs)
  cnstrDef <- L.find ((==constrName) . cName) (tConstrs typeDef)
  pure (cnstrDef, typeDef)

  where 
    has name = L.any ((== name) . cName) 