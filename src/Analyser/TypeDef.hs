-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Analyser.TypeDef
Description : Type definition analysis and validation for Moo custom types
Copyright   : (c) 2025 Moo Language Team
License     : MIT
Maintainer  : joaoduos@gmail.com
Stability   : experimental
Portability : POSIX

This module validates custom type definitions in Moo programs.

= Type Definition Validation

Checks that custom types are well-formed:

* Constructor parameters reference valid types
* Type variables are properly scoped
* Generic type arguments have correct arity
* Recursive type references are valid

= Validation Rules

1. __Type Variables__: Must be declared in the type parameter list
2. __Data Types__: Must exist and receive correct number of arguments  
3. __Built-in Types__: 'TInt', 'TChar', 'TReal' are always valid
4. __Recursion__: Types can reference themselves or other defined types

= Example Usage

@
-- Valid type definition
data Option a = None | Some a

-- Validation checks:
checkTypeDef optionTypeDef allTypes  -- Verifies 'a' is properly scoped

-- Invalid type definition  
data BadType = BadConstr UnknownType  -- Would fail: UnknownType not defined
@
-}

module Analyser.TypeDef where

import Analyser.Error

import Utils
import Grammar.Type 
import Control.Monad 

import Data.List

-- | Validate a type definition against all available type definitions.
--
-- Checks that all constructors are well-formed:
-- * Constructor parameters reference valid types
-- * Type variables are properly scoped within the type definition
-- * Generic type applications have correct arity
--
-- @
-- -- Valid: Option type with properly scoped variable 'a'
-- checkTypeDef (TypeDef \"Option\" [\"a\"] [ConstrDef \"None\" [], ConstrDef \"Some\" [TVar \"a\"]])
--
-- -- Invalid: 'b' not in scope  
-- checkTypeDef (TypeDef \"Bad\" [\"a\"] [ConstrDef \"Constr\" [TVar \"b\"]])  -- Error
-- @
-- no repetitions by parsing?
checkTypeDef :: TypeDef -> [TypeDef] -> Either Error ()
checkTypeDef td ts = do
  forM_ (tConstrs td) $ \constr -> do
    checkConstr constr (tParams td) ts 
  pure ()

-- | Validate a constructor definition within a type context.
--
-- Checks each constructor parameter:
-- * 'TVar' names must be in the type parameter list
-- * 'TData' types must exist and have correct arity
-- * Built-in types ('TInt', 'TChar', etc.) are always valid
--
-- @
-- -- Valid: 'a' is in type parameters [\"a\"]
-- checkConstr (ConstrDef \"Some\" [TVar \"a\"]) [\"a\"] allTypes
--
-- -- Invalid: 'List' expects 1 argument but gets 0
-- checkConstr (ConstrDef \"Bad\" [TData \"List\" []]) [] allTypes  -- Error
-- @
-- Esse Error se propagará?
checkConstr :: ConstrDef -> [Name] -> [TypeDef] -> Either Error ()
checkConstr c ps ts = do
  forM_ (cParams c) $ \param ->

    case param of
      TVar name -> --inline it please
        unless (name `elem` ps) 
          (Left $ UnboundTypeVar name)

      TData typeName typeArgs -> 
        -- 1. does the type exist? 
        case findType typeName ts of
          Nothing -> Left $ TypeNotFound typeName
          Just t -> 
        -- 2. how much args should it recieve?
            if length (tParams t) /= length typeArgs then
              Left $ TypeArityMismatch typeName (length $ tParams t) (length typeArgs)
            else 
              checkTypeArgs typeArgs ps ts

      _ -> pure ()
  pure () 

-- | Recursively validate type arguments.
--
-- Ensures that all type arguments in a generic type application are valid:
-- * Type variables must be in scope
-- * Data types must exist with correct arity
-- * Recursively validates nested type applications
--
-- @
-- checkTypeArgs [TVar \"a\", TData \"List\" [TVar \"b\"]] [\"a\", \"b\"] allTypes
-- @
checkTypeArgs :: [Type] -> [Name] -> [TypeDef] -> Either Error ()
checkTypeArgs [] _ _ = pure ()
checkTypeArgs (t:ts) typeParams typeDefs = do
  case t of
    TVar name -> unless (name `elem` typeParams) (Left $ UnboundTypeVar name)
    TData typeName typeArgs ->
      -- 1. does the type exist? 
      case findType typeName typeDefs of
        Nothing -> Left $ TypeNotFound typeName
        Just t' -> 
            -- 2. how much args should it recieve?
            if length (tParams t') /= length typeArgs then
              Left $ TypeArityMismatch typeName (length $ tParams t') (length typeArgs)
            else do
              _ <- checkTypeArgs typeArgs typeParams typeDefs
              pure ()
    _ -> pure ()
  checkTypeArgs ts typeParams typeDefs

-- | Find a type definition by name.
--
-- Searches for a custom type definition in the list of available types.
-- Does not handle built-in types ('TInt', 'TChar', etc.) - those are 
-- considered always valid.
--
-- @
-- findType \"Option\" [optionType, listType] = Just optionType
-- findType \"Unknown\" [] = Nothing
-- @
-- WHAT ABOUT THE TInt, TChar and TFloat
findType :: Name -> [TypeDef] -> Maybe TypeDef
findType name = find matchName
  where 
    matchName = (name ==) . tName