{-|
Module      : Analyser.Fun.FunContext.Utils
Description : Utility functions for function context scope management
Copyright   : (c) 2025 Moo Language Team
License     : MIT
Maintainer  : joaoduos@gmail.com
Stability   : experimental
Portability : POSIX

This module provides utilities for managing function analysis contexts.

= Scope Management

Functions for:

* Adding local variable declarations to current scope
* Entering and exiting nested block scopes  
* Looking up variables in the scope stack
* Type checking with function context

= Variable Lookup

Variable resolution follows lexical scoping rules:

1. Search current (top) scope first
2. Search progressively outer scopes  
3. Fall back to global context (constants, globals)
4. Report error if not found

= Block Scope Operations

* 'addDecl': Add variable to current scope
* 'enterScope': Create new nested scope  
* 'exitScope': Return to parent scope
* 'findDecl': Look up variable in scope stack

= Example Usage

@
-- Add local variable to current scope
funCtx' <- addDecl \"x\" TInt funCtx

-- Enter new block scope
let blockCtx = enterScope funCtx'

-- Look up variable  
maybeType = findDecl \"x\" blockCtx
@
-}

module Analyser.Fun.FunContext.Utils where

import Analyser.Context.Def

import Grammar.Program
import Grammar.Type

import Prelude hiding ((!!), any)
import Data.List.NonEmpty

import Utils  
import Control.Monad

import Analyser.Error
import qualified Data.List as L
import Control.Applicative

import Control.Lens hiding (has, Context)
import Analyser.Fun.FunContext.Def

import qualified Analyser.Context.Utils as Ctx

-- | Type check within function context (same as global context checking).
--
-- Validates that types are concrete with no unbound type variables:
-- * 'TVar': Invalid (no unbound type variables allowed)
-- * 'TData': Must exist with correct arity  
-- * Built-in types: Always valid
--
-- Uses the global context for type definition lookup.
-- concrete: 👍; abstract: 👎;
checkType :: FunContext -> Type -> Either Error ()
checkType _ (TVar _) = Left Error
ctx `checkType` (TData tName tArgs) = do
  case ctx `findTypeDef` tName of 
    Nothing -> Left Error
    Just TypeDef{tParams} 
      | L.length tArgs /= L.length tParams -> Left Error
      | otherwise -> forM_ tArgs (ctx `checkType`)
checkType _ _ = pure ()

---- Block dealer ----

-- | Add a variable declaration to the current (top) scope.
--
-- Creates a new declaration in the innermost scope of the function context.
-- Returns updated context or error if variable already exists in current scope.
--
-- @
-- addDecl \"x\" TInt funCtx  -- Add int variable \"x\" to current scope
-- @
addDecl :: Name -> Type -> FunContext -> Either Error FunContext
addDecl name t funCtx =
  let 
    d :| ds = funCtx ^. getStack 
    newDecl = Decl name t
    ctxLevel = funCtx ^. getLevel
    
    currentScope = if ctxLevel == 0 then d else ds L.!! (ctxLevel - 1)
    alreadyExists = L.any ((==name) . dclName) currentScope
  in 
    if alreadyExists then
      Left Error
    else if ctxLevel == 0 then
      let ctxDecls = (newDecl : d) :| ds in
        pure $ funCtx & getStack .~ ctxDecls  
    else 
      case L.splitAt (ctxLevel - 1) ds of
        (before, current:after) ->
          let updatedScope = newDecl : current
          in Right $ funCtx & getStack .~ d :| (before ++ [updatedScope] ++ after)  -- LENS
        _ -> Left Error -- unreachable case

enterBlock :: FunContext -> FunContext
enterBlock funCtx = 
  let d :| ds = funCtx ^. getStack in
    funCtx 
    & getStack .~ d :| (ds ++ [[]]) -- adds a empty scope in the ending using LENS
    & getLevel %~ (+1)

--TODO:
quitBlock :: FunContext -> Maybe FunContext
quitBlock (FunCtx _ _ 0 _) = Nothing
quitBlock funCtx =  do
  let d :| ds = funCtx ^. getStack
  (ds', _) <- L.unsnoc ds
  pure $ 
    funCtx
    & getStack .~ d :| ds'
    & getLevel %~ subtract 1

---- Finders ----

findDecl :: FunContext -> Name -> Maybe Decl
findDecl FunCtx{_getStack, _getLevel} name =
  findDeclAux _getStack name _getLevel
  where
    findDeclAux (decls :| _) dName 0 = 
      L.find ((==dName) . dclName) decls
    findDeclAux decls dName scope = 
      let scopeDecls = (decls !! scope)
        in L.find ((==dName) . dclName) scopeDecls
        <|> findDeclAux decls dName (scope-1)

findFunDef :: FunContext -> Name -> Maybe FunDef
findFunDef funCtx = Ctx.findFunDef (funCtx ^. getCtx)

findTypeDef :: FunContext -> Name -> Maybe TypeDef
findTypeDef funCtx = Ctx.findTypeDef (funCtx ^. getCtx)

findGlobalDef :: FunContext -> Name -> Maybe GlobalDef
findGlobalDef funCtx = Ctx.findGlobalDef (funCtx ^. getCtx) 

findConstDef :: FunContext -> Name -> Maybe ConstDef
findConstDef funCtx = Ctx.findConstDef (funCtx ^. getCtx) 

-- needs a fix
findConstrAndTypeDefsByName :: FunContext -> Name -> Maybe (ConstrDef, TypeDef)
findConstrAndTypeDefsByName funCtx 
  = Ctx.findConstrAndTypeDefsByName (funCtx ^. getCtx)
