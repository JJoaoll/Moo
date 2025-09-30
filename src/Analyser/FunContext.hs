{-# LANGUAGE NamedFieldPuns #-}

module Analyser.FunContext where

import Grammar.Program
import Grammar.Type

import Prelude hiding ((!!), any)
import Data.List.NonEmpty

import Utils

import Analyser.Error
import qualified Data.List as L
import Control.Applicative

-- ScopeLevel
type ScopeLevel = Int
type Scope = [Decl]

-- Just need to verify the type of the vars-4-now
data FunContext = FunCtx 
  { ctxDecls    :: NonEmpty Scope -- decl[SCOPE][VAR] 
  , ctxGlobals  :: [GlobalDef] -- could be just the bottom of the scopes
  , ctxConsts   :: [ConstDef]
  , ctxFunDefs  :: [FunDef]
  , ctxTypeDefs :: [TypeDef]
  , ctxLevel    :: ScopeLevel
  , ctxRtrnType :: Type
  } deriving (Eq, Show)

-- Don't need values Here!
data Decl = Decl
  { dclName  :: Name
  , dclType  :: Type
  -- , dclLevel :: ScopeLevel --> im already handleing it in the matrix!
  } deriving (Eq, Show)


---- Block dealer ----

addDecl :: Name -> Type -> FunContext -> Either Error FunContext
addDecl name t ctx@FunCtx{ctxDecls, ctxLevel} =
  let 
    d :| ds = ctxDecls
    newDecl = Decl name t
    
    currentScope = if ctxLevel == 0 then d else ds L.!! (ctxLevel - 1)
    alreadyExists = L.any (dclName ||> (==name)) currentScope
  in 
    if alreadyExists then
      Left Error
    else if ctxLevel == 0 then
      Right $ ctx { ctxDecls = (newDecl : d) :| ds }
    else 
      case L.splitAt (ctxLevel - 1) ds of
        (before, current:after) ->
          let updatedScope = newDecl : current
          in Right $ ctx { ctxDecls = d :| (before ++ [updatedScope] ++ after) }
        _ -> Left Error -- unreachable case

enterBlock :: FunContext -> FunContext
enterBlock ctx@FunCtx{ctxDecls, ctxLevel} = 
  let d :| ds = ctxDecls in
  ctx { ctxDecls = d :| (ds ++ [[]])  -- adds a empty scope in the ending
      , ctxLevel = ctxLevel + 1       -- the 'pointer' refers to the new scope 
      }

quitBlock :: FunContext -> Maybe FunContext
quitBlock (FunCtx _ _ _ _ _ 0 _) = Nothing
quitBlock ctx@FunCtx{ctxDecls, ctxLevel} =  do
  let d :| ds = ctxDecls 
  (ds', _) <- L.unsnoc ds
  pure $ 
    ctx { ctxDecls = d :| ds' -- removes the last scope
    , ctxLevel = ctxLevel - 1 -- updates the scope reference
    }

---- Finders ----

findDecl :: FunContext -> Name -> Maybe Decl
findDecl FunCtx{ctxDecls, ctxLevel} name =
  findDeclAux ctxDecls name ctxLevel
  where
    findDeclAux (decls :| _) dName 0 = 
      L.find (dclName ||> (==dName)) decls
    findDeclAux decls dName scope = 
      let scopeDecls = (decls !! scope)
        in L.find (dclName ||> (==dName )) scopeDecls
        <|> findDeclAux decls dName (scope-1)

findFunDef :: FunContext -> Name -> Maybe FunDef
findFunDef FunCtx{ctxFunDefs} name 
  = L.find (fName ||> (==name)) ctxFunDefs


findTypeDef :: FunContext -> Name -> Maybe TypeDef
findTypeDef FunCtx{ctxTypeDefs} name
  = L.find (tName ||> (==name)) ctxTypeDefs

findGlobalDef :: FunContext -> Name -> Maybe GlobalDef
findGlobalDef FunCtx{ctxGlobals} name
  = L.find (gName ||> (==name)) ctxGlobals

findConstDef :: FunContext -> Name -> Maybe ConstDef
findConstDef FunCtx{ctxConsts} name
  = L.find (kName ||> (==name)) ctxConsts

-- needs a fix
findConstrAndTypeDefsByName :: FunContext -> Name -> Maybe (ConstrDef, TypeDef)
findConstrAndTypeDefsByName FunCtx{ctxTypeDefs} constrName = do

  typeDef  <- L.find (tConstrs ||> has constrName) ctxTypeDefs
  cnstrDef <- L.find (cName ||> (==constrName)) (tConstrs typeDef)
  pure (cnstrDef, typeDef)

  where 
    has name = L.any (cName ||> (== name)) 