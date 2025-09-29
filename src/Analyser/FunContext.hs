{-# LANGUAGE NamedFieldPuns #-}

module Analyser.FunContext where

import Grammar.Program
import Grammar.Type

import Prelude hiding ((!!), any)
import Data.List.NonEmpty

import Utils

import qualified Data.List as L
import Control.Applicative

import Data.Text ( Text )  
import Analyser.Error

-- ScopeLevel
type ScopeLevel = Int

-- Just need to verify the type of the vars-4-now
data FunContext = FunCtx 
  { ctxDecls    :: NonEmpty [Decl] -- decl[SCOPE][VAR] 
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
  , dclLevel :: ScopeLevel
  } deriving (Eq, Show)

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