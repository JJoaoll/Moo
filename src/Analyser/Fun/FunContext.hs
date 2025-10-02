{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Analyser.Fun.FunContext 
  (module Analyser.Fun.FunContext, 
   module Analyser.Context.Def) where

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

import Control.Lens hiding (element, Context, (|>))
import Control.Lens.TH

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
  { getCtx         -- TODO: tirar essa coisa de NonEmpty
  , getStack    :: NonEmpty Scope -- decl[SCOPE][VAR] -- rename -> Stack?
  , getLevel    :: ScopeLevel
  , getRtrnType :: Type
  } deriving (Eq, Show)

-- lens (https://hackage.haskell.org/package/lens-tutorial-1.0.5/docs/Control-Lens-Tutorial.html):
$(makeLenses ''FunContext)

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

addDecl :: Name -> Type -> FunContext -> Either Error FunContext
addDecl name t funCtx =
  let 
    d :| ds = ctxDecls
    newDecl = Decl name t
    
    currentScope = if ctxLevel == 0 then d else ds L.!! (ctxLevel - 1)
    alreadyExists = L.any (dclName ||> (==name)) currentScope
  in 
    if alreadyExists then
      Left Error
    else if ctxLevel == 0 then
      let ctxDecls = (newDecl : d) :| ds 
      in Right $ funCtx & getStack .~ ctxDecls -- LENS
    else 
      case L.splitAt (ctxLevel - 1) ds of
        (before, current:after) ->
          let updatedScope = newDecl : current
          in Right $ funCtx & getStack .~ d :| (before ++ [updatedScope] ++ after)  -- LENS
        _ -> Left Error -- unreachable case

enterBlock :: FunContext -> FunContext
enterBlock funCtx = 
  let d :| ds = funCtx & getStack in
    funCtx 
    |> getStack .~ d :| (ds ++ [[]])-- adds a empty scope in the ending using LENS
    |> getLevel %~ (+1)

--TODO:
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