{-# LANGUAGE RecordWildCards #-}

module Analyser.Context.Utils where

import Analyser.Error 
import Analyser.Context.Def

import Grammar.Program
import Grammar.Type

import Control.Lens hiding (element, Context, has)
import qualified Data.List as L

import Control.Monad (forM_)

import Utils

-- indicates theres to much
paramToDecl :: Param -> Decl
paramToDecl Param{..} =
  Decl pName pType

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

findFunDef :: Context -> Name -> Maybe FunDef
findFunDef ctx name = 
  L.find ((name==) . fName) (ctx ^. getFunDefs)

findTypeDef :: Context -> Name -> Maybe TypeDef
findTypeDef ctx name -- {ctxTypeDefs} name
  = L.find ((name==) . tName) (ctx ^. getTypeDefs)

findGlobalDef :: Context -> Name -> Maybe GlobalDef
findGlobalDef ctx name -- {ctxGlobals} name
  = L.find ((name==) . gName) (ctx ^. getGlobals)

findConstDef :: Context -> Name -> Maybe ConstDef
findConstDef ctx name --{ctxConsts} name
  = L.find ((name==) . kName) (ctx ^. getConsts)

-- needs a fix
findConstrAndTypeDefsByName :: Context -> Name -> Maybe (ConstrDef, TypeDef)
findConstrAndTypeDefsByName ctx constrName = do

  typeDef  <- L.find (has constrName . tConstrs) (ctx ^. getTypeDefs)
  cnstrDef <- L.find ((==constrName) . cName) (tConstrs typeDef)
  pure (cnstrDef, typeDef)

  where 
    has name = L.any ((== name) . cName) 