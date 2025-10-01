
module Analyser.Context.Utils where

import Analyser.Error 
import Analyser.Context.Def

import Grammar.Program
import Grammar.Type

import qualified Data.List as L

import Control.Monad (forM_)

import Utils


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
findFunDef Ctx{getFunDefs} name 
  = L.find (fName ||> (==name)) getFunDefs


findTypeDef :: Context -> Name -> Maybe TypeDef
findTypeDef Ctx{getTypeDefs} name
  = L.find (tName ||> (==name)) getTypeDefs

findGlobalDef :: Context -> Name -> Maybe GlobalDef
findGlobalDef Ctx{getGlobals} name
  = L.find (gName ||> (==name)) getGlobals

findConstDef :: Context -> Name -> Maybe ConstDef
findConstDef Ctx{getConsts} name
  = L.find (kName ||> (==name)) getConsts

-- needs a fix
findConstrAndTypeDefsByName :: Context -> Name -> Maybe (ConstrDef, TypeDef)
findConstrAndTypeDefsByName Ctx{getTypeDefs} constrName = do

  typeDef  <- L.find (tConstrs ||> has constrName) getTypeDefs
  cnstrDef <- L.find (cName ||> (==constrName)) (tConstrs typeDef)
  pure (cnstrDef, typeDef)

  where 
    has name = L.any (cName ||> (== name)) 