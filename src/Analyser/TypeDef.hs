-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Analyser.TypeDef where

import Analyser.Error

import Utils
import Grammar.Type 
import Control.Monad (forM, forM_, void, unless)

import Data.List


-- no repetitions by parsing?
checkTypeDef :: TypeDef -> [TypeDef] -> Either Error ()
checkTypeDef td ts = do
  forM_ (tConstrs td) $ \constr -> do
    checkConstr constr (tParams td) ts 
  pure ()

-- Esse Error se propagará?
checkConstr :: ConstrDef -> [Name] -> [TypeDef] -> Either Error ()
checkConstr c ps ts = do
  forM_ (cParams c) $ \param ->

    case param of
      TVar name -> --inline it please
        unless (name `elem` ps) 
          (Left Error)

      TData typeName typeArgs -> 
        -- 1. does the type exist? 
        case findType typeName ts of
          Nothing -> Left Error
          Just t -> 
        -- 2. how much args should it recieve?
            if length (tParams t) /= length typeArgs then
              Left Error
            else 
              checkTypeArgs typeArgs ps ts

      _ -> pure ()
  pure () 

checkTypeArgs :: [Type] -> [Name] -> [TypeDef] -> Either Error ()
checkTypeArgs [] _ _ = pure ()
checkTypeArgs (t:ts) typeParams typeDefs = do
  case t of
    TVar name -> unless (name `elem` typeParams) (Left Error)
    TData typeName typeArgs ->
      -- 1. does the type exist? 
      case findType typeName typeDefs of
        Nothing -> Left Error
        Just t' -> 
            -- 2. how much args should it recieve?
            if length (tParams t') /= length typeArgs then
              Left Error
            else do
              _ <- checkTypeArgs typeArgs typeParams typeDefs
              pure ()
    _ -> pure ()
  checkTypeArgs ts typeParams typeDefs

-- WHAT ABOUT THE TInt, TChar and TFloat
findType :: Name -> [TypeDef] -> Maybe TypeDef
findType name = find matchName
  where 
    matchName = (name ==) . tName