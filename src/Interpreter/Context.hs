{-# LANGUAGE TemplateHaskell #-}
{-# Language PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Context where

import Grammar.Type
import Grammar.Program

import Control.Lens hiding (Context)
import qualified Data.List as L

import Data.Text
import Utils
-- import Interpreter.InterpretT (InterpretT)

-- modularize Values!
data Value 
  = VInt    Int
  | VChar   Char
  | VFloat  Float
  -- some day..
  -- | VString String
  -- | VOption (Maybe Value)
  -- | VList   [Value] -- not garantee that they'll have the same type by types :c
  | VData Name [Value] -- Lisp
  deriving (Eq, Show)

-- Booleans:
pattern VTrue, VFalse :: Value
pattern VTrue  = VData "True" []
pattern VFalse = VData "False" []

-- list_p
pattern VNil :: Value
pattern VCons :: Value -> Value -> Value
pattern VNil = VData "Nil" []
pattern VCons x xs = VData "Cons" [x, xs]

mooCat :: Value -> Value -> Value
mooCat VNil ys = ys
mooCat (VCons x xs) ys = VCons x (mooCat xs ys)
mooCat xs ys
  = error $ "tried to concatenate things"
  ++ " that were not lists, but: " ++ show xs 
  ++ " and " ++ show ys

-- declarated Var
data DVar = DVar
  { _vName :: Text
  , _vType :: Type
  , _vVal  :: Value
  } deriving (Eq, Show)

$(makeLenses ''DVar)

data Context = Context
  { _cTypes :: [TypeDef]
  , _cFuncs :: [FunDef]
  , _cGlobs :: [DVar]   -- it could just be the bottom of the stack.. (but would be problems with atribs..)
  , _cStack :: [[DVar]] -- positions represents the scope level. -- USING MAPS COULD IMPROVE PERFORMANCE!!!
  , _cScope :: Int
  } deriving (Eq, Show)

$(makeLenses ''Context)

withDVar :: Context -> (Name, Type, Value) -> Either Text Context
ctx `withDVar` (name, typε, val) =
  let 
    stack = ctx ^. cStack
    newDecl = DVar name typε val
    ctxLevel = ctx ^. cScope
  in 
    case stack L.!? ctxLevel of
      Nothing -> Left "invalid scope level"
      Just currentScope 
        | L.any ((name==) . (^. vName)) currentScope ->
            Left "already Exists"
        | otherwise ->
          case L.splitAt ctxLevel stack of
            (before, current:after) ->
              let updatedScope = newDecl : current
              in Right $ ctx & cStack .~ (before ++ [updatedScope] ++ after)
            _ -> Left "invalid scope structure"

modifyDVarVal :: Context -> (Name, Value) -> Either Text Context
ctx `modifyDVarVal` (name, val) =
  let 
    stack = ctx ^. cStack
    mCtxLevel = firstOcurrenceOf name (ctx ^. cScope) stack
  -- because of the analysis, the variable is avaiable somewhere!
  in case mCtxLevel of
    Nothing -> Left "problem with scope counting catched by \"modifyDVarVal\""
    Just ctxLevel -> 
      case L.splitAt ctxLevel stack of
        (before, current:after) ->
          let updatedScope = (name, val) `replaceIn` current
          in Right $ ctx & cStack .~ (before ++ [updatedScope] ++ after)
        _ -> Left "invalid scope structure"

blockItUp :: Context -> Context
blockItUp ctx = 
    ctx  
    & cStack %~ (++ [[]])
    & cScope %~ (+1)

blockItDown :: Context -> Maybe Context
blockItDown (Context _ _ _ _ 0) = Nothing
blockItDown ctx = 
  case ctx ^. cStack of
    [] -> Nothing
    stack -> Just $ 
      ctx & cStack .~ L.init stack
          & cScope %~ subtract 1

-- better error handling?
firstOcurrenceOf :: Name -> Int -> [[DVar]] -> Maybe Int
firstOcurrenceOf _ currentScope _ | currentScope < 0 = Nothing -- base 
firstOcurrenceOf name currentScope dss =
  case dss L.!? currentScope of
    Nothing -> Nothing -- invalid Scope! 🤡
    Just ds 
      | L.any ((name ==) . (^. vName)) ds -> Just currentScope  -- found!
      | otherwise -> firstOcurrenceOf name (currentScope-1) dss -- one less..


replaceIn :: (Name, Value) -> [DVar] -> [DVar]
_ `replaceIn` [] = []
(name, val) `replaceIn` (d:ds)
  | d ^. vName == name = (d & vVal .~ val) : ds
  | otherwise = d : ((name, val) `replaceIn` ds)

