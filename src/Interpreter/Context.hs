{-# LANGUAGE TemplateHaskell #-}
{-# Language PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Interpreter.Context where

import Grammar.Expr
import Grammar.Sttm
import Grammar.Type
import Grammar.Program

import Control.Lens hiding (Context, has)
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

-- i wanna be a true list 😭
listoquio :: Value -> [Value]
listoquio VNil = []
listoquio (VCons x xs) =
  x:listoquio xs
listoquio weird = error "write me"

-- declarated Var
data DVar = DVar
  { _vName :: Text
  -- , _vType :: Type
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


withDVar :: Context -> (Name, Value) -> Either Text Context
ctx `withDVar` (name, val) =
  let
    stack = ctx ^. cStack
    newDecl = DVar name val
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

modifyGlobalVal :: Context -> (Name, Value) -> Either Text Context
ctx `modifyGlobalVal` (name, val) = 
  case L.find ((name ==) . (^. vName)) (ctx ^. cGlobs) of
    Nothing -> Left "global variable not found"
    Just _ -> 
      Right $ ctx & cGlobs %~ _replace (name, val)

      where 
        _replace _ [] = []
        _replace (name, val) (g:gs) 
          | name /= (g ^. vName) = g:_replace (name, val) gs
          | otherwise = (g & vVal .~ val):gs
          


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


matches :: Value -> Pattern -> Bool
val `matches` (PLit lit) =
  case lit of
    LInt n -> val == VInt n
    LChar c -> val == VChar c
    LFloat x -> val == VFloat x
    LConstr cName cLits ->
      case val of
        VData vName vArgs ->
          cName == vName
          && L.length cLits == L.length vArgs
          && and (L.zipWith matches vArgs $ PLit <$> cLits)

        _ -> False

val `matches` PConstructor cName cPatterns =
  case val of
    VData vName vArgs ->
      cName == vName
      && L.length cPatterns == L.length vArgs
      && and (L.zipWith matches vArgs cPatterns)
    _ -> False
val `matches` _ = True


findTypeDef :: Context -> Name -> Maybe TypeDef
findTypeDef ctx name 
  = L.find ((name==) . tName) (ctx ^. cTypes)

findFunDef :: Context -> Name -> Maybe FunDef
findFunDef ctx name = 
  L.find ((name==) . fName) (ctx ^. cFuncs)


  -- = LInt   Int    -- ^ Integer literal (e.g., @42@, @-17@)
  -- | LChar  Char   -- ^ Character literal (e.g., @'a'@, @'\\n'@)
  -- | LFloat Float  -- ^ Float literal (e.g., @3.14@, @-2.5@)
  -- | LConstr Name [Lit]  -- ^ Constructor with arguments (e.g., @"Just" [LInt 5]@)

lit2Val :: Lit -> Value
lit2Val (LInt n) = VInt n
lit2Val (LChar c) = VChar c
lit2Val (LFloat x) = VFloat x
lit2Val (LConstr cName cArgs) = 
  VData cName (lit2Val <$> cArgs)

checkLitType :: Context -> Lit -> Maybe Type
ctx `checkLitType` lit = 
  case lit of
    LInt _ -> Just TInt
    LChar _ -> Just TChar
    LFloat _ -> Just TFloat
    LConstr cName cArgs -> do
      (_, TypeDef{tName}) <- ctx `findConstrAndTypeDefsByName` cName
      argTypes <- mapM (ctx `checkLitType`) cArgs

      pure $ TData tName argTypes


findConstrAndTypeDefsByName :: Context -> Name -> Maybe (ConstrDef, TypeDef)
findConstrAndTypeDefsByName ctx constrName = do

  typeDef  <- L.find (has constrName . tConstrs) (ctx ^. cTypes)
  cnstrDef <- L.find ((==constrName) . cName) (tConstrs typeDef)
  pure (cnstrDef, typeDef)

  where 
    has name = L.any ((== name) . cName) 