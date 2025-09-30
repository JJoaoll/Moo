{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Analyser.Sttm where


import qualified Analyser.TypeDef as TypeDef (findType)

import Grammar.Expr
import Grammar.Type
import Grammar.Sttm

import Analyser.Error
import Analyser.Expr
import Analyser.Pattern
import Analyser.FunContext

import Control.Monad
import Control.Monad.State.Strict (StateT, runStateT, get, put, lift, modify)

import qualified Data.List as L
import Data.List.NonEmpty

import Utils

-- maybe it would be bettter to just return the FunContext 🤡
-- TODO: make everything modularized
type CheckM = StateT FunContext (Either Error)

checkSttm :: FunContext -> Sttm -> Either Error FunContext
ctx `checkSttm` (SInit name typε expr) = do 
  exprType <- ctx `checkExpr` expr
  if exprType == typε then do
    ctx `checkType` typε 
    addDecl name typε ctx -- missing to check TODO
  else
    Left Error -- TODO: incompatible types

ctx `checkSttm` (SAtrib name expr) = undefined
ctx `checkSttm` (SPrint expr) = undefined
ctx `checkSttm` (SScan typε) = undefined
ctx `checkSttm` (SFunCall fName fArgs) = undefined
ctx `checkSttm` (SMatch scrutinee cases) = undefined
ctx `checkSttm` (SWhile cond body) = undefined
ctx `checkSttm` (SFor i is sttms) = undefined
ctx `checkSttm` (SReturn expr) = undefined


checkSttm _ _ = undefined

-- checkSttmM :: Sttm -> CheckM ()
-- checkSttmM (SInit name typε expr) = do -- ✅
--   ctx <- get
--   exprType <- lift $ ctx `checkExpr` expr
--   if exprType == typε then do
--     checkType typε
--     newCtx <- lift $ addDecl name typε ctx
--     put newCtx
--   else
--     lift $ Left Error -- TODO: incompatible types
--
-- checkSttmM (SAtrib name expr) = do -- ✅
--   ctx <- get
--   Decl{dclType} <- lift $ maybe (Left Error) Right (ctx `findDecl` name)
--   exprType <- lift $ ctx `checkExpr` expr
--   if dclType == exprType then
--     pure () 
--   else
--     lift $ Left Error -- TODO: incompatible types
--
-- checkSttmM (SPrint expr) = do -- ✅
--   ctx <- get
--   _ <- lift $ ctx `checkExpr` expr
--   pure ()
--
-- checkSttmM (SScan typε) = checkType typε -- ✅
--
-- checkSttmM (SFunCall fName fArgs) = do -- ✅
--   ctx <- get
--   if ctx `hasFun` fName then do
--     _ <- lift $ ctx `checkExpr` EFunCall fName fArgs
--     pure ()
--   else
--     lift $ Left Error 
--
-- checkSttmM (SMatch scrutinee cases) = do
--   ctx <- get
--   scrtnType <- lift $ ctx `checkExpr` scrutinee
--   forM_ cases $ \(pttrn, sttms) -> do
--     currentCtx <- get
--     void $ lift $ checkPattern currentCtx pttrn scrtnType
--     put (enterBlock currentCtx)
--     forM_ sttms checkSttmM 
--
--     -- back one step? (does it even work?)
--     blockCtx <- get
--     parentCtx <- lift $ maybe (Left Error) Right (quitBlock blockCtx)
--
--     put parentCtx
--
-- checkSttmM (SWhile cond body) = do
--   ctx <- get
--   condType <- lift $ ctx `checkExpr` cond
--   if condType /= TBool then
--     lift $ Left Error -- TODO: condição deve ser bool
--   else do
--     put (enterBlock ctx)
--     checkSttmsM body
--     blockCtx <- get
--     parentCtx <- lift $ maybe (Left Error) Right (quitBlock blockCtx)
--     put parentCtx
--
-- checkSttmM (SFor i is sttms) = do
--   ctx <- get
--   lsT <- lift $ ctx `checkExpr` is
--   case lsT of
--     TList a -> do
--       let blockCtx = enterBlock ctx
--       forCtx <- lift $ addDecl i a blockCtx
--       put forCtx
--       checkSttmsM sttms
--       -- Sai do bloco do for
--       finalCtx <- get
--       parentCtx <- lift $ maybe (Left Error) Right (quitBlock finalCtx)
--       put parentCtx
--     _ -> lift $ Left Error -- TODO: deve ser lista
--
-- checkSttmM (SReturn expr) = do
--   ctx <- get
--   exprType <- lift $ ctx `checkExpr` expr
--   if exprType /= ctxRtrnType ctx then
--     lift $ Left Error -- TODO: tipo incompatível
--   else
--     pure ()
--
-- -- Checa lista de statements sequencialmente
-- checkSttmsM :: [Sttm] -> CheckM ()
-- checkSttmsM = mapM_ checkSttmM
--
-- -- Função de conveniência para rodar CheckM
-- runCheckM :: FunContext -> CheckM a -> Either Error (a, FunContext)
-- runCheckM ctx action = runStateT action ctx
--
-- runCheckM_ :: FunContext -> CheckM a -> Either Error FunContext
-- runCheckM_ ctx action = fmap snd $ runStateT action ctx
--
-- -- =====================================
-- -- VERSÕES ANTIGAS (COMPATIBILIDADE)
-- -- =====================================
--
-- -- let x: Int = 3
-- -- Int x = 3;
-- checkSttm :: FunContext -> Sttm -> Either Error Ok
-- checkSttm ctx (SInit name typε expr) 
--   | name `isFreshVarIn` ctx  = do
--     exprType <- ctx `checkExpr` expr
--     if exprType == typε then
--       pure Ok
--     else
--       Left Error -- TODO: msg
--   | otherwise = Left Error -- TODO: msg
--
-- -- x = 42
-- checkSttm ctx (SAtrib name expr) = do
--   case ctx `findDecl` name of
--     Nothing -> Left Error -- TODO: msg 
--     Just Decl{dclType} -> do
--       exprType <- ctx `checkExpr` expr
--       if dclType == exprType then 
--         pure Ok
--       else 
--         Left Error -- TODO: msg
--
--
-- -- just check if the expr is ok and print wherever it is
--
-- checkSttm ctx (SPrint expr) = do
--   void $ ctx `checkExpr` expr
--   pure Ok
--
-- -- because ive already checked that all types
-- -- are already well formed, so i only need  
-- -- to find it exists!!
-- checkSttm ctx (SScan typε) = 
--   case typε of
--     TVar _ -> Left Error -- TODO: Msg
--     TData name _ 
--       | ctx `hasType` name -> pure Ok
--       | otherwise -> Left Error -- TODO: Msg
--     _ -> pure Ok
--
-- checkSttm ctx (SFunCall fName fArgs) 
--   | ctx `hasFun` fName = do
--       void $ ctx `checkExpr` EFunCall fName fArgs 
--       -- TODO: msg saying something if the retrn type of the 
--       -- function is "One"
--       pure Ok 
--   | otherwise = Left Error -- TODO: Msg
--
-- checkSttm ctx (SMatch scrutinee cases) = do
--   scrtnType <- ctx `checkExpr` scrutinee
--
--   forM_ cases $ \(pttrn, sttms) -> do
--     void $ checkPattern ctx pttrn scrtnType
--     forM_ sttms $ \sttm -> do
--       void $ ctx `checkSttm` sttm
--
--   pure Ok
--
-- checkSttm ctx (SWhile cond body) = do
--   condType <- ctx `checkExpr` cond
--   if condType /= TBool then
--     Left Error -- TODO: Msg
--   else do
--     -- TODO: OkMsgs?
--     _OkMsgs <- forM body (ctx `checkSttm`)
--     pure Ok
--
-- checkSttm ctx@FunCtx{ctxDecls} (SFor i is sttms) = do
--   lsT <- ctx `checkExpr` is
--   case lsT of
--     TList a -> do
--       let d :| ds = ctxDecls
--       forM_ sttms $ checkSttm ctx{ctxDecls = d :| (ds ++ [[Decl i a]]) }
--       pure Ok
--     _ -> Left Error
--
--
-- checkSttm ctx (SReturn expr) = do
--   exprType <- ctx `checkExpr` expr
--   if exprType /= ctxRtrnType ctx then
--     Left Error -- TODO: Msg
--   else
--     pure Ok
--
-- -- remove this booleanism!!!
-- isFreshVarIn :: Name -> FunContext -> Bool
-- name `isFreshVarIn` ctx = 
--   case ctx `findDecl` name of
--     Nothing -> True
--     Just _  -> False
--
-- hasType :: FunContext -> Name -> Bool
-- FunCtx{ctxTypeDefs} `hasType` typeName =
--   case TypeDef.findType typeName ctxTypeDefs of
--     Nothing -> False
--     Just _  -> True
--
-- hasFun :: FunContext -> Name -> Bool
-- ctx `hasFun` name =
--   case ctx `findFunDef` name of
--     Nothing -> False
--     Just _  -> True

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





-- paramToDecl :: ScopeLevel -> Param -> Decl
-- paramToDecl lvl Param{pName, pType} 
--   = Decl paramName paramType lvl

-- checkFuncDef :: FunDef -> FunContext -> Either Error Ok
-- checkFuncDef FunDef{fParams, body, rtrType} ctx@FunCtx{ctxDecls, ctxLevel} = do

--   let new_decls = paramToDecl ctxLevel <$> fParams
--   let ctx' = ctx { ctxDecls = new_decls ++ ctxDecls, ctxRtrnType = rtrType }

--   forM_ body $ checkSttm ctx'
--   pure Ok
