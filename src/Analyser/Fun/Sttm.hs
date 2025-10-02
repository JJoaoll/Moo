{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Analyser.Fun.Sttm where

import Grammar.Type
import Grammar.Sttm

import Analyser.Error
import Analyser.Fun.Expr
import Analyser.Fun.Pattern
import Analyser.Fun.FunContext.Def
import Analyser.Fun.FunContext.Utils

import Control.Monad
import Control.Lens hiding (Context)

checkSttm :: FunContext -> Sttm -> Either Error FunContext
ctx `checkSttm` (SInit name typε expr) = do 
  exprType <- ctx `checkExpr` expr
  if exprType == typε then do
    ctx `checkType` typε 
    addDecl name typε ctx -- missing to check TODO
  else
    Left Error -- TODO: incompatible types

ctx `checkSttm` (SAtrib name expr) = do
  exprType <- ctx `checkExpr` expr
  case ctx `findDecl` name of
    Nothing -> Left Error
    Just Decl{dclType} 
      | dclType == exprType -> pure ctx
      | otherwise -> Left Error
  
ctx `checkSttm` (SPrint expr) = do
  void $ ctx `checkExpr` expr
  pure ctx

ctx `checkSttm` (SScan typε) = do
  ctx `checkType` typε
  pure ctx
  
ctx `checkSttm` (SFunCall fName fArgs) = do
  case ctx `findFunDef` fName of
    Nothing -> Left Error
    Just f -> ctx <$ handleFun ctx f fArgs
  
ctx `checkSttm` (SMatch scrutinee cases) = do
  scrtnType <- ctx `checkExpr` scrutinee

  forM_ cases $ \(pttrn, sttms) -> do
    void $ checkPattern ctx pttrn scrtnType
    foldM_ checkSttm (enterBlock ctx) sttms

  pure ctx

ctx `checkSttm` (SWhile cond body) = do 
  condType <- ctx `checkExpr` cond
  if condType /= TBool then
    Left Error
  else 
    ctx <$ foldM checkSttm (enterBlock ctx) body

ctx `checkSttm` (SFor i is body) = do
  lType <- ctx `checkExpr` is
  case lType of
    TList a -> do
      newCtx <- ctx & enterBlock & addDecl i a
      ctx <$ foldM checkSttm newCtx body
    _ -> Left Error


ctx `checkSttm` (SReturn expr) = do
  exprType <- ctx `checkExpr` expr
  if (ctx ^. getRtrnType) == exprType then
    pure ctx
  else
    Left Error
