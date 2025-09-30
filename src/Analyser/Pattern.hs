module Analyser.Pattern where

import Grammar.Expr
import Grammar.Type
import Grammar.Sttm

import Analyser.Error
import Analyser.Expr
import Analyser.FunContext

import Control.Monad

checkECase :: FunContext -> (Pattern, Expr) -> Type -> Type -> Either Error Ok
checkECase ctx (pttrn, expr) pttrnType exprType = do
  void$checkPattern ctx pttrn pttrnType
  t <- ctx `checkExpr` expr
  if t == exprType then 
    pure Ok
  else
    Left Error -- TODO: Msg
            
                                -- scrutinee type
checkPattern :: FunContext -> Pattern -> Type -> Either Error Ok
checkPattern _ (PVar _) _  = pure Ok
checkPattern _ PWildcard _ = pure Ok
checkPattern _ (PLit lit) expectedType = 
  case (lit, expectedType) of 
    (LInt   _, TInt)    -> pure Ok
    (LChar  _, TChar)   -> pure Ok
    (LFloat _, TFloat)  -> pure Ok
    _ -> Left Error -- TODO: Error msg
                                          -- so faz sentido ser um data aqui!
                                          --
-- sla se funfa :P
checkPattern ctx (PConstructor constrName pttrns) (TData name ts) = 
  case ctx `findConstrAndTypeDefsByName` constrName of
    Nothing -> Left $ PatternNotFound constrName
    Just (ConstrDef{cParams}, TypeDef{tName, tParams})  -- verifiquei os tamnhos certos? e o ts?
      | tName /= name                   -> Left Error -- TODO: Error Msg
      | length pttrns  /= length cParams -> Left Error -- TODO: Error Msg
      | length tParams /= length ts            -> Left Error -- TODO: Error Mstg
      -- case everythings ok:
      | otherwise -> do
          case unifyTParams (TVar <$> tParams) ts cParams of
            Nothing -> Left Error
            Just ts' ->
              forM_ (zip pttrns ts') $ \(pttrn, expectedType)-> do 
                checkPattern ctx pttrn expectedType

          pure Ok
checkPattern _ _ _ = Left Error -- TODO Msg