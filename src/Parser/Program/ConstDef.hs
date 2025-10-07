{-# LANGUAGE OverloadedStrings #-}

module Parser.Program.ConstDef where

import Grammar.Program
import Grammar.Type
import Grammar.Expr (Lit(..))

import Parser.Utils.Utils
import Parser.Utils.Cases
import qualified Parser.Expr.Lit as Lit

-- | Parse constant definition: <const> flag := "value"
constDef :: Parser ConstDef
constDef = do
  _ <- symbol "<const>"
  name <- lexeme snakeCase
  _ <- symbol ":="
  value <- Lit.literal
  let typε = inferLitType value
  pure $ Const name typε value

-- | Infer type from literal value
inferLitType :: Lit -> Type
inferLitType lit = case lit of
  LInt _    -> TInt
  LChar _   -> TChar
  LFloat _  -> TFloat
  LConstr "True" []  -> TBool
  LConstr "False" [] -> TBool
  LConstr "Cons" _   -> TList TChar  -- String as list of chars
  LConstr "Nil" []   -> TList TChar  -- Empty list
  LConstr name args  -> TData name (map inferLitType args)  -- Generic constructor