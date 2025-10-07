{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Parser.Program.ConstDef where

import Grammar.Program
import qualified Parser.Type as T (typε)

import Parser.Utils.Utils
import Parser.Utils.Cases
import qualified Parser.Expr.Lit as Lit

-- | Parse constant definition: <const> flag := "value"
constDef :: Parser ConstDef
constDef = do
  _ <- keyword "<const>"
  name <- lexeme snakeCase

  _ <- symbol ":"
  typε <- T.typε 

  _ <- symbol ":="
  value <- Lit.literal

  pure $ Const name typε value
