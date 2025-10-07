{-# LANGUAGE OverloadedStrings #-}

module Parser.Program.GlobalDef where

import Grammar.Program

import Parser.Utils.Utils
import Parser.Utils.Cases
import qualified Parser.Type as Type
import qualified Parser.Expr.Lit as Lit

-- | Parse global variable definition: @global abelha: Int := 42
globalDef :: Parser GlobalDef
globalDef = do
  _ <- symbol "@global"
  name <- lexeme snakeCase
  _ <- symbol ":"
  typε <- Type.typε
  _ <- symbol ":="
  value <- Lit.literal
  pure $ Global name typε value