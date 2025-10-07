{-# LANGUAGE OverloadedStrings #-}

module Parser.Program where

import Grammar.Program

import Text.Megaparsec

import Parser.Utils.Utils
import qualified Parser.Program.ConstDef as ConstDef
import qualified Parser.Program.GlobalDef as GlobalDef
import qualified Parser.Program.TypeDef as TypeDef
import qualified Parser.Program.FunDef as FunDef

-- | Parse a complete program
program :: Parser Program
program = do
  sc  -- consume initial whitespace
  types   <- many TypeDef.typeDef
  globals <- many GlobalDef.globalDef
  consts  <- many ConstDef.constDef
  funs    <- many FunDef.funDef
  eof
  pure $ Program globals consts funs types
