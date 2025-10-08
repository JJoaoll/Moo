{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Parser.Program.FunDef where

import Grammar.Program

import Text.Megaparsec

import Parser.Utils.Utils
import Parser.Utils.Cases
import qualified Parser.Type as Type
import qualified Parser.Sttm as Sttm

-- | Parse function definition: fun add(x: Int, y: Int) -> Int do ... end-add
funDef :: Parser FunDef
funDef = do
  _ <- keyword "fun"
  name <- lexeme camelCase <?> "func name"

  _ <- symbol "("                    <?> "open parens"
  params <- param `sepBy` symbol "," <?> "args"
  _ <- symbol ")"                    <?> "close parens"

  _ <- symbol "->"                   <?> "arrow \"->\""
  returnType <- Type.typε            <?> "return type"

  _ <- keyword "do"                  <?> "\"do\" keyword"
  body <- Sttm.sttms                 <?> "body"

  _ <- keyword ("end-" `mappend` name) <?> "end with right name"
  pure $ FunDef name params returnType body

-- | Parse function parameter: x: Int
param :: Parser Param
param = do
  name <- lexeme snakeCase
  _ <- symbol ":"
  typε <- Type.typε

  pure $ Param name typε