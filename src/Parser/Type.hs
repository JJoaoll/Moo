{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}  

module Parser.Type where

import Text.Megaparsec (try, sepBy, (<|>), choice, (<?>))
import Text.Megaparsec.Char (string)
import Grammar.Type ( Type(TData, TInt, TChar, TFloat) )
import Data.Text ( Text )

import Parser.Utils.Utils (symbol, lexeme, Parser)
import Parser.Utils.Cases (pascalCase)

typε :: Parser Type 
typε = lexeme $
  try primitive <|> custom

primitive :: Parser Type
primitive = choice
  [ string "Int"   >> return TInt
  , string "Char"  >> return TChar
  , string "Float" >> return TFloat
  ] 

custom :: Parser Type
custom = (try complex <|> simple) <?> "Custom Type"

simple :: Parser Type
simple = TData <$> name <*> pure []
-- simple = do
--   tName <- name
--   pure $ TData tName []

complex :: Parser Type
complex = do
  tName <- name
  _ <- symbol "("
  tArgs <- typε `sepBy` symbol ","
  _ <- symbol ")"

  pure $ TData tName tArgs

name :: Parser Text
name = lexeme pascalCase