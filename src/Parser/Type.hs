{-# LANGUAGE OverloadedStrings #-}

module Parser.Type where

import Text.Megaparsec (try, sepBy, (<|>), choice)
import Text.Megaparsec.Char (string)
import Grammar.Type
import Data.Text

import Parser.Utils.Utils
import Parser.Utils.Cases (pascalCase)

pTypeName :: Parser Text
pTypeName = pascalCase

pPrimitiveType :: Parser Type
pPrimitiveType = choice
    [ string "Int"   >> return TInt
    , string "Char"  >> return TChar
    , string "Float" >> return TFloat
    ]

pSpecialType :: Parser Type
pSpecialType = do
    nome <- pTypeName
    return (TData nome [])

pSimpleType :: Parser Type
pSimpleType = try pPrimitiveType <|> pSpecialType

pComplexType :: Parser Type
pComplexType = do
    nome <- pTypeName
    _ <- symbol "("
    tipos <- pType `sepBy` symbol ","
    _ <- symbol ")"
    return (TData nome tipos)

pType :: Parser Type
pType = try pComplexType <|> pSimpleType