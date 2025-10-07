{-# LANGUAGE OverloadedStrings #-}

module Parser.Program.TypeDef where

import Utils (Name)
import Grammar.Type

import Text.Megaparsec

import Parser.Utils.Utils
import Parser.Utils.Cases
import qualified Parser.Type as Type

-- | Parse type definition: type Tree(a) def Leaf(a) Node(Tree(a), a, Tree(a)) end-def
typeDef :: Parser TypeDef
typeDef = do
  _ <- keyword "type"
  name <- lexeme pascalCase
  params <- typeParams
  _ <- keyword "def"
  constrs <- some constrDef
  _ <- keyword "end-def"
  pure $ TypeDef name params constrs

-- | Parse type parameters: (a) or (a, b) or empty
typeParams :: Parser [Name]
typeParams = option [] $ parens (lexeme snakeCase `sepBy` symbol ",")

-- | Parse constructor definition: Leaf(a) or None
constrDef :: Parser ConstrDef
constrDef = do
  name <- lexeme pascalCase
  params <- option [] $ parens (Type.typε `sepBy` symbol ",")
  pure $ ConstrDef name params