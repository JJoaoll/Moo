{-# LANGUAGE OverloadedStrings #-}

module Parser.Program.TypeDef where

import Grammar.Type

import Text.Megaparsec

import Parser.Utils.Utils
import Parser.Utils.Cases
import qualified Parser.Type as Type

-- | Parse type definition
-- 
-- Accepts types with 0 or more constructors.
--
-- Examples:
--
--   * @type Empty def end-def@  (uninhabited type)
--   * @type Bool def True False end-def@
--   * @type Option(a) def None Some(a) end-def@
--   * @type Tree(a) def Leaf(a) Node(Tree(a), a, Tree(a)) end-def@
typeDef :: Parser TypeDef
typeDef = do
  _ <- keyword "type"
  name   <- lexeme pascalCase
  params <- lexeme typeParams

  _ <- keyword "def"
  constrs <- many constrDef

  _ <- keyword "end-def"
  pure $ TypeDef name params constrs

  -- | Parse type parameters: (a) or (a, b) or empty
  where typeParams = option [] $ parens (lexeme snakeCase `sepBy` symbol ",")

-- | Parse constructor definition: Leaf(a) or None
constrDef :: Parser ConstrDef
constrDef = do
  name <- lexeme pascalCase
  params <- option [] $ parens (typε `sepBy` symbol ",")
  pure $ ConstrDef name params

  -- | Parses concrete or abstract type
  where typε = try Type.typε <|> (TVar <$> lexeme snakeCase)