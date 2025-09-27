{-# LANGUAGE OverloadedStrings #-}

module Parser.Samples.Ids where

-- import qualified Text.Megaparsec.Char.Lexer as L -- (1)
import Parser.Utils.Cases
import Data.Text

import Parser.Utils.Utils
varId, funId, typeId, constrId, constId, globalId :: Parser Text

varId    = lexeme snakeCase
funId    = lexeme camelCase
typeId   = lexeme pascalCase
constrId = lexeme pascalCase

constId = lexeme $ do
  _ <- symbol "<"
  constName <- kebabCase
  _ <- symbol ">"
  return constName

globalId = lexeme $ varId <* symbol "@"