{-# LANGUAGE OverloadedStrings #-}

module Parser.Samples.Ids where

-- import qualified Text.Megaparsec.Char.Lexer as L -- (1)
import Parser.Utils.Cases
import Text.Megaparsec
import Data.Text
import Control.Monad 

import Parser.Utils.Utils
varId, funId, typeId, constrId, constId, globalId :: Parser Text

varId    = lexeme snakeCase
funId    = lexeme camelCase
typeId   = lexeme pascalCase
constrId = lexeme pascalCase

-- <tarifa>
constId = lexeme $ do
  void $ symbol "<"
  constName <- kebabCase <?> "kebabCase"
  void $ symbol ">"
  return constName

-- @count
globalId = lexeme $ varId <* symbol "@"