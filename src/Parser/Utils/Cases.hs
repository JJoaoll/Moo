module Parser.Utils.Cases where

import Data.Char (isLower, isUpper, isDigit, isAlphaNum)
import Text.Megaparsec.Char
import Data.Text hiding (any)
import Text.Megaparsec

import Parser.Utils.Utils

snakeCase :: Parser Text
snakeCase = do
  c  <- lowerChar
  cs <- takeWhileP Nothing 
     (\x -> any ($ x) [isLower, isDigit, (== '_')])

  return (c `cons` cs)

camelCase :: Parser Text
camelCase = do
  c  <- lowerChar
  cs <- takeWhileP Nothing isAlphaNum

  return (c `cons` cs)

pascalCase :: Parser Text
pascalCase = do
  c  <- upperChar 
  cs <- takeWhileP Nothing isAlphaNum

  return (c `cons` cs)

kebabCase :: Parser Text
kebabCase = do
  c  <- lowerChar
  cs <- takeWhileP Nothing 
     (\x -> any ($ x) [isLower, isDigit, (== '-')])

  return (c `cons` cs)
  
screamingKebabCase :: Parser Text
screamingKebabCase = do
  c  <- upperChar
  cs <- takeWhileP Nothing 
     (\x -> any ($ x) [isUpper, isDigit, (== '-')])

  return (c `cons` cs)

