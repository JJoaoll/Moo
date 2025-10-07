{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Parser.Utils.Utils where

-- use :set -XOverloadedStrings
-- import Prelude (String(..))

import Text.Megaparsec.Char
import Data.Text hiding (any)
import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L -- (1)

type Parser = Parsec Void Text

-- space consumer
sc :: Parser ()
sc = L.space
  space1                         
  (L.skipLineComment "//")       
  (L.skipBlockComment "/*" "*/") 

lexeme :: Parser α -> Parser α
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

charLiteral :: Parser Char 
charLiteral = between (char '\'') (char '\'') L.charLiteral

parens :: Parser α -> Parser α
parens = between (symbol "(") (symbol ")")

-- | Parse a keyword (reserved word) ensuring it's not followed by alphanumeric characters
keyword :: Text -> Parser Text
keyword kw = lexeme (string kw <* notFollowedBy alphaNumChar)
