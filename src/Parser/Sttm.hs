{-# LANGUAGE OverloadedStrings #-}

module Parser.Sttm where

import Grammar.Sttm

import Text.Megaparsec
import Text.Megaparsec.Char

import Parser.Utils.Utils
import Parser.Utils.Cases
import qualified Parser.Expr as E
import qualified Parser.Type as Type
import qualified Parser.Expr.Lit as Lit

-- ============================================================
-- MAIN STATEMENT PARSER
-- ============================================================

-- | Main statement parser
sttm :: Parser Sttm
sttm = choice $ try <$>
  [ sInit    <?> "variable inicialization"
  , sAtrib   <?> "assignment"
  , sGtrib   <?> "global assignment"
  , sPrint   <?> "print statement"
  , sMatch   <?> "match statement"
  , sWhile   <?> "while loop"
  , sFor     <?> "for loop"
  , sReturn  <?> "return statement"
  , sFunCall <?> "function call statement"
  ]

-- | Parse multiple statements
sttms :: Parser Sttms
sttms = many sttm
-- sttms = many sttm

-- ============================================================
-- VARIABLE OPERATIONS
-- ============================================================

-- | Variable declaration: let x: Int := 42
sInit :: Parser Sttm
sInit = do
  _ <- keyword "let"
  name <- lexeme snakeCase
  _ <- symbol ":"
  typε <- Type.typε
  _ <- symbol ":="
  expr <- E.expr
  pure $ SInit name typε expr

-- | Variable assignment: x := expr
sAtrib :: Parser Sttm
sAtrib = do
  name <- lexeme snakeCase
  _ <- symbol ":="
  expr <- E.expr
  pure $ SAtrib name expr

-- | Global assignment: @global := expr
sGtrib :: Parser Sttm
sGtrib = do
  _ <- char '@'
  name <- lexeme snakeCase
  _ <- symbol ":="
  expr <- E.expr
  pure $ SGtrib name expr

-- ============================================================
-- I/O OPERATIONS
-- ============================================================

-- | Print statement: print(expr)
sPrint :: Parser Sttm
sPrint = do
  _ <- keyword "print"
  _ <- symbol "("
  expr <- E.expr
  _ <- symbol ")"
  pure $ SPrint expr

-- ============================================================
-- CONTROL FLOW
-- ============================================================

-- | Pattern matching: match expr with ... end-match
sMatch :: Parser Sttm
sMatch = do
  _ <- keyword "match"
  scrutinee <- E.expr
  _ <- keyword "with"
  cases <- many matchCase
  _ <- keyword "end-match"
  pure $ SMatch scrutinee cases

-- | Single match case: pattern do sttms end-case
matchCase :: Parser (Pattern, Sttms)
matchCase = do
  _ <- keyword "case"
  pttrn <- pattern
  _ <- keyword "do"
  body <- sttms
  _ <- keyword "end-case"
  pure (pttrn, body)
  <|> do
  _ <- keyword "otherwise"
  _ <- keyword "do"
  body <- sttms
  _ <- keyword "end-case"
  pure (PWildcard, body)

-- | While loop: while cond do sttms end-while
sWhile :: Parser Sttm
sWhile = do
  _ <- keyword "while"
  cond <- E.expr
  _ <- keyword "do"
  body <- sttms
  _ <- keyword "end-while"
  pure $ SWhile cond body

-- | For loop: for item in items do sttms end-for
sFor :: Parser Sttm
sFor = do
  _ <- keyword "for"
  var <- lexeme snakeCase
  _ <- keyword "in"
  iterable <- E.expr
  _ <- keyword "do"
  body <- sttms
  _ <- keyword "end-for"
  pure $ SFor var iterable body

-- | Return statement: return expr
sReturn :: Parser Sttm
sReturn = do
  _ <- keyword "return"
  SReturn <$> E.expr

-- | Function call as statement: foo(x, y)
sFunCall :: Parser Sttm
sFunCall = do
  name <- lexeme camelCase
  _ <- symbol "("
  args <- E.expr `sepBy` symbol ","
  _ <- symbol ")"
  pure $ SFunCall name args

-- ============================================================
-- PATTERN MATCHING
-- ============================================================

-- | Pattern parser for match cases
pattern :: Parser Pattern
pattern = choice $ try <$>
  [ pConstructor <?> "constructor pattern"
  , pLit         <?> "literal pattern"
  , pVar         <?> "variable pattern"
  , pWildcard    <?> "wildcard pattern"
  ]

-- | Literal pattern: 42, 'a', True
pLit :: Parser Pattern
pLit = PLit <$> Lit.literal

-- | Variable pattern: x (binds value to x)
pVar :: Parser Pattern
pVar = PVar <$> lexeme snakeCase

-- | Wildcard pattern: _ (matches anything)
pWildcard :: Parser Pattern
pWildcard = PWildcard <$ symbol "_"

-- | Constructor pattern: Some(x), Cons(h, t), None
pConstructor :: Parser Pattern
pConstructor = try complex <|> simple
  where
    simple = do
      name <- lexeme pascalCase
      pure $ PConstructor name []

    complex = do
      name <- lexeme pascalCase
      _ <- symbol "("
      patterns <- pattern `sepBy` symbol ","
      _ <- symbol ")"
      pure $ PConstructor name patterns

