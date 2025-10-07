{-# LANGUAGE OverloadedStrings #-}

module Parser.Expr where

import Grammar.Expr

import Text.Megaparsec 
import Text.Megaparsec.Char
-- import Grammar.Type
import Data.Text 

import qualified Text.Megaparsec.Char.Lexer as L
import Parser.Utils.Utils 
import Parser.Utils.Cases
-- import Control.Monad (when)
import GHC.Float (int2Float)
import Parser.Utils.Cases (snakeCase)

import qualified Parser.Expr.Lit as Lit (literal)
import qualified Parser.Type as Type 
import Parser.Expr.Op

expr :: Parser Expr
expr = choice $ try <$>
  [ literal      <?> "literal"
  , constructor  <?> "constructor"
  , funCall      <?> "fun call"
  , variable     <?> "variable"
  , constant     <?> "constant"
  , global       <?> "global"
  -- , operation
  , scan         <?> "scan!"
  --, special -- builtin
  ] 

literal, constructor, variable, constant, global, operation, funCall, scan :: Parser Expr

literal = ELit <$> Lit.literal

constructor = 
  try complex <|> simple

variable = EVar <$> lexeme snakeCase 

constant = do
  _ <- symbol "<"
  name <- lexeme snakeCase
  _ <- symbol ">"

  pure (EConst name)

global = do
  _ <- char '@'
  name <- lexeme snakeCase
  pure (EGlobal name)

operation = undefined

funCall = do
  name <- fName

  _ <- symbol "("
  args <- fArgs
  _ <- symbol ")"

  pure (EFunCall name args)

  where 
    fArgs = 
      expr `sepBy` symbol ","
      <|> pure []

scan = do 
  -- keyword!
  _ <- symbol "scan!"

  _ <- symbol "("
  typε <- Type.typε 
  _ <- symbol ")"

  pure (EScan typε)

simple :: Parser Expr
simple = EConstr <$> cName <*> pure []
-- simple = do
--   tName <- name
--   pure $ TData tName []

complex :: Parser Expr
complex = do
  name <- cName
  _ <- symbol "("
  args <- expr `sepBy` symbol ","
  _ <- symbol ")"

  pure $ EConstr name args

cName, fName :: Parser Text
cName = lexeme pascalCase
fName = lexeme camelCase


{-

pattern TEmpty = TData "TEmpty" []
pattern TOne   = TData "TOne" []
pattern TBool  = TData "TBool" []

pattern TTuple x y = TData "TTuple" [x, y]
pattern TOption x  = TData "TOption" [x]
pattern TList x    = TData "TList" [x]

{-
  TypeDef Patterns:
-}

pattern TDefEmpty = TypeDef "Empty" [] [] 
pattern TDefOne   = TypeDef "One" [] [ConstrDef "O" []]
pattern TDefBool  = TypeDef "Bool" [] [ConstrDef "True" [], ConstrDef "False" []]

pattern TDefTuple  = TypeDef "Tuple" ["a", "b"] [ConstrDef "Tuple" [TVar "a", TVar "b"]]
pattern TDefOption = TypeDef "Option" ["a"] [ConstrDef "None" [], ConstrDef "Some" [TVar "a"]]
pattern TDefList   = TypeDef "List" ["a"] [ConstrDef "Nil" [], 
                              ConstrDef "Cons" [TVar "a", TList (TVar "a")]]

-}