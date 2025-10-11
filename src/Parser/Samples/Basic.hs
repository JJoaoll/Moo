{-# LANGUAGE OverloadedStrings #-}

module Parser.Samples.Basic where

import Data.Text hiding (empty, elem)
import Text.Megaparsec
import Grammar.Expr 

import Parser.Utils.Utils
import Parser.Samples.Ids
import Parser.Utils.Cases (kebabCase)

import Control.Monad (void)

reservedWords = []

run :: Text -> IO Expr
run input = 
  case parse pExpr "" input of
    Left _bundle -> error "erro!!" -- putStr (errorBundlePretty bundle)
    Right expr  -> return expr

pExpr :: Parser Expr 
pExpr = choice
  [ pLit
  , pVar
  , pConst 
  , pGlobal
  , pUnOp
  , pBinOp
  , pFunCall]

pLit, pVar, pConst, pGlobal, pUnOp, pBinOp, pFunCall :: Parser Expr
pLit = undefined
pVar = do
  varName <- varId
  if varName `elem` reservedWords then
    empty
  else
    return (EVar varName)

pConst = EConst <$> constId

-- EGlobal inside the result value of varId read(red) after the symbol "@"
pGlobal = EGlobal <$> varId <* symbol "@"

pUnOp = undefined

pBinOp = undefined

pFunCall = do
  fName <- funId
  void $ symbol "("
  fArgs <- pArgs <?> "function arguments"
  void $ symbol ")"

  return (EFunCall fName fArgs)

pArgs :: Parser [Expr]
pArgs = try $ do
  e <- pExpr
  es <- many $ do
    void $ symbol ","
    pExpr
  
  return (e:es)
  <|> pure []

  

{- Expressions:
 |  46               |   Literal
 |  abc              |   Variable
 |  <pi>             |   Const
 |  @cabrita         |   Global
 |  not True         |   Un-Op
 |  35 + length(xs)  |   Bin-Op
-}

{- 
data Lit
  = LInt   Int 
  | LChar  Char
  | LFloat Float
  deriving (Eq, Show)

data Expr
  = ELit Lit
  | EVar Name | Const Name | Global Name -- optimal
  | EUnOp UnOp Expr | EBinOp Expr BinOp Expr
  | EFunCall Name [Expr]
  deriving (Eq, Show)

data UnOp 
  = Neg
  | Not
  deriving (Eq, Show)

data BinOp
  = Add | Sub | Mul | Div | Rem
  | Add_| Sub_| Mul_| Div_
  | And | Or 
  | Cat 

  | NEq | Eq
  | Lt | LEq | GEq | Gt 
  deriving (Eq, Show)
-}

-- pVarId, pFunId, pTypeId, pConstrId, pGlobalId, pConstId :: Parser Expr
