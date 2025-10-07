{-# LANGUAGE OverloadedStrings #-}

{-|
Module: Parser.Expr.Op
Description: Operator parsers for unary and binary operations

This module provides parsers for operators in the Moo language.

Main exports:
  - exprWithOps: Complete expression parser with correct operator precedence
  - operatorTable: Precedence table for makeExprParser
  - unOp, binOp: Simplified operator parsers (for testing/legacy)

Usage in Parser.Expr:
  Define 'term' to parse non-operator expressions, then use:
  expr = exprWithOps
  where term = choice [literal, variable, constant, global, funCall, scan, parens exprWithOps]
-}

module Parser.Expr.Op where

import Data.Text hiding (empty)
import Text.Megaparsec
import Control.Monad.Combinators.Expr
import Grammar.Expr

import Parser.Utils.Utils

-- Type signatures
term :: Parser Expr
unOp, binOp :: Parser Expr
unOperator  :: Parser UnOp
binOperator :: Parser BinOp
exprWithOps :: Parser Expr

-- Main expression parser with correct operator precedence
exprWithOps = makeExprParser term operatorTable

-- Unary operators
unOp = do
  operator <- unOperator
  EUnOp operator <$> term 

-- Binary operators (simplified - doesn't handle precedence correctly)
-- For correct precedence, use 'exprWithOps' which uses makeExprParser
binOp = do
  left <- term
  operator <- binOperator
  EBinOp left operator <$> term

-- Parser for unary operators
unOperator = choice
  [ Neg <$ symbol "-"
  , Not <$ symbol "not"
  ]

-- Parser for individual binary operators
binOperator = choice
  -- Integer arithmetic
  [ Add <$ symbol "+"
  , Sub <$ symbol "-"
  , Mul <$ symbol "*"
  , Div <$ symbol "/"
  , Rem <$ symbol "%"

  -- Float arithmetic (with dot)
  , Add_ <$ symbol "+."
  , Sub_ <$ symbol "-."
  , Mul_ <$ symbol "*."
  , Div_ <$ symbol "/."

  -- Boolean logic
  , And <$ symbol "and"
  , Or  <$ symbol "or"

  -- List concatenation
  , Cat <$ symbol "++"

  -- Equality (try 2-char operators first)
  , NEq <$ symbol "/="
  , Eq  <$ symbol "=="

  -- Comparison (try 2-char operators first)
  , LEq <$ symbol "<="
  , GEq <$ symbol ">="
  , Lt  <$ symbol "<"
  , Gt  <$ symbol ">"
  ]

-- Basic term parser (expressions without operators)
-- NOTE: This needs to be imported from Parser.Expr to avoid circular dependency
-- For now, this is a placeholder that should be replaced when integrating
term = undefined  -- TODO: Import from Parser.Expr or define here
  -- Should parse: 
  -- - literal (numbers, chars, floats, constructors)
  -- - variable (identifiers)
  -- - constant (<name>)
  -- - global (@name)
  -- - funCall (name(...))
  -- - scan! (scan!(Type))
  -- - parenthesized expressions: (exprWithOps)

-- ============================================================
-- ALTERNATIVE: Using makeExprParser for correct precedence
-- ============================================================

-- Operator table with precedence and associativity
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ -- Level 7: Unary (highest precedence)
    [ Prefix (EUnOp Neg <$ symbol "-")
    , Prefix (EUnOp Not <$ symbol "not")
    ]

  , -- Level 6: Multiplication, Division
    [ InfixL ((\x y -> EBinOp x Mul y) <$ symbol "*")
    , InfixL ((\x y -> EBinOp x Div y) <$ symbol "/")
    , InfixL ((\x y -> EBinOp x Rem y) <$ symbol "%")
    , InfixL ((\x y -> EBinOp x Mul_ y) <$ symbol "*.")
    , InfixL ((\x y -> EBinOp x Div_ y) <$ symbol "/.")
    ]

  , -- Level 5: Addition, Subtraction
    [ InfixL ((\x y -> EBinOp x Add y) <$ symbol "+")
    , InfixL ((\x y -> EBinOp x Sub y) <$ symbol "-")
    , InfixL ((\x y -> EBinOp x Add_ y) <$ symbol "+.")
    , InfixL ((\x y -> EBinOp x Sub_ y) <$ symbol "-.")
    ]

  , -- Level 4: Concatenation
    [ InfixR ((\x y -> EBinOp x Cat y) <$ symbol "++")
    ]

  , -- Level 3: Comparison
    [ InfixN ((\x y -> EBinOp x LEq y) <$ symbol "<=")
    , InfixN ((\x y -> EBinOp x GEq y) <$ symbol ">=")
    , InfixN ((\x y -> EBinOp x Lt y) <$ symbol "<")
    , InfixN ((\x y -> EBinOp x Gt y) <$ symbol ">")
    ]

  , -- Level 2: Equality
    [ InfixN ((\x y -> EBinOp x Eq y) <$ symbol "==")
    , InfixN ((\x y -> EBinOp x NEq y) <$ symbol "/=")
    ]

  , -- Level 1: Logical (lowest precedence)
    [ InfixL ((\x y -> EBinOp x And y) <$ symbol "and")
    , InfixL ((\x y -> EBinOp x Or y) <$ symbol "or")
    ]
  ]

-- Helper to create infix binary operators
binary :: Text -> BinOp -> Operator Parser Expr
binary name op = InfixL ((\x y -> EBinOp x op y) <$ symbol name)

-- ============================================================
-- RECOMMENDED USAGE:
-- ============================================================
-- To parse expressions with operators, use makeExprParser:
--
-- exprWithOps :: Parser Expr
-- exprWithOps = makeExprParser term operatorTable
--
-- Where 'term' parses basic terms (literals, variables, parentheses, etc)
--
-- Complete example:
-- term :: Parser Expr
-- term = choice
--   [ parens exprWithOps  -- expressions in parentheses
--   , literal             -- literals
--   , variable            -- variables
--   , funCall             -- function calls
--   ]
--
-- Precedence (from highest to lowest):
-- 7. Unary: -, not
-- 6. Multiplication/Division: *, /, %, *., /.
-- 5. Addition/Subtraction: +, -, +., -.
-- 4. Concatenation: ++
-- 3. Comparison: <, <=, >, >=
-- 2. Equality: ==, /=
-- 1. Logical: and, or
--
-- Associativity:
-- - InfixL: Left-associative (a + b + c = (a + b) + c)
-- - InfixR: Right-associative (a ++ b ++ c = a ++ (b ++ c))
-- - InfixN: Non-associative (a < b < c is an error)
-- - Prefix: Prefix unary operators (-x, not flag)


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
