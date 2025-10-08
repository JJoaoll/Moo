{-# LANGUAGE OverloadedStrings #-}

module Parser.Expr where

import Grammar.Expr

import Text.Megaparsec 
import Text.Megaparsec.Char
-- import Grammar.Type
import Data.Text 

import Parser.Utils.Utils 
import Parser.Utils.Cases
import qualified Parser.Expr.Lit as Lit (literal)
-- import Control.Monad (when)

-- @global abelha: Int := fib(5)
-- | Global variable definition
--
import qualified Parser.Type as Type 
import Control.Monad.Combinators.Expr

-- Main expression parser: term | unOp term | term binOp term (with precedence)
expr :: Parser Expr
expr = makeExprParser term operatorTable

-- Term parser: atomic expressions without operators
term :: Parser Expr
term = choice $ try <$>
  [ parens expr         <?> "parenthesized expression"
  , constructor         <?> "constructor"
  , scan                <?> "scan!"   -- not think i'm a fun!
  , funCall             <?> "fun call"
  , literal             <?> "literal" -- a empty constructor would always match
  , variable            <?> "variable"
  , constant            <?> "constant"
  , global              <?> "global"
  ] 

literal, constructor, variable, constant, global, funCall, scan :: Parser Expr

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

funCall = do
  name <- fName                    <?> "fun name"

  _ <- symbol "("                  <?> "openning parens"
  args <- expr `sepBy` symbol ","  <?> "fun args"
  _ <- symbol ")"                  <?> "closing parens"

  pure (EFunCall name args)


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
  args <- expr `sepBy` symbol "," <?> "constructor args"
  _ <- symbol ")"                 

  pure $ EConstr name args

cName, fName :: Parser Text
cName = lexeme pascalCase
fName = lexeme camelCase

----------------------------BEGIN-OPERATIONS--------------------------------

-- Operator precedence table: defines how expr combines terms with operators
-- Structure: expr ::= term | unOp term | term binOp term (respecting precedence)

-- Operator table with precedence and associativity
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ -- Level 7: Unary (highest precedence)
    [ Prefix (EUnOp Neg <$ symbol "-")
    , Prefix (EUnOp Not <$ symbol "not")
    ]

  , -- Level 6: Multiplication, Division (multi-char operators first!)
    [ InfixL ((\x y -> EBinOp x Mul_ y) <$ symbol "*.")
    , InfixL ((\x y -> EBinOp x Div_ y) <$ symbol "/.")
    , InfixL ((\x y -> EBinOp x Mul y) <$ symbol "*")
    , InfixL ((\x y -> EBinOp x Div y) <$ try (symbol "/" <* notFollowedBy (char '=')))
    , InfixL ((\x y -> EBinOp x Rem y) <$ symbol "%")
    ]

  , -- Level 5: Concatenation (higher precedence than +)
    [ InfixR ((\x y -> EBinOp x Cat y) <$ symbol "++")
    ]

  , -- Level 4: Addition, Subtraction (multi-char operators first!)
    [ InfixL ((\x y -> EBinOp x Add_ y) <$ symbol "+.")
    , InfixL ((\x y -> EBinOp x Sub_ y) <$ symbol "-.")
    , InfixL ((\x y -> EBinOp x Add y) <$ symbol "+")
    , InfixL ((\x y -> EBinOp x Sub y) <$ symbol "-")
    ]

  , -- Level 3: Comparison
    [ InfixN ((\x y -> EBinOp x LEq y) <$ symbol "<=")
    , InfixN ((\x y -> EBinOp x GEq y) <$ symbol ">=")
    , InfixN ((\x y -> EBinOp x Lt y) <$ symbol "<")
    , InfixN ((\x y -> EBinOp x Gt y) <$ symbol ">")
    ]

  , -- Level 2: Equality (multi-char first!)
    [ InfixN ((\x y -> EBinOp x NEq y) <$ symbol "/=")
    , InfixN ((\x y -> EBinOp x Eq y) <$ symbol "==")
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

-----------------------------END-EXPR--------------------------------------