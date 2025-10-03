{-# LANGUAGE GADTs #-}

-- TODO: correct&adjust the auto generated haddock

{-|
Module      : Grammar.Expr
Description : Expression grammar and literals for the Moo programming language
Copyright   : (c) 2025 Moo Language Team
License     : MIT
Maintainer  : joaoduos@gmail.com
Stability   : experimental
Portability : POSIX

This module defines the core expression grammar for the Moo language, including:

* Literals (integers, characters, floats, constructors)
* Expressions (variables, operations, function calls)
* Unary and binary operators with type classification

= Example Usage

@
-- Literal expressions
intLit = ELit (LInt 42)
charLit = ELit (LChar 'a')
listLit = ELit (LConstr "Cons" [LInt 1, LConstr "Nil" []])

-- Binary operations
addition = EBinOp (ELit (LInt 5)) Add (ELit (LInt 3))
comparison = EBinOp (EVar "x") Lt (ELit (LInt 10))

-- Function calls
factorial = EFunCall "fib" [ELit (LInt 5)]
@
-}

module Grammar.Expr where

import Utils

-- import Data.List.NonEmpty

-- l := [1, 2]
-- l := 1::2::[]
-- ELit $ LConstr
-- LConstr "Cons" [1, LConstr "Cons" [2, LConstr "Nil" []]]
-- "True" --> ELit (LConstr "True" [])

-- EConstr
-- LConstr "Cons" [1, LConstr "Cons" [x, LConstr "Nil" []]]

{-|
  All the defined things in the language are literals :P

  Literals represent compile-time constant values in the Moo language.
  They include primitive types and data constructors with their arguments.

  = Examples

  @
  -- Primitive literals
  intLit = LInt 42
  charLit = LChar 'a'
  floatLit = LFloat 3.14

  -- Constructor literals  
  trueLit = LConstr "True" []
  pairLit = LConstr "Pair" [LInt 1, LInt 2]
  listLit = LConstr "Cons" [LInt 1, LConstr "Nil" []]
  @
-}
data Lit
  = LInt   Int    -- ^ Integer literal (e.g., @42@, @-17@)
  | LChar  Char   -- ^ Character literal (e.g., @'a'@, @'\\n'@)
  | LFloat Float  -- ^ Float literal (e.g., @3.14@, @-2.5@)
  | LConstr Name [Lit]  -- ^ Constructor with arguments (e.g., @"Just" [LInt 5]@)
  deriving (Eq, Show)

{-|
  Core expression type for the Moo language.
  
  Expressions represent computations that can be evaluated at runtime.
  They include literals, variables, operations, and function calls.

  = Examples

  @
  -- Simple expressions
  constant = ELit (LInt 42)
  variable = EVar "x"
  global = EGlobal "PI"

  -- Operations
  negation = EUnOp Neg (EVar "x")
  addition = EBinOp (EVar "a") Add (EVar "b")
  
  -- Function call
  factorial = EFunCall "factorial" [ELit (LInt 5)]
  @
-}
data Expr
  = ELit Lit                    -- ^ Literal value
  | EConstr Name [Expr]         -- ^ Constructor application with expressions
  | EVar Name                   -- ^ Local variable reference
  | EConst Name                 -- ^ Named constant reference  
  | EGlobal Name                -- ^ Global variable reference -- optimal
  | EUnOp UnOp Expr            -- ^ Unary operation
  | EBinOp Expr BinOp Expr     -- ^ Binary operation
  | EFunCall Name [Expr]       -- ^ Function call with arguments
  deriving (Eq, Show)

{-|
  Unary operators in the Moo language.
  
  Currently supports arithmetic negation and boolean negation.

  = Examples

  @
  negativeNumber = EUnOp Neg (ELit (LInt 5))    -- -5
  booleanNot = EUnOp Not (EVar "flag")          -- !flag
  @
-}
data UnOp 
  = Neg  -- ^ Arithmetic negation (@-x@)
  | Not  -- ^ Boolean negation (@!x@)
  -- = Not | Neg | Access
  deriving (Eq, Show)

{-| Integer arithmetic operators: @+@, @-@, @*@, @/@, @%@ -}
intBinOps :: [BinOp]
intBinOps   = [Add, Sub, Mul, Div, Rem]

{-| Float arithmetic operators: @+.@, @-.@, @*.@, @/.@ -}
floatBinOps :: [BinOp]
floatBinOps = [Add_, Sub_, Mul_, Div_]

{-| Boolean logical operators: @&&@, @||@ -}
boolBinOps :: [BinOp]
boolBinOps  = [And, Or]

{-| List concatenation operator: @++@ -}
listBinOps :: [BinOp]
listBinOps  = [Cat]

{-| Comparison operators (excluding equality): @<@, @<=@, @>=@, @>@ -}
badCompareBinOps :: [BinOp]
badCompareBinOps = [Lt, LEq, GEq, Gt]

{-| Equality operators: @==@, @/=@ -}
bEqOps :: [BinOp]
bEqOps = [NEq, Eq]

{-|
  Binary operators in the Moo language.
  
  Operators are categorized by type:
  
  * Integer arithmetic: 'Add', 'Sub', 'Mul', 'Div', 'Rem'
  * Float arithmetic: 'Add_', 'Sub_', 'Mul_', 'Div_'  
  * Boolean logic: 'And', 'Or'
  * List operations: 'Cat'
  * Comparisons: 'Lt', 'LEq', 'GEq', 'Gt'
  * Equality: 'Eq', 'NEq'

  = Examples

  @
  -- Arithmetic
  addition = EBinOp (ELit (LInt 5)) Add (ELit (LInt 3))
  
  -- Comparison  
  lessThan = EBinOp (EVar "x") Lt (ELit (LInt 10))
  
  -- Boolean logic
  andExpr = EBinOp (EVar "a") And (EVar "b")
  @
-}
data BinOp
  = Add | Sub | Mul | Div | Rem  -- ^ Integer arithmetic operators
  | Add_| Sub_| Mul_| Div_       -- ^ Float arithmetic operators  
  | And | Or                     -- ^ Boolean logical operators
  | Cat                          -- ^ List concatenation operator

  | NEq | Eq                     -- ^ Equality operators
  | Lt | LEq | GEq | Gt          -- ^ Comparison operators
  deriving (Eq, Show)