{-# LANGUAGE GADTs #-}

module Grammar.Expr where

import Utils

-- import Data.List.NonEmpty

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
  -- = Not | Neg | Access
  deriving (Eq, Show)

intBinOps, floatBinOps, boolBinOps, listBinOps, badCompareBinOps, bEqOps :: [BinOp]
intBinOps   = [Add, Sub, Mul, Div, Rem]
floatBinOps = [Add_, Sub_, Mul_, Div_]
boolBinOps  = [And, Or]
listBinOps  = [Cat]

badCompareBinOps = [Lt, LEq, GEq, Gt]
bEqOps = [NEq, Eq]

data BinOp
  = Add | Sub | Mul | Div | Rem
  | Add_| Sub_| Mul_| Div_
  | And | Or 
  | Cat 

  | NEq | Eq
  | Lt | LEq | GEq | Gt 
  deriving (Eq, Show)
