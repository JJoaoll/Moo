{-# LANGUAGE GADTs #-}

module Grammar.Expr where

import Utils

-- import Data.List.NonEmpty

-- l = [1, 2]
-- l = 1::2::[]
-- ELit $ LConstr
-- LConstr "Cons" [1, LConstr "Cons" [2, LConstr "Nil" []]]
-- "True" --> ELit (LConstr "True" [])

-- EConstr
-- LConstr "Cons" [1, LConstr "Cons" [x, LConstr "Nil" []]]

data Lit
  = LInt   Int 
  | LChar  Char
  | LFloat Float
  | LConstr Name [Lit]
  deriving (Eq, Show)

data Expr
  = ELit Lit | EConstr Name [Expr]
  | EVar Name | EConst Name | EGlobal Name -- optimal
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