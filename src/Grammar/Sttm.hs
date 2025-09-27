{-# LANGUAGE GADTs #-}

module Grammar.Sttm where

import Grammar.Expr
import Grammar.Type

import Utils

type Sttms = [Sttm]
data Sttm
  = SInit Name Type Expr
  | SAtrib Name Expr
  | SPrint Expr
  | SScan Type
  | SFunCall Name [Expr]
  | SMatch Expr [(Pattern, Sttms)]
  | SWhile Expr Sttms
  | SFor Name Expr Sttms
  | SReturn Expr
  deriving (Eq, Show)


data Pattern
  = PLit Lit
  | PVar Name | PWildcard
  | PConstructor Name [Pattern]
  -- | Special (num signs and more..)
  deriving (Eq, Show)



  

