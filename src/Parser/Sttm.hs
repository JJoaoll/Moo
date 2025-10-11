{-# LANGUAGE OverloadedStrings #-}

module Parser.Sttm where


import Text.Megaparsec
import Grammar.Sttm

import Parser.Utils.Utils
import Parser.Samples.Ids

import Control.Monad (void)
import Parser.Samples.Basic

-- data Sttm
--   = SInit Name Type Expr
--   | SAtrib Name Expr
--   | SPrint Expr
--   | SScan Type
--   | SFunCall Name [Expr]
--   | SMatch Expr [(Pattern, Sttms)]
--   | SWhile Expr Sttms
--   | SFor Name Expr Sttms
--   | SReturn Expr
--   deriving (Eq, Show)

pAtrib :: Parser Sttm
pAtrib = do
  name <- varId
  void $ symbol "="
  expr <- pExpr

  return (SAtrib name expr)




pSttm = choice
  [ pInit
  , pAtrib
  , pPrint
  , pScan
  , pFunCall
  , pMatch
  , pWhile
  , pFor
  , pReturn ]
