module Analyser.Error where

import Data.Text

type Msg = Text

data Ok 
  = Ok deriving (Eq, Show)

data Error 
  = Error 
  | VarNotFound Msg
  | FunNotFound Msg
  | IncorrectArity Msg Int Int -- FunName Expected Got
  | PatternNotFound Msg 
  deriving (Eq, Show)

{-

[1,2,3]
LConstr "Cons" 
  [1, LCostr "Cons" 
  [2, LConstr 
  [3, LConstr "Nil" []]]]

-}