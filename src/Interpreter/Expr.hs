module Interpreter.Expr where

import Grammar.Expr
import Interpreter.InterpretT

evalExpr :: Expr -> InterpretT Value
evalExpr = undefined