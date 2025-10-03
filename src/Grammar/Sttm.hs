{-# LANGUAGE GADTs #-}

{-|
Module      : Grammar.Sttm
Description : Statement and pattern matching definitions for the Moo programming language
Copyright   : (c) 2025 Moo Language Team
License     : MIT
Maintainer  : joaoduos@gmail.com
Stability   : experimental
Portability : POSIX

This module defines statements (commands) and pattern matching constructs for Moo.

= Statement Types

* Variable operations: declaration ('SInit'), assignment ('SAtrib')
* I/O operations: output ('SPrint'), input ('SScan')
* Control flow: pattern matching ('SMatch'), loops ('SWhile', 'SFor'), returns ('SReturn')
* Function calls: procedure calls ('SFunCall')

= Pattern Matching

Patterns are used in 'SMatch' statements for destructuring and conditional execution:

* 'PLit': Match specific literal values
* 'PVar': Bind variables to matched values  
* 'PWildcard': Match anything without binding
* 'PConstructor': Destructure constructor applications recursively

= Example Usage

@
-- Variable declaration and assignment  
initStmt = SInit "x" TInt (ELit (LInt 42))    -- let x: Int := 42
assignStmt = SAtrib "x" (EBinOp (EVar "x") Add (ELit (LInt 1)))  -- x := x + 1

-- Pattern matching on option type
matchStmt = SMatch (EVar "result")
  [ (PConstructor "None" [], [SPrint (ELit (LConstr "Error" []))])
  , (PConstructor "Some" [PVar "value"], [SPrint (EVar "value")])
  ]

-- Loop constructs  
whileStmt = SWhile (EBinOp (EVar "i") Lt (ELit (LInt 10))) 
  [SAtrib "i" (EBinOp (EVar "i") Add (ELit (LInt 1)))]  -- i := i + 1
  
forStmt = SFor "item" (EVar "items") [SPrint (EVar "item")]
@
-}

module Grammar.Sttm where

import Grammar.Expr
import Grammar.Type

import Utils

-- | A list of statements forming a command sequence.
-- Statements are executed sequentially within a block.
type Sttms = [Sttm]

-- | Statement data type representing all executable commands in Moo.
--
-- Statements represent imperative actions that modify program state:
--
-- [@SInit@] Variable declaration with initial value:
--   'SInit' \"x\" 'TInt' ('ELit' ('LInt' 42))  -- represents: let x: Int := 42
--
-- [@SAtrib@] Variable assignment (mutation):
--   'SAtrib' \"x\" ('EBinOp' ('EVar' \"x\") 'Add' ('ELit' ('LInt' 1)))  -- represents: x := x + 1
--
-- [@SPrint@] Output expression value to console:
--   'SPrint' ('ELit' ('LString' \"Hello, World!\"))
--
-- [@SScan@] Read input value of specified type:
--   'SScan' 'TString' 
--
-- [@SFunCall@] Call procedure (function returning unit):
--   'SFunCall' \"println\" ['ELit' ('LString' \"debug\")]
--
-- [@SMatch@] Pattern matching with multiple cases:
--   'SMatch' ('EVar' \"option\") [('PConstructor' \"Some\" ['PVar' \"x\"], ['SPrint' ('EVar' \"x\")])]
--
-- [@SWhile@] While loop with condition and body:
--   'SWhile' ('EBinOp' ('EVar' \"i\") 'Lt' ('ELit' ('LInt' 10))) ['SAtrib' \"i\" ...]
--
-- [@SFor@] For-each loop over iterable:
--   'SFor' \"item\" ('EVar' \"list\") ['SPrint' ('EVar' \"item\")]
--
-- [@SReturn@] Early function return with value:
--   'SReturn' ('ELit' ('LInt' 0))
data Sttm
  = SInit Name Type Expr      -- ^ Variable declaration: name, type, initial value
  | SAtrib Name Expr          -- ^ Variable assignment: name, new value  
  | SPrint Expr               -- ^ Print expression to output
  | SScan Type                -- ^ Scan input of given type
  | SFunCall Name [Expr]      -- ^ Function call: name, arguments
  | SMatch Expr [(Pattern, Sttms)]  -- ^ Pattern match: expression, cases
  | SWhile Expr Sttms         -- ^ While loop: condition, body statements
  | SFor Name Expr Sttms      -- ^ For loop: variable, iterable, body statements
  | SReturn Expr              -- ^ Return statement with value
  deriving (Eq, Show)


-- | Pattern data type for destructuring values in pattern matching.
--
-- Patterns allow extracting components from structured data:
--
-- [@PLit@] Match exact literal value:
--   'PLit' ('LInt' 42) matches only the integer 42
--   'PLit' ('LString' \"hello\") matches only the string \"hello\"
--
-- [@PVar@] Bind variable to matched value:
--   'PVar' \"x\" binds the matched value to variable \"x\" in scope
--
-- [@PWildcard@] Match anything without binding:
--   'PWildcard' accepts any value but doesn't create bindings
--
-- [@PConstructor@] Match constructor with nested patterns:
--   'PConstructor' \"Some\" ['PVar' \"x\"] matches 'Some' constructor,
--   binding inner value to \"x\"
--   'PConstructor' \"Cons\" ['PVar' \"head\", 'PVar' \"tail\"] matches
--   list constructor, binding head and tail elements
--
-- = Pattern Examples
--
-- @
-- -- Match option types
-- somePattern = PConstructor \"Some\" [PVar \"value\"]
-- nonePattern = PConstructor \"None\" []
--
-- -- Match list patterns  
-- emptyPattern = PConstructor \"Nil\" []
-- consPattern = PConstructor \"Cons\" [PVar \"x\", PWildcard]
--
-- -- Match literals
-- zeroPattern = PLit (LInt 0)
-- truePattern = PLit (LBool True)
-- @
data Pattern
  = PLit Lit                  -- ^ Match literal value exactly
  | PVar Name                 -- ^ Bind variable to matched value
  | PWildcard                 -- ^ Match anything without binding
  | PConstructor Name [Pattern] -- ^ Match constructor with nested patterns
  -- | Special (num signs and more..)
  deriving (Eq, Show)



  

