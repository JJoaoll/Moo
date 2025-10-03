{-# LANGUAGE GADTs #-}

{-|
Module      : Grammar.Program
Description : Core AST definitions for the Moo programming language
Copyright   : (c) 2025 Moo Language Team
License     : MIT
Maintainer  : joaoduos@gmail.com
Stability   : experimental
Portability : POSIX

This module defines the core Abstract Syntax Tree (AST) structures for the Moo programming language.
It includes definitions for programs, functions, global variables, constants, and parameters.

=== Example Usage

@
-- A simple program with a main function
program = Program
  { pGlobals = [Global "x" (ELit (LInt 42)) TInt]
  , pConsts  = [Const "PI" TFloat (ELit (LFloat 3.14))]
  , pFuns    = [main]
  , pTypes   = []
  }
  where
    main = FunDef "main" [] TInt [SReturn (ELit (Lit 0))]
@
-}

module Grammar.Program where 

import Utils

import Grammar.Type
import Grammar.Expr
import Grammar.Sttm 

-- | List of function parameters
type Params = [Param]


-- | Top-level program structure containing all definitions
--
-- A program consists of:
--
-- * Global variable definitions ('pGlobals')
-- * Constant definitions ('pConsts')
-- * Function definitions ('pFuns')
-- * Type definitions ('pTypes')
data Program = Program
  { pGlobals :: [GlobalDef]  -- ^ Global variable definitions
  , pConsts  :: [ConstDef]   -- ^ Constant definitions
  , pFuns    :: [FunDef]     -- ^ Function definitions
  , pTypes   :: [TypeDef]    -- ^ Type definitions
  } deriving (Eq, Show)

-- @global abelha: Int := fib(5)
-- | Global variable definition
--
-- Syntax: @global _varName_ : _TypeName_ := _expr_
--
-- === Example
-- @
-- Global "abelha" TInt (EFunCall "fib" [ELit (LInt 5)]) 
-- @
--
-- Represents: @global abelha: Int := fib(5)@
data GlobalDef = Global 
  { gName :: Name  -- ^ Variable name
  , gType :: Type  -- ^ Variable type
  , gExpr :: Expr  -- ^ Initialization expression
  } deriving (Eq, Show)

-- <const> flag := "-hfsh--trace-show special"
-- | Constant definition (must be a literal value)
--
-- Syntax: <const> _constName_ := _litExpr_
--
-- === Example
-- @
-- Const "flag" TString (ELit (LString "-hfsh--trace-show special"))
-- @
-- Represents: @const flag := "-hfsh--trace-show special"@
data ConstDef = Const 
  { kName :: Name  -- ^ Constant name
  , kType :: Type  -- ^ Constant type
  , kVal  :: Expr  -- ^ Constant value -- SHOULD BE A LITERAL
  } deriving (Eq, Show)

-- | Function parameter
--
-- === Example
-- @
-- Param "x" TInt
-- @
-- Represents parameter: @x: Int@
data Param = Param
  { pName :: Name  -- ^ Parameter name
  , pType :: Type  -- ^ Parameter type
  } deriving (Eq, Show)

-- | Function definition
--
-- === Example
-- @
-- FunDef "fibonacci" [Param "n" TInt] TInt 
--   [ SMatch (EVar "n") 
--     [ (PLit (LInt 0), [SReturn (ELit (LInt 1))])
--     , (PLit (LInt 1), [SReturn (ELit (LInt 1))])
--     , (PVar "n", [SReturn (EBinOp 
--         (EFunCall "fibonacci" [EBinOp (EVar "n") Sub (ELit (LInt 1))])
--         Add
--         (EFunCall "fibonacci" [EBinOp (EVar "n") Sub (ELit (LInt 2))]))])
--     ]
--   ]
-- @
data FunDef = FunDef 
  { fName   :: Name    -- ^ Function name
  , fParams :: Params  -- ^ Function parameters
  , rtrType :: Type    -- ^ Return type
  , body    :: Sttms   -- ^ Function body (list of statements)
  } deriving (Eq, Show)

     






