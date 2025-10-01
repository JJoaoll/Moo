{-# LANGUAGE GADTs #-}

module Grammar.Program where 

import Utils

import Grammar.Type
import Grammar.Expr
import Grammar.Sttm 

-- TODO: missing records && lens


type Params = [Param]

-- import ./Code (implicit ".meh"?)
type SubProgram = Program

data Program = Program
  { --{ pImports  :: [SubProgram] -- the parser should expand that!
    pGlobals :: [GlobalDef]
  , pConsts  :: [ConstDef]
  , pFuns    :: [FunDef]
  , pTypes   :: [TypeDef]
  } deriving (Eq, Show)

-- @global abelha: Int = fib(5)
data GlobalDef = Global 
  { gName :: Name
  , gExpr :: Expr 
  , gType :: Type 
  } deriving (Eq, Show)

-- <const> flag = "-hfsh--trace-show special"
data ConstDef = Const 
  { kName :: Name
  , kType :: Type
  , kVal  :: Expr -- SHOULD BE A LITERAL
  } deriving (Eq, Show)

data Param = Param
  { pName :: Name
  , pType :: Type
  } deriving (Eq, Show)

data FunDef = FunDef 
  { fName   :: Name
  , fParams :: Params
  , rtrType :: Type
  , body    :: Sttms } 
  deriving (Eq, Show)

     






