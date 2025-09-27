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
  { pImports  :: [SubProgram]
  , pGlobals  :: [GlobalDef]
  , pConsts   :: [ConstDef]
  , pFuns     :: [FunDef]
  , pTypes    :: [TypeDef]
  } deriving (Eq, Show)

-- @global abelha: Int = fib(5)
data GlobalDef =
  Global Name 
  Type Expr
  deriving (Eq, Show)

-- <const> flag = "-hfsh--trace-show special"
data ConstDef =
  Const Name 
  Type Expr
  deriving (Eq, Show)

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

     






