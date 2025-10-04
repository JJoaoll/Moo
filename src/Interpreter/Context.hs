{-# LANGUAGE TemplateHaskell #-}

module Interpreter.Context where

import Grammar.Type
import Grammar.Program

import Control.Lens hiding (Context)

import Data.Text
import Utils

data Value 
  = VInt    Int
  | VChar   Char
  | VFloat  Float
  -- some day..
  -- | VString String
  -- | VOption (Maybe Value)
  -- | VList   [Value] -- not garantee that they'll have the same type by types :c
  | VData Name [Value] -- Lisp
  deriving (Eq, Show)

-- declarated Var
data DVar = DVar
  { _vName :: Text
  , _vType :: Type
  , _vVal  :: Value
  } deriving (Eq, Show)

$(makeLenses ''DVar)

data Context = Context
  { _cTypes :: [TypeDef]
  , _cFuncs :: [FunDef]
  , _cGlobs :: [DVar]   -- it needs to be preprocessed!!!!!
  , _cCnsts :: [DVar]   -- the dream is remove it
  , _cStack :: [[DVar]] -- positions represents the scope level.
  , _cScope :: Int
  } deriving (Eq, Show)

$(makeLenses ''Context)
