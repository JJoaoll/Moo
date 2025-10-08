{-# Language PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Grammar.Type
Description : Type system definitions for the Moo programming language
Copyright   : (c) 2025 Moo Language Team
License     : MIT
Maintainer  : joaoduos@gmail.com
Stability   : experimental
Portability : POSIX

This module defines the complete type system for Moo, including:

* Basic primitive types (Int, Char, Float)
* Type variables for polymorphism
* Parametric data types with constructors
* Built-in type patterns with convenient syntax
* Type definitions for user-defined types

= Example Usage

@
-- Basic types
intType = TInt
charType = TChar
floatType = TFloat

-- Parametric types
listOfInts = TList TInt
optionalBool = TOption TBool
stringIntPair = TTuple TChar TInt

-- Custom type definition
treeDef = TypeDef "Tree" ["a"] 
  [ ConstrDef "Leaf" [TVar "a"]
  , ConstrDef "Node" [TData "Tree" [TVar "a"], TVar "a", TData "Tree" [TVar "a"]]
  ]
@
-}

module Grammar.Type where 

import Utils

{-|
  Core type system for the Moo language.
  
  Types can be primitive, parametric, or type variables for polymorphism.
  Type variables are primarily used in type definitions and should be 
  resolved during type checking.

  = Examples

  @
  -- Primitive types
  intType = TInt
  charType = TChar  
  floatType = TFloat
  
  -- Type variable (for polymorphism)
  genericType = TVar "a"
  
  -- Parametric types
  listType = TData "List" [TInt]
  treeType = TData "Tree" [TVar "a", TVar "b"]
  @
-}
data Type
  = TInt                -- ^ Integer type
  | TChar               -- ^ Character type
  | TFloat              -- ^ Floating-point type
  | TVar Name           -- ^ Type variable -- .. Only in typedefs, please?
  | TData Name [Type]   -- ^ Parametric data type with arguments
  deriving (Eq, Show)

-- | Built-in type patterns for common types
pattern TEmpty, TOne, TBool :: Type

-- | Tuple type constructor pattern  
pattern TTuple :: Type -> Type -> Type

-- | Container type patterns
pattern TOption, TList :: Type -> Type

{-|
  User-defined type with constructors and type parameters.
  
  Type definitions allow creating new algebraic data types with
  multiple constructors and polymorphic type parameters.

  = Examples

  @
  -- Simple enumeration
  boolDef = TypeDef "Bool" [] 
    [ ConstrDef "True" []
    , ConstrDef "False" []
    ]
    
  -- Polymorphic option type  
  optionDef = TypeDef "Option" ["a"]
    [ ConstrDef "None" []
    , ConstrDef "Some" [TVar "a"]
    ]
  @
-}
data TypeDef = TypeDef
  { tName    :: Name         -- ^ Type name
  , tParams  :: [Name]       -- ^ Type parameters
  , tConstrs :: [ConstrDef]  -- ^ Data constructors
  } deriving (Eq, Show)

-- | Pattern synonyms for predefined type definitions
pattern TDefEmpty, TDefOne, TDefBool, TDefOption, TDefTuple, TDefList :: TypeDef

{-|
  Data constructor definition with parameters.
  
  Constructors define how values of a type can be created,
  specifying the types of arguments they accept.

  = Examples

  @
  -- Constructor with no arguments
  nilConstr = ConstrDef "Nil" []
  
  -- Constructor with arguments  
  consConstr = ConstrDef "Cons" [TVar "a", TList (TVar "a")]
  pairConstr = ConstrDef "Pair" [TVar "a", TVar "b"]
  @
-}
data ConstrDef = ConstrDef 
  { cName :: Name      -- ^ Constructor name
  , cParams :: [Type]  -- ^ Constructor parameter types
  } deriving (Eq, Show)

{-
  Type Patterns:
-}

-- | Empty type (no values)
pattern TEmpty = TData "Empty" []
-- | Unit type (single value)  
pattern TOne   = TData "One" []
-- | Boolean type
pattern TBool  = TData "Bool" []

-- | Tuple type constructor (product type)
pattern TTuple x y = TData "Tuple" [x, y]
-- | Optional type constructor (sum type)
pattern TOption x  = TData "Option" [x]
-- | List type constructor (recursive type)
pattern TList x    = TData "List" [x]

{-
  TypeDef Patterns:
-}

-- | Empty type definition (no constructors, uninhabited type)
pattern TDefEmpty = TypeDef "Empty" [] [] 
-- | Unit type definition (single constructor, single value)
pattern TDefOne   = TypeDef "One" [] [ConstrDef "O" []]
-- | Boolean type definition (two constructors for True/False)
pattern TDefBool  = TypeDef "Bool" [] [ConstrDef "True" [], ConstrDef "False" []]

-- | Tuple type definition (product of two types)
pattern TDefTuple  = TypeDef "Tuple" ["a", "b"] [ConstrDef "Tuple" [TVar "a", TVar "b"]]
-- | Optional type definition (None or Some value)
pattern TDefOption = TypeDef "Option" ["a"] [ConstrDef "None" [], ConstrDef "Some" [TVar "a"]]
-- | List type definition (empty list or cons cell)
pattern TDefList   = TypeDef "List" ["a"] [ConstrDef "Nil" [], 
                              ConstrDef "Cons" [TVar "a", TList (TVar "a")]]
