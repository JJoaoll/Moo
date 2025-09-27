{-# Language PatternSynonyms #-}
-- {-# LANGUAGE DuplicateRecordFields #-}

module Grammar.Type where 

import Utils

data Type
  = TInt
  | TChar
  | TFloat
  | TVar Name -- .. Only in typedefs, please?
  | TData Name [Type]
  deriving (Eq, Show)

pattern TEmpty, TOne, TBool :: Type
pattern TTuple :: Type -> Type -> Type
pattern TOption, TList :: Type -> Type

data TypeDef = TypeDef
  { tName    :: Name
  , tParams  :: [Name]
  , tConstrs :: [ConstrDef]
  } deriving (Eq, Show)

pattern TDefEmpty, TDefOne, TDefBool, TDefOption, TDefTuple, TDefList :: TypeDef

data ConstrDef = ConstrDef 
  { cName :: Name 
  , cParams :: [Type]
  } deriving (Eq, Show)

{-
  Type Patterns:
-}

pattern TEmpty = TData "TEmpty" []
pattern TOne   = TData "TOne" []
pattern TBool  = TData "TBool" []

pattern TTuple x y = TData "TTuple" [x, y]
pattern TOption x  = TData "TOption" [x]
pattern TList x    = TData "TList" [x]

{-
  TypeDef Patterns:
-}

pattern TDefEmpty = TypeDef "Empty" [] [] 
pattern TDefOne   = TypeDef "One" [] [ConstrDef "O" []]
pattern TDefBool  = TypeDef "Bool" [] [ConstrDef "True" [], ConstrDef "False" []]

pattern TDefTuple  = TypeDef "Tuple" ["a", "b"] [ConstrDef "Tuple" [TVar "a", TVar "b"]]
pattern TDefOption = TypeDef "Option" ["a"] [ConstrDef "None" [], ConstrDef "Some" [TVar "a"]]
pattern TDefList   = TypeDef "List" ["a"] [ConstrDef "Nil" [], 
                              ConstrDef "Cons" [TVar "a", TList (TVar "a")]]
