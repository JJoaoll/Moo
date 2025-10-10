module Parser.Expr where


{-

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

-}