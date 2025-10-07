{-# LANGUAGE OverloadedStrings #-}

module Parser.Expr where

import Grammar.Expr

import Text.Megaparsec 
import Text.Megaparsec.Char
import Grammar.Type
import Data.Text 

import qualified Text.Megaparsec.Char.Lexer as L
import Parser.Utils.Utils 
import Parser.Utils.Cases
import Control.Monad (when)
import GHC.Float (int2Float)

litInt, litChar, litFloat, litConstr :: Parser Lit

literal :: Parser Lit
literal = choice 
  [ litFloat
  , litInt
  , litChar
  , litConstr
  ] 

litInt = lexeme $ fmap LInt $ 
  try L.decimal <|> 
  do _ <- char '-'
     n <- L.decimal
     pure (-n)

litChar = lexeme $ do 
  _ <- char '\''
  c <- L.charLiteral
  _ <- char '\''
  pure (LChar c)

litFloat = lexeme $ fmap LFloat $
  try L.float <|>
  do (LInt int) <- litInt
     _ <- char '.'
     pure $ int2Float int
     

litConstr = undefined


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