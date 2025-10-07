{-# LANGUAGE OverloadedStrings #-}

module Parser.Expr.Lit where

import Grammar.Expr

import Text.Megaparsec 
import Text.Megaparsec.Char
-- import Grammar.Type
import Data.Text 

import qualified Text.Megaparsec.Char.Lexer as L
import Parser.Utils.Utils 
import Parser.Utils.Cases
-- import Control.Monad (when)
import GHC.Float (int2Float)

litInt, litChar, litFloat, litConstr :: Parser Lit

literal :: Parser Lit
literal = choice $ try <$>
  [ litFloat
  , litInt
  , litChar
  , litConstr
  --, special -- builtin
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
     

litConstr = 
  try simpleC <|>
  complexC

simpleC, complexC :: Parser Lit

simpleC = LConstr <$> name <*> pure []
complexC = do
  cName <- name
  _ <- symbol "("
  cArgs <- literal `sepBy` symbol ","
  _ <- symbol ")"

  pure $ LConstr cName cArgs

name :: Parser Text
name = lexeme pascalCase

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
