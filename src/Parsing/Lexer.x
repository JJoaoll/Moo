{
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Parsing.Lexer
Description : Alex-generated lexer for Moo language
Copyright   : (c) 2025 Moo Language Team
License     : MIT

Lexical analyzer for the Moo programming language.
Handles keywords, operators, identifiers (with case conventions), 
literals, and comments.
-}

module Parsing.Lexer
  ( scanTokens
  , alexError
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (isSpace, isDigit, isAlpha, isAlphaNum, isLower, isUpper, ord)
import qualified Parsing.Token as Tok
}

%wrapper "basic"

-- Character classes
$digit = 0-9
$alpha = [a-zA-Z]
$lower = [a-z]
$upper = [A-Z]
$digit = [0-9]
$alphaNum = [a-zA-Z0-9]

-- Identifiers (BOTH can match all-lowercase - context decides!)
-- snakeCase: lowercase + (lowercase|digit|underscore)* - for VARIABLES
@snakeCase = $lower ($lower | $digit | \_)*
-- camelCase: lowercase + (alphaNum)* but NO underscore - for FUNCTIONS
@camelCase = $lower $alphaNum*
@pascalCase = $upper $alphaNum*

tokens :-

  -- Whitespace (ignored)
  $white+                               ;
  
  -- Comments (line comments start with //)
  "//" .*                               ;

  -- Keywords (order matters - longer matches first!)
  "otherwise"                           { \_ -> Tok.TkOtherwise }
  "<const>"                             { \_ -> Tok.TkConst }
  "global"                              { \_ -> Tok.TkGlobal }
  "end-match"                           { \_ -> Tok.TkEnd }
  "end-case"                            { \_ -> Tok.TkEnd }
  "end-while"                           { \_ -> Tok.TkEnd }
  "end-for"                             { \_ -> Tok.TkEnd }
  "end-def"                             { \_ -> Tok.TkEnd }
  "end-" @camelCase                     { \s -> Tok.TkEndNamed (T.pack $ drop 4 s) }
  "scan!"                               { \_ -> Tok.TkScan }
  "fun"                                 { \_ -> Tok.TkFun }
  "return"                              { \_ -> Tok.TkReturn }
  "let"                                 { \_ -> Tok.TkLet }
  "match"                               { \_ -> Tok.TkMatch }
  "with"                                { \_ -> Tok.TkWith }
  "case"                                { \_ -> Tok.TkCase }
  "type"                                { \_ -> Tok.TkType }
  "def"                                 { \_ -> Tok.TkDef }
  "do"                                  { \_ -> Tok.TkDo }
  "while"                               { \_ -> Tok.TkWhile }
  "for"                                 { \_ -> Tok.TkFor }
  "in"                                  { \_ -> Tok.TkIn }
  "print"                               { \_ -> Tok.TkPrint }
  
  -- Logical operators (keywords)
  "and"                                 { \_ -> Tok.TkAnd }
  "or"                                  { \_ -> Tok.TkOr }
  "not"                                 { \_ -> Tok.TkNot }
  
  -- Multi-character operators (before single-char versions!)
  "+."                                  { \_ -> Tok.TkPlusPoint }
  "-."                                  { \_ -> Tok.TkMinusPoint }
  "*."                                  { \_ -> Tok.TkStarPoint }
  "/."                                  { \_ -> Tok.TkSlashPoint }
  "++"                                  { \_ -> Tok.TkPlusPlus }
  "=="                                  { \_ -> Tok.TkEq }
  "/="                                  { \_ -> Tok.TkNeq }
  "<="                                  { \_ -> Tok.TkLeq }
  ">="                                  { \_ -> Tok.TkGeq }
  ":="                                  { \_ -> Tok.TkColonEq }
  "->"                                  { \_ -> Tok.TkArrow }
  
  -- Single-character operators
  "+"                                   { \_ -> Tok.TkPlus }
  "-"                                   { \_ -> Tok.TkMinus }
  "*"                                   { \_ -> Tok.TkStar }
  "/"                                   { \_ -> Tok.TkSlash }
  "%"                                   { \_ -> Tok.TkPercent }
  "<"                                   { \_ -> Tok.TkLt }
  ">"                                   { \_ -> Tok.TkGt }
  
  -- Delimiters
  "("                                   { \_ -> Tok.TkLParen }
  ")"                                   { \_ -> Tok.TkRParen }
  "["                                   { \_ -> Tok.TkLBracket }
  "]"                                   { \_ -> Tok.TkRBracket }
  "{"                                   { \_ -> Tok.TkLBrace }
  "}"                                   { \_ -> Tok.TkRBrace }
  
  -- Punctuation
  ","                                   { \_ -> Tok.TkComma }
  ":"                                   { \_ -> Tok.TkColon }
  "@"                                   { \_ -> Tok.TkAt }
  "_"                                   { \_ -> Tok.TkUnderscore }
  "!"                                   { \_ -> Tok.TkExclaim }
  
  -- Literals
  $digit+\.$digit+                      { \s -> Tok.TkFloat (read s) }
  $digit+\.                             { \s -> Tok.TkFloat (read (s ++ "0")) }  -- 3. → 3.0
  $digit+                               { \s -> Tok.TkInt (read s) }
  \' ([^\'] | \\\') \'                  { \s -> Tok.TkChar (parseChar s) }
  \" ([^\"] | \\\")* \"                 { \s -> Tok.TkString (T.pack $ parseString s) }
  
  -- Identifiers (order matters: most specific first!)
  @pascalCase                           { \s -> Tok.TkPascalId (T.pack s) }
  @camelCase                            { \s -> Tok.TkCamelId (T.pack s) }
  @snakeCase                            { \s -> Tok.TkSnakeId (T.pack s) }

{
-- | Parse character literal (handles escapes)
parseChar :: String -> Char
parseChar ('\'':c:'\'':_) = case c of
  '\\' -> '\\'
  'n'  -> '\n'
  't'  -> '\t'
  '\'' -> '\''
  _    -> c
parseChar _ = error "Invalid character literal"

-- | Parse string literal (removes quotes and handles escapes)
parseString :: String -> String
parseString = unescape . init . tail
  where
    unescape [] = []
    unescape ('\\':'n':xs) = '\n' : unescape xs
    unescape ('\\':'t':xs) = '\t' : unescape xs
    unescape ('\\':'\\':xs) = '\\' : unescape xs
    unescape ('\\':'"':xs) = '"' : unescape xs
    unescape (x:xs) = x : unescape xs

-- | Error handler
alexError :: String -> a
alexError msg = error $ "Lexical error: " ++ msg

-- | Wrapper to add position information to tokens (simplified for now)
scanTokens :: String -> [Tok.Token]
scanTokens str = map (Tok.Token dummyPos) (alexScanTokens str)
  where
    dummyPos = Tok.AlexPn 0 1 1  -- TODO: Track real positions
}
