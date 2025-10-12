{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Parsing.MooParser
Description : Main interface for Moo parser using Alex/Happy
Copyright   : (c) 2025 Moo Language Team
License     : MIT

High-level interface for parsing Moo source files.
Provides better error messages than the Megaparsec implementation.
-}

module Parsing.MooParser
  ( parseFile
  , parseString
  , ParseResult(..)
  ) where

import qualified Parsing.Lexer as Lexer
import qualified Parsing.Parser as Parser
import Grammar.Program (Program)
import Control.Exception (catch, SomeException)

-- | Result of parsing
data ParseResult
  = ParseSuccess Program
  | ParseError String
  deriving (Show)

-- | Parse a Moo source file
parseFile :: FilePath -> IO ParseResult
parseFile path = do
  content <- readFile path
  return $ parseString content

-- | Parse a Moo source string
parseString :: String -> ParseResult
parseString input =
  let tokens = Lexer.scanTokens input
      result = Parser.parseProgram tokens
  in ParseSuccess result  -- TODO: Better error handling
