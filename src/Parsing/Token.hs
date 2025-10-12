{-# LANGUAGE DeriveDataTypeable #-}

module Parsing.Token where

import Data.Data (Data, Typeable)
import Data.Text (Text)

-- | Position information for error reporting
data AlexPosn = AlexPn !Int  -- absolute character offset
                        !Int  -- line number
                        !Int  -- column number
        deriving (Eq, Show, Data, Typeable)

-- | Token with position information
data Token = Token AlexPosn TokenClass
  deriving (Eq, Show, Data, Typeable)

-- | Token classes
data TokenClass
  -- Keywords
  = TkFun
  | TkEnd
  | TkEndNamed Text  -- end-<name> for function/type definitions
  | TkDo
  | TkReturn
  | TkLet
  | TkMatch
  | TkWith
  | TkCase
  | TkOtherwise  -- Added for 'otherwise' in match
  | TkType
  | TkDef
  | TkWhile
  | TkFor
  | TkIn
  | TkPrint
  | TkScan
  | TkGlobal
  | TkConst
  
  -- Logical operators
  | TkAnd
  | TkOr
  | TkNot
  
  -- Literals
  | TkInt Integer
  | TkFloat Double
  | TkChar Char
  | TkString Text
  
  -- Identifiers (with naming conventions)
  | TkSnakeId Text    -- ^ snake_case (variables, functions params)
  | TkCamelId Text    -- ^ camelCase (functions)
  | TkPascalId Text   -- ^ PascalCase (types, constructors)
  
  -- Operators
  | TkPlus
  | TkMinus
  | TkStar
  | TkSlash
  | TkPercent
  | TkPlusPoint      -- +.
  | TkMinusPoint     -- -.
  | TkStarPoint      -- *.
  | TkSlashPoint     -- /.
  | TkPlusPlus       -- ++
  
  -- Comparison
  | TkEq             -- ==
  | TkNeq            -- /=
  | TkLt             -- <
  | TkGt             -- >
  | TkLeq            -- <=
  | TkGeq            -- >=
  
  -- Delimiters
  | TkLParen         -- (
  | TkRParen         -- )
  | TkLBracket       -- [
  | TkRBracket       -- ]
  | TkLBrace         -- {
  | TkRBrace         -- }
  
  -- Punctuation
  | TkComma
  | TkColon
  | TkColonEq        -- :=
  | TkArrow          -- ->
  | TkAt             -- @
  | TkUnderscore     -- _
  | TkExclaim        -- !
  
  -- Special
  | TkComment Text
  | TkEOF
  
  deriving (Eq, Show, Data, Typeable)

-- | Get position from token
tokenPosn :: Token -> AlexPosn
tokenPosn (Token pos _) = pos

-- | Get line number from position
posnLine :: AlexPosn -> Int
posnLine (AlexPn _ line _) = line

-- | Get column number from position
posnColumn :: AlexPosn -> Int
posnColumn (AlexPn _ _ col) = col
