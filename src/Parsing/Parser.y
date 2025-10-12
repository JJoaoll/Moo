{
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Parsing.Parser
Description : Happy-generated parser for Moo language
Copyright   : (c) 2025 Moo Language Team
License     : MIT

Parser for the Moo programming language, generating an AST
from a token stream produced by the Alex lexer.
-}

module Parsing.Parser
  ( parseProgram
  , parseError
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Parsing.Token
import Utils (Name)
import Grammar.Program
import Grammar.Type
import Grammar.Expr
import Grammar.Sttm
}

%name parseProgram program
%tokentype { Token }
%error { parseError }

%token
  -- Keywords
  fun         { Token _ TkFun }
  end         { Token _ TkEnd }
  ENDNAMED    { Token _ (TkEndNamed $$) }
  do          { Token _ TkDo }
  return      { Token _ TkReturn }
  let         { Token _ TkLet }
  match       { Token _ TkMatch }
  with        { Token _ TkWith }
  case        { Token _ TkCase }
  otherwise   { Token _ TkOtherwise }
  type        { Token _ TkType }
  def         { Token _ TkDef }
  while       { Token _ TkWhile }
  for         { Token _ TkFor }
  in          { Token _ TkIn }
  print       { Token _ TkPrint }
  scan        { Token _ TkScan }
  '@global'   { Token _ TkGlobal }
  '<const>'   { Token _ TkConst }
  
  -- Logical
  and         { Token _ TkAnd }
  or          { Token _ TkOr }
  not         { Token _ TkNot }
  
  -- Literals
  INT         { Token _ (TkInt $$) }
  FLOAT       { Token _ (TkFloat $$) }
  CHAR        { Token _ (TkChar $$) }
  STRING      { Token _ (TkString $$) }
  
  -- Identifiers
  SNAKE       { Token _ (TkSnakeId $$) }
  CAMEL       { Token _ (TkCamelId $$) }
  PASCAL      { Token _ (TkPascalId $$) }
  
  -- Operators
  '+'         { Token _ TkPlus }
  '-'         { Token _ TkMinus }
  '*'         { Token _ TkStar }
  '/'         { Token _ TkSlash }
  '%'         { Token _ TkPercent }
  '+.'        { Token _ TkPlusPoint }
  '-.'        { Token _ TkMinusPoint }
  '*.'        { Token _ TkStarPoint }
  '/.'        { Token _ TkSlashPoint }
  '++'        { Token _ TkPlusPlus }
  '=='        { Token _ TkEq }
  '/='        { Token _ TkNeq }
  '<'         { Token _ TkLt }
  '>'         { Token _ TkGt }
  '<='        { Token _ TkLeq }
  '>='        { Token _ TkGeq }
  
  -- Delimiters
  '('         { Token _ TkLParen }
  ')'         { Token _ TkRParen }
  '['         { Token _ TkLBracket }
  ']'         { Token _ TkRBracket }
  '{'         { Token _ TkLBrace }
  '}'         { Token _ TkRBrace }
  
  -- Punctuation
  ','         { Token _ TkComma }
  ':'         { Token _ TkColon }
  ':='        { Token _ TkColonEq }
  '->'        { Token _ TkArrow }
  '@'         { Token _ TkAt }
  '_'         { Token _ TkUnderscore }
  '!'         { Token _ TkExclaim }

-- Operator precedence (lowest to highest)
%right or
%right and
%nonassoc '==' '/='
%nonassoc '<' '<=' '>' '>='
%right '++'
%left '+' '-' '+.' '-.'
%left '*' '/' '%' '*.' '/.'
%left NEG NOT

%%

-- ============================================================
-- PROGRAM
-- ============================================================

program :: { Program }
program
  : definitions                         { buildProgram $1 }

definitions :: { [ProgramDef] }
definitions
  : {- empty -}                         { [] }
  | definition definitions              { $1 : $2 }

definition :: { ProgramDef }
definition
  : typeDef                             { PDType $1 }
  | globalDef                           { PDGlobal $1 }
  | constDef                            { PDConst $1 }
  | funDef                              { PDFun $1 }

-- ============================================================
-- TYPE DEFINITIONS
-- ============================================================

typeDef :: { TypeDef }
typeDef
  : type PASCAL typeParams def constructors end
      { TypeDef $2 $3 $5 }

typeParams :: { [Name] }
typeParams
  : {- empty -}                         { [] }
  | '(' typeParamList ')'               { $2 }

typeParamList :: { [Name] }
typeParamList
  : SNAKE                               { [$1] }
  | SNAKE ',' typeParamList             { $1 : $3 }

constructors :: { [ConstrDef] }
constructors
  : constructor                         { [$1] }
  | constructor constructors            { $1 : $2 }

constructor :: { ConstrDef }
constructor
  : PASCAL                              { ConstrDef $1 [] }
  | PASCAL '(' typeList ')'             { ConstrDef $1 $3 }

-- ============================================================
-- GLOBAL & CONST DEFINITIONS
-- ============================================================

globalDef :: { GlobalDef }
globalDef
  : '@global' SNAKE ':' typeExpr ':=' literal
      { Global $2 $4 $6 }

constDef :: { ConstDef }
constDef
  : '<const>' SNAKE ':' typeExpr ':=' literal
      { Const $2 $4 $6 }

-- ============================================================
-- FUNCTION DEFINITIONS
-- ============================================================

funDef :: { FunDef }
funDef
  : fun funName '(' params ')' '->' typeExpr do stmts ENDNAMED
      { FunDef $2 $4 $7 $10 $9 }
  | fun funName '(' params ')' '->' typeExpr do stmts end
      { FunDef $2 $4 $7 "" $9 }

funName :: { Name }
funName
  : CAMEL                               { $1 }
  | SNAKE                               { $1 }

params :: { [Param] }
params
  : {- empty -}                         { [] }
  | paramList                           { $1 }

paramList :: { [Param] }
paramList
  : param                               { [$1] }
  | param ',' paramList                 { $1 : $3 }

param :: { Param }
param
  : SNAKE ':' typeExpr                  { Param $1 $3 }

-- ============================================================
-- TYPES
-- ============================================================

typeExpr :: { Type }
typeExpr
  : PASCAL                              { parseType $1 [] }
  | PASCAL '(' typeList ')'             { parseType $1 $3 }
  | SNAKE                               { TVar $1 }

typeList :: { [Type] }
typeList
  : typeExpr                            { [$1] }
  | typeExpr ',' typeList               { $1 : $3 }

-- ============================================================
-- STATEMENTS
-- ============================================================

stmts :: { [Sttm] }
stmts
  : {- empty -}                         { [] }
  | stmt stmts                          { $1 : $2 }

stmt :: { Sttm }
stmt
  : let SNAKE ':' typeExpr ':=' expr    { SInit $2 $4 $6 }
  | SNAKE ':=' expr                     { SAtrib $1 $3 }
  | '@' SNAKE ':=' expr                 { SGtrib $2 $4 }
  | print '(' expr ')'                  { SPrint $3 }
  | match expr with cases end           { SMatch $2 $4 }
  | while expr do stmts end             { SWhile $2 $4 }
  | for SNAKE in expr do stmts end      { SFor $2 $4 $6 }
  | return expr                         { SReturn $2 }
  | funCall                             { let (n, args) = $1 in SFunCall n args }

cases :: { [(Pattern, [Sttm])] }
cases
  : caseClause                          { [$1] }
  | caseClause cases                    { $1 : $2 }

caseClause :: { (Pattern, [Sttm]) }
caseClause
  : case pattern do stmts end           { ($2, $4) }
  | otherwise do stmts end              { (PWildcard, $3) }

-- ============================================================
-- PATTERNS
-- ============================================================

pattern :: { Pattern }
pattern
  : literal                             { PLit $1 }
  | SNAKE                               { PVar $1 }
  | '_'                                 { PWildcard }
  | PASCAL                              { PConstructor $1 [] }
  | PASCAL '(' patternList ')'          { PConstructor $1 $3 }

patternList :: { [Pattern] }
patternList
  : pattern                             { [$1] }
  | pattern ',' patternList             { $1 : $3 }

-- ============================================================
-- EXPRESSIONS
-- ============================================================

expr :: { Expr }
expr
  -- Literals and variables
  : literal                             { ELit $1 }
  | SNAKE                               { EVar $1 }
  | '<' SNAKE '>'                       { EConst $2 }
  | '@' SNAKE                           { EGlobal $2 }
  
  -- Constructors
  | PASCAL                              { EConstr $1 [] }
  | PASCAL '(' exprList ')'             { EConstr $1 $3 }
  
  -- Function calls
  | funCall                             { let (n, args) = $1 in EFunCall n args }
  
  -- Scan
  | scan '!' '(' typeExpr ')'           { EScan $4 }
  
  -- Unary operators
  | '-' expr                  %prec NEG { EUnOp Neg $2 }
  | not expr                  %prec NOT { EUnOp Not $2 }
  
  -- Binary operators (arithmetic)
  | expr '+' expr                       { EBinOp $1 Add $3 }
  | expr '-' expr                       { EBinOp $1 Sub $3 }
  | expr '*' expr                       { EBinOp $1 Mul $3 }
  | expr '/' expr                       { EBinOp $1 Div $3 }
  | expr '%' expr                       { EBinOp $1 Rem $3 }
  | expr '+.' expr                      { EBinOp $1 Add_ $3 }
  | expr '-.' expr                      { EBinOp $1 Sub_ $3 }
  | expr '*.' expr                      { EBinOp $1 Mul_ $3 }
  | expr '/.' expr                      { EBinOp $1 Div_ $3 }
  | expr '++' expr                      { EBinOp $1 Cat $3 }
  
  -- Binary operators (comparison)
  | expr '==' expr                      { EBinOp $1 Eq $3 }
  | expr '/=' expr                      { EBinOp $1 NEq $3 }
  | expr '<' expr                       { EBinOp $1 Lt $3 }
  | expr '<=' expr                      { EBinOp $1 LEq $3 }
  | expr '>' expr                       { EBinOp $1 Gt $3 }
  | expr '>=' expr                      { EBinOp $1 GEq $3 }
  
  -- Binary operators (logical)
  | expr and expr                       { EBinOp $1 And $3 }
  | expr or expr                        { EBinOp $1 Or $3 }
  
  -- Parentheses
  | '(' expr ')'                        { $2 }

funCall :: { (Name, [Expr]) }
funCall
  : funName '(' ')'                     { ($1, []) }
  | funName '(' exprList ')'            { ($1, $3) }

exprList :: { [Expr] }
exprList
  : expr                                { [$1] }
  | expr ',' exprList                   { $1 : $3 }

-- ============================================================
-- LITERALS
-- ============================================================

literal :: { Lit }
literal
  : INT                                 { LInt (fromInteger $1) }
  | FLOAT                               { LFloat (realToFrac $1) }
  | CHAR                                { LChar $1 }

{
-- ============================================================
-- HELPER FUNCTIONS
-- ============================================================

-- | Helper to build Program from list of definitions
buildProgram :: [ProgramDef] -> Program
buildProgram defs = Program globals consts funs types
  where
    globals = [g | PDGlobal g <- defs]
    consts  = [c | PDConst c <- defs]
    funs    = [f | PDFun f <- defs]
    types   = nativeTypes ++ [t | PDType t <- defs]
    
    nativeTypes = [TDefEmpty, TDefOne, TDefBool, TDefOption, TDefTuple, TDefList]

-- | Sum type for program definitions (used during parsing)
data ProgramDef
  = PDType TypeDef
  | PDGlobal GlobalDef
  | PDConst ConstDef
  | PDFun FunDef

-- | Parse type name with parameters into Type
parseType :: Name -> [Type] -> Type
parseType name params = case name of
  "Int"    -> TInt
  "Float"  -> TFloat
  "Char"   -> TChar
  _        -> TData name params

-- | Error handler with better messages
parseError :: [Token] -> a
parseError [] = error "Parse error: unexpected end of input"
parseError (Token (AlexPn _ line col) tok : _) =
  error $ "Parse error at line " ++ show line ++ ", column " ++ show col 
       ++ ": unexpected " ++ showToken tok

-- | Show token for error messages
showToken :: TokenClass -> String
showToken (TkSnakeId t) = "identifier '" ++ T.unpack t ++ "'"
showToken (TkCamelId t) = "function name '" ++ T.unpack t ++ "'"
showToken (TkPascalId t) = "type/constructor '" ++ T.unpack t ++ "'"
showToken (TkInt n) = "integer " ++ show n
showToken (TkFloat f) = "float " ++ show f
showToken (TkChar c) = "character '" ++ [c] ++ "'"
showToken tok = show tok
}
