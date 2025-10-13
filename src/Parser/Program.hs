{-# LANGUAGE OverloadedStrings #-}

module Parser.Program where

import Grammar.Type 
import Utils (Name)

import Text.Megaparsec
import Control.Monad (when)
import qualified Data.Text as T
import Data.List (find)
import Prelude hiding (const)

import Parser.Utils.Utils
import qualified Parser.Program.ConstDef as ConstDef
import qualified Parser.Program.GlobalDef as GlobalDef
import qualified Parser.Program.TypeDef as TypeDef
import qualified Parser.Program.FunDef as FunDef
import Grammar.Program

nativeTypes :: [TypeDef]
nativeTypes = 
  [ TDefEmpty
  , TDefOne
  , TDefBool
  , TDefOption
  , TDefTuple
  , TDefList ]

-- | Empty program to use as accumulator
emptyProgram :: Program
emptyProgram = Program [] [] [] nativeTypes

-- | Built-in type names that cannot be redefined
builtinTypes :: [Name]
builtinTypes = ["Empty", "One", "Bool", "Tuple", "Option", "List", "String",
  "Int", "Float", "Char"]

-- | Built-in constructor names
builtinConstructors :: [Name]
builtinConstructors = ["O", "True", "False", "Tuple", "None", "Some", "Nil", "Cons"]

-- | Reserved function names (allow "main" as user wants it!)
reservedFunctions :: [Name]
reservedFunctions = []  

-- | Parse a complete program with definitions in any order
program :: Parser Program
program = do
  sc  -- consume initial whitespace
  prog <- parseDefinitions emptyProgram
  eof
  
  -- Reverse is another option to to maintain original order (we prepend during parsing)
  -- for future debugging..
  pure $ Program 
    (pGlobals prog)
    (pConsts prog)
    (pFuns prog)
    (pTypes prog)

-- | Parse definitions in any order, accumulating and validating
parseDefinitions :: Program -> Parser Program
parseDefinitions prog = do
  maybeDef <- optional (try programDef)
  case maybeDef of
    Nothing  -> pure prog  -- No more definitions
    Just def -> do
      prog' <- validateAndAdd def prog
      parseDefinitions prog'

-- | Sum type for any program definition
data ProgramDef
  = PDType TypeDef
  | PDGlobal GlobalDef
  | PDConst ConstDef
  | PDFun FunDef

-- | Parse any single top-level definition
programDef :: Parser ProgramDef
programDef = choice $ try <$>
  [ PDType   <$> TypeDef.typeDef
  , PDGlobal <$> GlobalDef.globalDef
  , PDConst  <$> ConstDef.constDef
  , PDFun    <$> FunDef.funDef
  ]

-- | Validate and add a definition to the program
validateAndAdd :: ProgramDef -> Program -> Parser Program
validateAndAdd (PDType typedef) prog = do
  let typeName = tName typedef
      constructors = tConstrs typedef
  
  -- Check 1: Type name is builtin?
  when (typeName `elem` builtinTypes) $
    fail $ "Cannot redefine built-in type: " ++ T.unpack typeName
  
  -- Check 2: Any constructor is builtin?
  mapM_ (\c -> when (cName c `elem` builtinConstructors) $
    fail $ "Cannot redefine built-in constructor: " ++ T.unpack (cName c)) constructors
  
  -- Check 3: Type already defined?
  when (typeName `elem` map tName (pTypes prog)) $
    fail $ "Type already defined: " ++ T.unpack typeName
  
  -- Check 4: Constructor already used in another type?
  let allConstructors = concatMap tConstrs (pTypes prog)
      constructorNames = map cName constructors
  mapM_ (\cname -> 
    case find ((cname==) . cName) allConstructors of
      Just _ -> fail $ "Constructor already defined in another type: " ++ T.unpack cname
      Nothing -> pure ()
    ) constructorNames
  
  pure $ prog { pTypes = typedef : pTypes prog }

validateAndAdd (PDGlobal global) prog = do
  let name = gName global
  
  -- Check: Global name already used?
  when (name `elem` map gName (pGlobals prog)) $
    fail $ "Global variable already defined: " ++ T.unpack name
  
  -- Also check if name conflicts with consts or functions
  when (name `elem` map kName (pConsts prog)) $
    fail $ "Name already used as constant: " ++ T.unpack name
  when (name `elem` map fName (pFuns prog)) $
    fail $ "Name already used as function: " ++ T.unpack name
  
  pure $ prog { pGlobals = global : pGlobals prog }

validateAndAdd (PDConst const) prog = do
  let name = kName const
  
  -- Check: Const name already used?
  when (name `elem` map kName (pConsts prog)) $
    fail $ "Constant already defined: " ++ T.unpack name
  
  -- Also check if name conflicts with globals or functions
  when (name `elem` map gName (pGlobals prog)) $
    fail $ "Name already used as global variable: " ++ T.unpack name
  when (name `elem` map fName (pFuns prog)) $
    fail $ "Name already used as function: " ++ T.unpack name
  
  pure $ prog { pConsts = const : pConsts prog }

validateAndAdd (PDFun fun) prog = do
  let name = fName fun
  
  -- Check: Reserved function name? (currently none)
  when (name `elem` reservedFunctions) $
    fail $ "Cannot use reserved function name: " ++ T.unpack name
  
  -- Check: Function name already used?
  when (name `elem` map fName (pFuns prog)) $
    fail $ "Function already defined: " ++ T.unpack name
  
  -- Also check if name conflicts with globals or consts
  when (name `elem` map gName (pGlobals prog)) $
    fail $ "Name already used as global variable: " ++ T.unpack name
  when (name `elem` map kName (pConsts prog)) $
    fail $ "Name already used as constant: " ++ T.unpack name
  
  pure $ prog { pFuns = fun : pFuns prog }