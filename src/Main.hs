{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Grammar.Program
import Interpreter.Program 
import Interpreter.Context
import Interpreter.InterpretT
import Control.Monad.State
import Control.Monad.Except
import Grammar.Type

-- Parser imports
import qualified Parser.Program as PP
import Text.Megaparsec (runParser, errorBundlePretty)
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)

-- Analyser imports
import qualified Analyser.Program as AP 

-- AI GENERATED Example!

nativeTypes :: [TypeDef]
nativeTypes = 
  [ TDefEmpty
  , TDefOne
  , TDefBool
  , TDefOption
  , TDefTuple
  , TDefList ]

initContext :: Program -> Context
initContext Program{..} = Context
  { _cTypes = nativeTypes ++ pTypes 
  , _cFuncs = pFuns
  , _cGlobs = globalToDVar <$> pGlobals
  , _cStack = [[]]  -- fix these
  , _cScope = 0     -- weird bugs
  }
  where 
    globalToDVar (Global name _ lit) = DVar name (lit2Val lit)

-- | Parse a Moo program from a file
parseProgram :: FilePath -> IO (Either String Program)
parseProgram filepath = do
  content <- TIO.readFile filepath
  case runParser PP.program filepath content of
    Left err -> pure $ Left $ errorBundlePretty err
    Right prog -> pure $ Right prog

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Usage: Moo <file.moo>"
      putStrLn "Example: Moo examples/hello.moo"
      exitFailure
    
    (filepath:_) -> do
      putStrLn $ "Parsing file: " ++ filepath
      result <- parseProgram filepath
      case result of
        Left err -> do
          putStrLn "Parse error:"
          putStrLn err
          exitFailure
        
        Right program -> do
          putStrLn "✓ Parse successful!"
          putStrLn $ "  Types: " ++ show (length $ pTypes program)
          putStrLn $ "  Globals: " ++ show (length $ pGlobals program)
          putStrLn $ "  Constants: " ++ show (length $ pConsts program)
          putStrLn $ "  Functions: " ++ show (length $ pFuns program)
          
          -- Semantic analysis
          putStrLn "\nAnalysing program..."
          case AP.checkProgram program of
            Left err -> do
              putStrLn "Analysis error:"
              print err
              exitFailure
            Right () -> do
              putStrLn "✓ Analysis successful!"
              
              -- Initialize interpreter context
              let ctx = initContext program
              
              -- Run the program
              putStrLn "\nRunning program..."
              result' <- runExceptT $ evalStateT evalProgram ctx
              case result' of
                Left err -> do
                  putStrLn "Runtime error:"
                  print err
                  exitFailure
                Right val -> do
                  putStrLn "Program completed successfully!"
                  print val
