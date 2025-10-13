-- Test script for GHCi
import Parser.Program
import Text.Megaparsec
import qualified Data.Text.IO as TIO

testFile path = do
  content <- TIO.readFile path
  case runParser program path content of
    Left err -> putStrLn $ errorBundlePretty err
    Right prog -> do
      putStrLn "Success!"
      print prog
