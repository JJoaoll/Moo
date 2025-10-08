module Main (main) where

import Test.Hspec
import qualified Parser.Program.ImportTest

main :: IO ()
main = hspec $ do
  describe "Parser.Program.Import" Parser.Program.ImportTest.spec
