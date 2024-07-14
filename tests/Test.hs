module Main (main) where

import qualified Asset
import qualified System.Exit as Exit
import Test.HUnit
import qualified TranspileTest

tests =
  TestList
    [ TestLabel "Transpile" TranspileTest.tests
    ]

main :: IO ()
main = do
  result <- runTestTT tests
  if errors result > 0 || failures result > 0
    then Exit.exitFailure
    else Exit.exitSuccess