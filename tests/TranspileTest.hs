module TranspileTest where

import Asset (transpile)
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Test.HUnit

noImport :: JSAST
noImport = readJsModule "console.log('hello');"

defaultInput :: JSAST
defaultInput = readJsModule "import x from 'y';"

defaultOutput :: JSAST
defaultOutput = readJsModule "const x = require('y');"

tests :: Test
tests =
  TestList
    [ TestLabel "Only changes imports" $ TestCase $ do
        assertEqual "noImport" noImport (transpile noImport),
      TestLabel "Transpile" $ TestCase $ do
        assertEqual "default" defaultOutput (transpile defaultInput)
    ]