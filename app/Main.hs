module Main where

import Asset
-- import Control.Monad.Except
-- import Control.Monad.Trans
-- import Data.Foldable (traverse_)
-- import Language.JavaScript.Parser (parseModule)

import Language.JavaScript.Parser (renderToString)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Usage: js-parser <entrypoint>"
      exitFailure
    (entrypoint : _) -> do
      content <- readFile entrypoint
      case mkAsset entrypoint content 0 of
        Right asset -> do
          -- print asset
          putStrLn . renderToString . aContent $ asset
        Left e -> print e