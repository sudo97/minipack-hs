module Main where

import Asset
-- import Control.Monad.Except
-- import Control.Monad.Trans
-- import Data.Foldable (traverse_)
-- import Language.JavaScript.Parser (parseModule)

import Language.JavaScript.Parser (renderToString)
import System.Environment (getArgs)
import System.Exit (exitFailure)

-- import System.FilePath

-- type BundleMonad = ExceptT String IO

-- bundle :: String -> String -> IO ()
-- bundle content entrypoint = do
--   case parseModule content entrypoint of
--     Left e -> do
--       putStrLn $ "Error: " ++ show e
--     Right modl -> do
--       putStrLn $ "Module: " ++ show modl
--       putStrLn "Imports: "
--       traverse_ putStrLn (imports modl)

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
        Right asset -> print asset *> (putStrLn . renderToString . aContent $ asset)
        Left e -> print e