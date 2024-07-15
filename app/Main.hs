module Main where

import Asset
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Graph
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
      _ <- runExceptT $ printGraph entrypoint
      pure ()

printGraph :: String -> GraphM ()
printGraph path = do
  graph <- createGraph path
  forM_ graph $ \(asset, mapping) -> liftIO $ do
    putStrLn $ aPath asset
    print mapping
    putStrLn ""
