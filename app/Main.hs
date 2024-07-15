module Main where

import Asset
import Control.Monad
-- import Language.JavaScript.Parser (renderToString)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import qualified Data.Map as M
import System.Environment (getArgs)
import System.Exit (exitFailure)

-- import Data.IORef
-- import Control.Monad.Error.Class
-- import System.Directory
-- import System.FilePath

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

type GraphM a = ExceptT String IO a

createGraph :: String -> GraphM [(Asset, M.Map String Int)]
createGraph = undefined