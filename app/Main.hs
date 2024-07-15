module Main where

import Asset
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S
-- import Language.JavaScript.Parser (renderToString)
import System.Directory
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath

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
  let maps = aMapping <$> graph
  let mapStrings = M.toList <$> maps
  liftIO $ print mapStrings
  pure ()

type GraphM a = ExceptT String IO a

createGraph :: String -> GraphM [Asset]
createGraph path = do
  content <- liftIO $ readFile path
  mainAsset <- ExceptT $ return $ mkAsset path content 0
  let initialVisited = S.singleton (aPath mainAsset)
  let initialQueue = [(mainAsset, 0)]
  (mainAsset :) <$> traverseGraph initialQueue initialVisited []

traverseGraph :: [(Asset, Int)] -> S.Set String -> [Asset] -> GraphM [Asset]
traverseGraph [] _ result = return result
traverseGraph ((asset, id') : queue) visited result = do
  let dirname = takeDirectory (aPath asset)
  -- liftIO $ putStrLn dirname
  newAssets <- forM (aDependencies asset) $ \dep -> do
    let depPath = dirname </> dep
    absolutePath <- liftIO $ canonicalizePath depPath
    -- liftIO $ putStrLn depPath
    -- liftIO $ putStrLn absolutePath
    if S.member absolutePath visited
      then return Nothing
      else do
        content <- liftIO $ readFile absolutePath
        newAsset <- ExceptT $ return $ mkAsset absolutePath content (id' + 1)
        return $ Just (newAsset, id' + 1)
  let newAssetsFiltered = catMaybes newAssets
  let newVisited = foldr (S.insert . aPath . fst) visited newAssetsFiltered
  let newQueue = queue ++ newAssetsFiltered
  let updatedAsset = asset {aMapping = M.fromList [(dep, aId newAsset) | (newAsset, _) <- newAssetsFiltered, dep <- aDependencies asset]}
  traverseGraph newQueue newVisited (result ++ [updatedAsset])