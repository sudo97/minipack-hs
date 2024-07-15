module Graph where

import Asset
import Control.Monad
import Control.Monad.Error.Class (liftEither)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Foldable (find)
import Data.IORef
import Data.List (intercalate)
import Language.JavaScript.Parser
import System.Directory
import System.FilePath

type GraphM a = ExceptT String IO a

-- Looks ugly, because I just moved it from rust. TLDR it is just stack based graph traversal
createGraph :: FilePath -> GraphM [(Asset, [(String, Int)])]
createGraph filePath = do
  counter <- liftIO $ newIORef 0
  mainAsset <- createAsset filePath =<< liftIO (readIORef counter)
  counter += 1

  stack <- liftIO $ newIORef [mainAsset]
  visited <- liftIO $ newIORef [mainAsset]
  result <- liftIO $ newIORef []

  let loop = do
        item <- pop stack
        case item of
          Nothing -> pure ()
          Just asset -> do
            let dirname = takeDirectory $ aPath asset
            mapping <- forM (aDependencies asset) $ \dep -> do
              absolutePath <- liftIO $ canonicalizePath $ dirname </> dep
              visited' <- liftIO $ readIORef visited
              case find ((== absolutePath) . aPath) visited' of
                Just child -> pure (dep, aId child)
                Nothing -> do
                  child <- createAsset absolutePath =<< liftIO (readIORef counter)
                  counter += 1
                  push stack child
                  push visited child
                  pure (dep, aId child)
            push result (asset, mapping)
            loop

  loop

  liftIO $ readIORef result

push :: IORef [a] -> a -> GraphM ()
push ioref val = liftIO $ modifyIORef' ioref (val :)

pop :: IORef [a] -> GraphM (Maybe a)
pop ioref = do
  val <- liftIO $ readIORef ioref
  case val of
    [] -> pure Nothing
    (x : xs) -> do
      liftIO $ writeIORef ioref xs
      pure (Just x)

(+=) :: IORef Int -> Int -> GraphM ()
(+=) ioref val = liftIO $ modifyIORef' ioref (+ val)

(.=) :: IORef a -> a -> GraphM ()
(.=) ioref val = liftIO $ modifyIORef' ioref (const val)

createAsset :: FilePath -> Int -> GraphM Asset
createAsset path id' = do
  path' <- liftIO $ canonicalizePath path
  contents <- liftIO $ readFile path'
  liftEither $ mkAsset path contents id'

bundle :: [(Asset, [(String, Int)])] -> String
bundle [] = ""
bundle [(asset, _)] = renderToString (aContent asset)
bundle xs = do
  let result =
        foldl
          ( \acc (asset, mapping) ->
              acc
                ++ show (aId asset)
                ++ ": [\n"
                ++ "function (require, module) {\n"
                ++ renderToString (aContent asset)
                ++ "\n},\n"
                ++ asJSON mapping
                ++ "\n],\n"
          )
          ""
          xs
   in "(function(modules) {\n"
        ++ "function require(id) {\n"
        ++ "const [fn, mapping] = modules[id];\n\n"
        ++ "function localRequire(name) {\n"
        ++ "return require(mapping[name]);\n}\n\n"
        ++ "const module = {};\n\n"
        ++ "fn(localRequire, module);\n\n"
        ++ "return module;\n}\n\n"
        ++ "require(0);\n"
        ++ "})({"
        ++ result
        ++ "})"

asJSON :: [(String, Int)] -> String
asJSON [] = "{}"
asJSON [(x, y)] = "{" ++ show x ++ ": " ++ show y ++ "}"
asJSON xs = "{" ++ intercalate ", " (map (\(x, y) -> show x ++ ": " ++ show y) xs) ++ "}"