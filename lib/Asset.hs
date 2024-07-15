module Asset (Asset (..), mkAsset) where

import qualified Data.Map as M
import Language.JavaScript.Parser
import Transpile

data Asset = Asset
  { aId :: Int,
    aPath :: String,
    aContent :: JSAST,
    aMapping :: M.Map String Int,
    aDependencies :: [String]
  }
  deriving (Show, Eq)

mkAsset :: String -> String -> Int -> Either String Asset
mkAsset path content id' = do
  mdl <- parseModule content path
  deps <- imports mdl
  pure Asset {aId = id', aPath = path, aContent = transpile mdl, aMapping = M.empty, aDependencies = deps}
