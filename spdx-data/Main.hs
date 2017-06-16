module Main (main) where

import Data.List (sort)
import Data.Foldable (toList, for_)
import Data.Csv (decode, HasHeader (..))

import qualified Data.ByteString.Lazy as LBS

extractLicense :: [String] -> (LicenseId, String, Bool)
extractLicense row = (LicenseId $ row !! 1, row !! 0, (row !! 4) == "YES")

newtype LicenseId = LicenseId String
  deriving (Eq, Ord, Show)

main :: IO ()
main = do
    contents <- LBS.readFile "data/spdx_licenselist_v2.6_licenses.csv"
    licenses <- either fail (pure . toList) (decode HasHeader contents)
    for_ (sort $ map extractLicense licenses) $ \triple ->
        putStrLn $ "  , " ++ show triple
