module Distribution.SPDX.Ranges (licenseRanges, lookupLicenseRange) where

import Data.Char
import Data.List
import Data.Maybe

import Distribution.SPDX.Types
import Distribution.SPDX.Licenses

licenseRanges :: [[LicenseId]]
licenseRanges = filter longerThanSingleton . map f $ prefixes
  where f p = filter (\l -> p `isPrefixOf` l && restIsNumber l p) licenseIdentifiers

-- | Lookup newer licenses we know about
--
-- >>> lookupLicenseRange "MIT"
-- ["MIT"]
--
-- >>> lookupLicenseRange "GPL-2.0"
-- ["GPL-2.0","GPL-3.0"]
--
-- >>> lookupLicenseRange "LGPL-2.0"
-- ["LGPL-2.0","LGPL-2.1","LGPL-3.0"]
lookupLicenseRange :: LicenseId -> [LicenseId]
lookupLicenseRange l = fromMaybe [l] $ lookup l ranges'

ranges' :: [(LicenseId, [LicenseId])]
ranges' = map (\r -> (head r, r)) . filter (not . nullOrSingleton) . concatMap tails $ licenseRanges

nullOrSingleton :: [a] -> Bool
nullOrSingleton []      = True
nullOrSingleton [_]     = True
nullOrSingleton (_:_:_) = False

restIsNumber :: String -> String -> Bool
restIsNumber l p = all (\c -> isDigit c || c == 'a' || c == 'b' || c == 'c' || c == '.') (drop (length p) l)

pref :: String -> String
pref = reverse . dropWhile (/= '-') . reverse

prefixes :: [String]
prefixes = nub (filter (not . null) $ map pref licenseIdentifiers)

longerThanSingleton :: [a] -> Bool
longerThanSingleton []      = False
longerThanSingleton [_]     = False
longerThanSingleton (_:_:_) = True
