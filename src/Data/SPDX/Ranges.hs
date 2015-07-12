{-# LANGUAGE Safe #-}
module Data.SPDX.Ranges (licenseRanges, lookupLicenseRange) where

import Data.Char
import Data.List
import Data.Maybe

import Data.SPDX.Types
import Data.SPDX.Licenses

licenseRanges :: [[LicenseId]]
licenseRanges = filter longerThanSingleton . map f $ prefixes
  where f p = filter (\l -> p `isPrefixOf` getLicenseId l && restIsNumber (getLicenseId l) p) licenseIdentifiers

-- | Lookup newer licenses we know about
--
-- >>> lookupLicenseRange $ fromJust $ mkLicenseId "MIT"
-- [LicenseId "MIT"]
--
-- >>> lookupLicenseRange $ fromJust $ mkLicenseId "GPL-2.0"
-- [LicenseId "GPL-2.0",LicenseId "GPL-3.0"]
--
-- >>> lookupLicenseRange $ fromJust $ mkLicenseId "LGPL-2.0"
-- [LicenseId "LGPL-2.0",LicenseId "LGPL-2.1",LicenseId "LGPL-3.0"]
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
prefixes = nub (filter (not . null) $ map (pref . getLicenseId) licenseIdentifiers)

longerThanSingleton :: [a] -> Bool
longerThanSingleton []      = False
longerThanSingleton [_]     = False
longerThanSingleton (_:_:_) = True
