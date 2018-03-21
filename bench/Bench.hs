module Main (main) where

import Generators
import Test.QuickCheck
import Test.QuickCheck.Random (mkQCGen)

import           Data.SPDX

main :: IO ()
main = quickCheckWith args $
    forAll (scaleGen (`div` 3) exprGen) $
        \e -> isOsiApprovedExpr e == isOsiApprovedExpr' e
  where
    args = stdArgs
        { replay = Just (mkQCGen 1337, 0)
        , maxSuccess = 500
        }

osiLicenseIds :: [LicenseId]
osiLicenseIds = filter isOsiApproved licenseIdentifiers

osiLicenseExpr :: LicenseExpression
osiLicenseExpr = foldr1 EConjunction $ map (\l -> ELicense False (Right l) Nothing) osiLicenseIds

isOsiApprovedExpr :: LicenseExpression -> Bool
isOsiApprovedExpr (ELicense _ (Left _) _) = False
isOsiApprovedExpr (ELicense _ _ (Just _)) = False
isOsiApprovedExpr (ELicense False (Right e) Nothing) = isOsiApproved e
isOsiApprovedExpr (ELicense True (Right e2) Nothing) = any isOsiApproved $ lookupLicenseRange e2
isOsiApprovedExpr (EConjunction e1 e2) = isOsiApprovedExpr e1 && isOsiApprovedExpr e2
isOsiApprovedExpr (EDisjunction e1 e2) = isOsiApprovedExpr e1 || isOsiApprovedExpr e2

isOsiApprovedExpr' :: LicenseExpression -> Bool
isOsiApprovedExpr' e = e `satisfies` osiLicenseExpr
