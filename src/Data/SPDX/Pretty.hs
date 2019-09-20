{-# LANGUAGE CPP  #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif
-- | Parser inverse.
module Data.SPDX.Pretty
  ( prettyLicenseId
  , prettyLicenseExceptionId
  , prettyLicenseRef
  , prettyLicenseExpression
  ) where

import Data.SPDX.Types

prettyLicenseId :: LicenseId -> String
prettyLicenseId = getLicenseId

prettyLicenseExceptionId :: LicenseExceptionId -> String
prettyLicenseExceptionId = getLicenseExceptionId

prettyLicenseRef :: LicenseRef -> String
prettyLicenseRef (LicenseRef Nothing r)  = "LicenseRef-" ++ r
prettyLicenseRef (LicenseRef (Just d) r) = "DocumentRef-" ++ d ++ ":LicenseRef-" ++ r

prettyLicenseExpression :: LicenseExpression -> String
prettyLicenseExpression = pprExpr 1

pprLicense :: Either LicenseRef LicenseId -> String
pprLicense (Left ref) = prettyLicenseRef ref
pprLicense (Right i)  = prettyLicenseId i

pprExpr :: Int -> LicenseExpression -> String
pprExpr _ (ELicense newer lic exc) = pprLicense lic ++ n ++ e
  where n = if newer then "+" else ""
        e = case exc of
              Just exc' -> " WITH " ++ prettyLicenseExceptionId exc'
              Nothing   -> ""
pprExpr d (EConjunction e1 e2)     = parens (d < 0) $ pprExpr 0 e1 ++ " AND " ++ pprExpr 0 e2
pprExpr d (EDisjunction e1 e2)     = parens (d < 1) $ pprExpr 1 e1 ++ " OR " ++ pprExpr 1 e2

parens :: Bool -> String -> String
parens False s = s
parens True  s  = "(" ++ s ++ ")"
