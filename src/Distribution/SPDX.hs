-- |
-- Module      : Distribution.SPDX
-- Description : SPDX licenses and expression language
-- Copyright   : (c) 2015 Oleg Grenrus
-- License     : BSD3
-- Maintainer  : Oleg Grenrus <oleg.grenrus@iki.fi>
--
module Distribution.SPDX (
  -- * Types
    LicenseId
  , LicenseExceptionId
  , LicenseRef(..)
  , LicenseExpression(..)
  -- * Data
  , licenseIdentifiers
  , licenseExceptions
  -- ** Ranges
  , licenseRanges
  , lookupLicenseRange
  -- * Parsing
  , parseExpression
  , unsafeParseExpr
  -- * Logic
  , satisfies
  ) where

import Distribution.SPDX.Types
import Distribution.SPDX.Ranges
import Distribution.SPDX.Licenses
import Distribution.SPDX.Parser
import Distribution.SPDX.LatticeSyntax

data Lic = Lic (Either LicenseRef String) (Maybe String)
  deriving (Eq, Ord, Show, Read)

exprToLSLic :: LicenseExpression -> LatticeSyntax Lic
exprToLSLic (ELicense False l e) = LVar (Lic l e)
exprToLSLic (ELicense True (Right l) e) = foldr1 LJoin $ map (\l' -> LVar $ Lic (Right l') e) $ lookupLicenseRange l
-- We don't know anything about newer license references
exprToLSLic (ELicense True (Left l) e) = LVar (Lic (Left l) e)
exprToLSLic (EConjunction a b) = LMeet (exprToLSLic a) (exprToLSLic b)
exprToLSLic (EDisjunction a b) = LJoin (exprToLSLic a) (exprToLSLic b)

-- |
--
-- @⟦ satisfies a b ⟧ ≡ a ≥ b ≡ a ∧ b = b @
--
-- >>> unsafeParseExpr "GPL-3.0" `satisfies` unsafeParseExpr "ISC AND MIT"
-- False
--
-- >>> unsafeParseExpr "Zlib" `satisfies` unsafeParseExpr "ISC AND MIT AND Zlib"
-- True
--
-- >>> unsafeParseExpr "(MIT OR GPL-2.0)" `satisfies` unsafeParseExpr "(ISC AND MIT)"
-- True
--
-- >>> unsafeParseExpr "(MIT AND GPL-2.0)" `satisfies` unsafeParseExpr "(MIT AND GPL-2.0)"
-- True
--
-- >>> unsafeParseExpr "(MIT AND GPL-2.0)" `satisfies` unsafeParseExpr "(ISC AND GPL-2.0)"
-- False
satisfies :: LicenseExpression -- ^ package license
          -> LicenseExpression -- ^ license policy
          -> Bool
satisfies a b = exprToLSLic b `preorder` exprToLSLic a
