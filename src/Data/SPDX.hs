-- |
-- Module      : Data.SPDX
-- Description : SPDX licenses and expression language
-- Copyright   : (c) 2015 Oleg Grenrus
-- License     : BSD3
-- Maintainer  : Oleg Grenrus <oleg.grenrus@iki.fi>
--
module Data.SPDX (
  -- * Types
    LicenseId
  , getLicenseId
  , LicenseExceptionId
  , getLicenseExceptionId
  , LicenseRef(..)
  , LicenseExpression(..)
  -- * Data
  , licenses
  , licenseIdentifiers
  , mkLicenseId
  , isOsiApproved
  , licenseExceptions
  -- ** Ranges
  , licenseRanges
  , lookupLicenseRange
  -- * Parsing
  , parseExpression
  , unsafeParseExpr
  -- * Prettifying
  -- | Inverse of parsing
  , prettyLicenseId
  , prettyLicenseExceptionId
  , prettyLicenseRef
  , prettyLicenseExpression
  -- * Logic
  , satisfies
  , equivalent
  ) where

import Data.SPDX.LatticeSyntax (LatticeSyntax(..))
import qualified Data.SPDX.LatticeSyntax as LS
import           Data.SPDX.Licenses
import           Data.SPDX.Parser
import           Data.SPDX.Pretty
import           Data.SPDX.Ranges
import           Data.SPDX.Types

data Lic = Lic (Either LicenseRef LicenseId) (Maybe LicenseExceptionId)
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
satisfies a b = exprToLSLic b `LS.preorder` exprToLSLic a

-- | Check wheather two 'LicenseExpression' are equivalent.
--
-- >>> unsafeParseExpr "(MIT AND GPL-2.0)" `equivalent` unsafeParseExpr "(GPL-2.0 AND MIT)"
-- True
--
-- >>> unsafeParseExpr "MIT" `equivalent` unsafeParseExpr "MIT OR BSD-3-Clause"
-- False
equivalent :: LicenseExpression -> LicenseExpression -> Bool
equivalent a b = exprToLSLic a `LS.equivalent` exprToLSLic b
