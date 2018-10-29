-- |
--
module Distribution.SPDX.Extra (
  -- * Types
  -- | We don't export the constructors, as they change with Cabal version.
  License,
  LicenseExpression,
  SimpleLicenseExpression,
  LicenseId,
  LicenseExceptionId,

  -- * Logic
  satisfies,
  equivalent,
  ) where

import Distribution.SPDX
       (License (..), LicenseExceptionId, LicenseExpression (..), LicenseId,
       SimpleLicenseExpression (..))
import Distribution.SPDX.Extra.Internal (LatticeSyntax (..))

import qualified Distribution.SPDX.Extra.Internal as LS

-- |
--
-- @⟦ satisfies a b ⟧ ≡ a ≥ b ≡ a ∧ b = b @
--
-- >>> unsafeParseExpr "GPL-3.0-only" `satisfies` unsafeParseExpr "ISC AND MIT"
-- False
--
-- >>> unsafeParseExpr "Zlib" `satisfies` unsafeParseExpr "ISC AND MIT AND Zlib"
-- True
--
-- >>> unsafeParseExpr "(MIT OR GPL-2.0-only)" `satisfies` unsafeParseExpr "(ISC AND MIT)"
-- True
--
-- >>> unsafeParseExpr "(MIT AND GPL-2.0-only)" `satisfies` unsafeParseExpr "(MIT AND GPL-2.0-only)"
-- True
--
-- >>> unsafeParseExpr "(MIT AND GPL-2.0-only)" `satisfies` unsafeParseExpr "(ISC AND GPL-2.0-only)"
-- False
--
satisfies :: License -- ^ package license
          -> License -- ^ license policy
          -> Bool
satisfies a b = exprToLSLic b `LS.preorder` exprToLSLic a

-- | Check wheather two 'LicenseExpression' are equivalent.
--
-- >>> unsafeParseExpr "(MIT AND GPL-2.0-only)" `equivalent` unsafeParseExpr "(GPL-2.0-only AND MIT)"
-- True
--
-- >>> unsafeParseExpr "MIT" `equivalent` unsafeParseExpr "MIT OR BSD-3-Clause"
-- False
--
equivalent :: License -> License -> Bool
equivalent a b = exprToLSLic a `LS.equivalent` exprToLSLic b

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

data Lic = Lic !SimpleLicenseExpression !(Maybe LicenseExceptionId)
  deriving (Eq, Ord)

exprToLSLic :: License -> LatticeSyntax Lic
exprToLSLic NONE          = LBound False
exprToLSLic (License lic) = licTo lic

licTo :: LicenseExpression -> LatticeSyntax Lic
licTo (ELicense lic exc) = LVar (Lic lic exc)
licTo (EAnd a b)         = LMeet (licTo a) (licTo b)
licTo (EOr a b)          = LJoin (licTo a) (licTo b)

-- $setup
-- >>> import Distribution.Parsec.Class (simpleParsec)
-- >>> let unsafeParseExpr e = maybe (error $ "invalid: " ++ e) (id :: License -> License) (simpleParsec e)
