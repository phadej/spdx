{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
#if __GLASGOW_HASKELL__ >= 701
#if __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
#endif
module Data.SPDX.Types (
    LicenseId(..)
  , getLicenseId
  , LicenseExceptionId(..)
  , getLicenseExceptionId
  , LicenseRef(..)
  , LicenseExpression(..)
  ) where

import Data.Data
import GHC.Generics

data LicenseRef = LicenseRef
  { lrDocument :: !(Maybe String)
  , lrLicense  :: !String
  }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

-- | Opaque license identifier type.
newtype LicenseId = LicenseId String
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

getLicenseId :: LicenseId -> String
getLicenseId (LicenseId l) = l

-- | Opaque license exception identifier type.
newtype LicenseExceptionId = LicenseExceptionId String
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

getLicenseExceptionId :: LicenseExceptionId -> String
getLicenseExceptionId (LicenseExceptionId l) = l

data LicenseExpression = ELicense !Bool !(Either LicenseRef LicenseId) !(Maybe LicenseExceptionId)
                       | EConjunction !LicenseExpression !LicenseExpression
                       | EDisjunction !LicenseExpression !LicenseExpression
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)
