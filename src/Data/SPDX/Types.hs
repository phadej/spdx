{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DeriveGeneric #-}
#endif

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

#if __GLASGOW_HASKELL__ >= 701
import GHC.Generics
#endif

data LicenseRef = LicenseRef
  { lrDocument :: !(Maybe String)
  , lrLicense  :: !String
  }
  deriving (Eq, Ord, Show, Read, Typeable, Data
#if __GLASGOW_HASKELL__ >= 701
           , Generic
#endif
           )

-- | Opaque license identifier type.
newtype LicenseId = LicenseId String
  deriving (Eq, Ord, Show, Read, Typeable, Data
#if __GLASGOW_HASKELL__ >= 701
           , Generic
#endif
           )

getLicenseId :: LicenseId -> String
getLicenseId (LicenseId l) = l

-- | Opaque license exception identifier type.
newtype LicenseExceptionId = LicenseExceptionId String
  deriving (Eq, Ord, Show, Read, Typeable, Data
#if __GLASGOW_HASKELL__ >= 701
           , Generic
#endif
           )

getLicenseExceptionId :: LicenseExceptionId -> String
getLicenseExceptionId (LicenseExceptionId l) = l

data LicenseExpression = ELicense !Bool !(Either LicenseRef LicenseId) !(Maybe LicenseExceptionId)
                       | EConjunction !LicenseExpression !LicenseExpression
                       | EDisjunction !LicenseExpression !LicenseExpression
  deriving (Eq, Ord, Show, Read, Typeable, Data
#if __GLASGOW_HASKELL__ >= 701
           , Generic
#endif
           )
