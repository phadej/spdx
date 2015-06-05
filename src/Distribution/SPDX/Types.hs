{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.SPDX.Types where

import Data.Data
import GHC.Generics

data LicenseRef = LicenseRef
  { lrDocument :: !(Maybe String)
  , lrLicense  :: !String
  }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

type LicenseId = String
type LicenseExceptionId = String

data LicenseExpression = ELicense !Bool !(Either LicenseRef LicenseId) !(Maybe LicenseExceptionId)
                       | EConjunction !LicenseExpression !LicenseExpression
                       | EDisjunction !LicenseExpression !LicenseExpression
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)
