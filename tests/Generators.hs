{-# LANGUAGE CPP #-}
module Generators where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 0
#endif

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import           Test.Tasty.QuickCheck as QC

import           Data.SPDX
import           Data.SPDX.LatticeSyntax (LatticeSyntax(..))

licenseIdGen :: Gen LicenseId
licenseIdGen = elements licenseIdentifiers

licenseExceptionGen :: Gen LicenseExceptionId
licenseExceptionGen = elements licenseExceptions

maybeGen :: Gen a -> Gen (Maybe a)
maybeGen g = oneof [ pure Nothing, Just <$> g]

licenseRefGen :: Gen LicenseRef
licenseRefGen = LicenseRef <$> maybeGen idStringGen <*> idStringGen

idStringGen :: Gen String
idStringGen = elements ["foo", "bar", "baz", "AllRightsReserved"]

latticeSyntaxGen :: Gen (LatticeSyntax Char)
latticeSyntaxGen = sized gen
  where var   = LVar <$> elements "abcdef"
        gen 0 = var
        gen n = oneof [ var
                      , LMeet <$> gen' <*> gen'
                      , LJoin <$> gen' <*> gen'
                      ]
          where gen' = gen (n `div` 2)

mkExprGen :: Gen LicenseExpression -> Gen LicenseExpression
mkExprGen licGen = sized gen
  where gen 0 = licGen
        gen n = oneof [ licGen
                      , EDisjunction <$> gen' <*> gen'
                      , EConjunction <$> gen' <*> gen'
                      ]
          where gen' = gen (n `div` 2)

exprGen :: Gen LicenseExpression
exprGen = mkExprGen $ ELicense <$> arbitrary <*> (Right <$> licenseIdGen) <*> (pure Nothing)

-- | 'exprGen' which contains also LicenseRefs and exceptions
exprGen' :: Gen LicenseExpression
exprGen' = mkExprGen $ ELicense <$> arbitrary <*> eitherLicenseIdRefGen <*> maybeGen licenseExceptionGen

eitherLicenseIdRefGen :: Gen (Either LicenseRef LicenseId)
eitherLicenseIdRefGen = oneof [Right <$> licenseIdGen, Left <$> licenseRefGen]

exprShrink :: LicenseExpression -> [LicenseExpression]
exprShrink (ELicense _ _ _)   = []
exprShrink (EDisjunction a b) = a : b : ((a `EDisjunction`) <$> exprShrink b) ++ ((`EDisjunction` b) <$> exprShrink a)
exprShrink (EConjunction a b) = a : b : ((a `EConjunction`) <$> exprShrink b) ++ ((`EConjunction` b) <$> exprShrink a)

scaleGen :: (Int -> Int) -> Gen a -> Gen a
scaleGen f g = sized (\n -> resize (f n) g)
