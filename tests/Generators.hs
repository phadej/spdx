{-# LANGUAGE CPP #-}
module Generators where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 0
#endif

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import           Test.Tasty.QuickCheck as QC

import           Distribution.SPDX
import           Distribution.SPDX.Extra.Internal (LatticeSyntax(..))

latestLicenseListVersion :: LicenseListVersion
#if MIN_VERSION_Cabal(3,4,0)
latestLicenseListVersion = LicenseListVersion_3_9
#elif MIN_VERSION_Cabal(3,0,0)
latestLicenseListVersion = LicenseListVersion_3_6
#else
latestLicenseListVersion = LicenseListVersion_3_2
#endif

licenseIdGen :: Gen LicenseId
licenseIdGen = elements $ licenseIdList latestLicenseListVersion

licenseExceptionGen :: Gen LicenseExceptionId
licenseExceptionGen = elements $ licenseExceptionIdList latestLicenseListVersion

licenseRefGen :: Gen LicenseRef
licenseRefGen = mkLicenseRef' <$> liftArbitrary idStringGen <*> idStringGen

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

mkExprGen :: Gen LicenseExpression -> Gen License
mkExprGen licGen = License <$> sized gen where
    gen 0 = licGen
    gen n = oneof [ licGen
                  , EOr <$> gen' <*> gen'
                  , EAnd <$> gen' <*> gen'
                  ]
      where gen' = gen (n `div` 2)

exprGen :: Gen License
exprGen = mkExprGen $ ELicense <$> simpleLicenseExprGen <*> pure Nothing

-- | 'exprGen' which contains also LicenseRefs and exceptions
exprGen' :: Gen License
exprGen' = mkExprGen $ ELicense <$> simpleLicenseExprGen <*> liftArbitrary licenseExceptionGen

simpleLicenseExprGen :: Gen SimpleLicenseExpression
simpleLicenseExprGen = oneof [ELicenseId <$> licenseIdGen, ELicenseRef <$> licenseRefGen]

exprShrink :: License -> [License]
exprShrink NONE        = []
exprShrink (License e) = map License (go e) where
    go :: LicenseExpression -> [LicenseExpression]
    go (ELicense _ _) = []
    go (EOr a b)      = a : b : map (uncurry EOr)  (liftShrink2 go go (a, b))
    go (EAnd a b)     = a : b : map (uncurry EAnd) (liftShrink2 go go (a, b))

scaleGen :: (Int -> Int) -> Gen a -> Gen a
scaleGen f g = sized (\n -> resize (f n) g)
