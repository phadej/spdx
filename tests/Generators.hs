{-# LANGUAGE CPP #-}
module Generators where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 0
#endif

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import           Test.QuickCheck as QC

import           Distribution.SPDX
import           Distribution.SPDX.Extra.Internal (LatticeSyntax(..))

licenseIdGen :: Gen LicenseId
licenseIdGen = elements $ licenseIdList LicenseListVersion_3_2

licenseExceptionGen :: Gen LicenseExceptionId
licenseExceptionGen = elements $ licenseExceptionIdList LicenseListVersion_3_2

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

mkExprGen :: Gen LicenseExpression -> Gen LicenseExpression
mkExprGen licGen = sized gen where
    gen 0 = licGen
    gen n = oneof [ licGen
                  , EOr <$> gen' <*> gen'
                  , EAnd <$> gen' <*> gen'
                  ]
      where gen' = gen (n `div` 2)

licenseGen :: Gen License
licenseGen = frequency
    [ (1, pure NONE)
    , (50, License <$> exprGen)
    ]

licenseGen' :: Gen License
licenseGen' = frequency
    [ (1, pure NONE)
    , (50, License <$> exprGen')
    ]

exprGen :: Gen LicenseExpression
exprGen = mkExprGen $ ELicense <$> simpleLicenseExprGen <*> pure Nothing

-- | 'exprGen' which contains also LicenseRefs and exceptions
exprGen' :: Gen LicenseExpression
exprGen' = mkExprGen $ ELicense <$> simpleLicenseExprGen <*> liftArbitrary licenseExceptionGen

simpleLicenseExprGen :: Gen SimpleLicenseExpression
simpleLicenseExprGen = oneof [ELicenseId <$> licenseIdGen, ELicenseRef <$> licenseRefGen]

licenseShrink :: License -> [License]
licenseShrink NONE        = []
licenseShrink (License e) = map License (exprShrink e)

exprShrink :: LicenseExpression -> [LicenseExpression]
exprShrink = go where
    go :: LicenseExpression -> [LicenseExpression]
    go (ELicense _ _) = []
    go (EOr a b)      = a : b : map (uncurry EOr)  (liftShrink2 go go (a, b))
    go (EAnd a b)     = a : b : map (uncurry EAnd) (liftShrink2 go go (a, b))

scaleGen :: (Int -> Int) -> Gen a -> Gen a
scaleGen f g = sized (\n -> resize (f n) g)
