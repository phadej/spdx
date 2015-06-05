module Main (main) where

import Control.Applicative
import Data.List
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Distribution.SPDX
import Distribution.SPDX.LatticeSyntax

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, units]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

up :: String -> LicenseExpression
up = unsafeParseExpr

valid :: String -> Bool
valid = (==1) . length . parseExpression

validExpr :: TestName -> TestTree
validExpr str = QC.testProperty str $ once $ property $ valid str

invalidExpr :: TestName -> TestTree
invalidExpr str = QC.testProperty str $ once $ property $ not $ valid str

units :: TestTree
units = testGroup "Unit tests" [ simpleUnits
                               , compositeUnits
                               , rangeUnit
                               ]

simpleExprGen :: Gen String
simpleExprGen = elements licenseIdentifiers

latticeSyntaxGen :: Gen (LatticeSyntax Char)
latticeSyntaxGen = sized gen
  where var = elements "abcdef"
        gen 0 = LVar <$> var
        gen n = oneof [ LVar <$> var
                      , LMeet <$> gen' <*> gen'
                      , LJoin <$> gen' <*> gen'
                      ]
          where gen' = gen (n `div` 2)

simpleUnits :: TestTree
simpleUnits = testGroup "Simple License Expressions"
  [ invalidExpr "Invalid-Identifier"
  , validExpr "GPL-2.0"
  , validExpr "GPL-2.0+"
  , validExpr "LicenseRef-23"
  , validExpr "LicenseRef-MIT-Style-1"
  , validExpr "DocumentRef-spdx-tool-1.2:LicenseRef-MIT-Style-2"
  ]

compositeUnits :: TestTree
compositeUnits = testGroup "Composite License Expressions"
  [ validExpr "LGPL-2.1 OR MIT"
  , validExpr "LGPL-2.1 OR MIT OR BSD-3-Clause"
  , validExpr "LGPL-2.1 AND MIT"
  , validExpr "LGPL-2.1 AND MIT AND BSD-2-Clause"
  , validExpr "GPL-2.0+ WITH Bison-exception-2.2"
  , QC.testProperty "Order of Precedence and Parentheses" $ once $ property $
    up "LGPL-2.1 OR BSD-3-Clause AND MIT" == EDisjunction (up "LGPL-2.1") (EConjunction (up "BSD-3-Clause") (up "MIT"))
  ]

rangeUnit :: TestTree
rangeUnit = QC.testProperty "calculated license ranges" $ once $ property $ sort licenseRanges == sort ranges

lsProps :: TestTree
lsProps = testGroup "LatticeSyntax"
  [ QC.testProperty "a ≤ b ⇔ a ∨ b ≡ b ⇔ a ≡ a ∧ b" $ forAll latticeSyntaxGen $ \a -> forAll latticeSyntaxGen $ \b ->
     let lhs = ((a `LJoin` b) `equivalent` b)
         rhs = ((a `LMeet` b) `equivalent` a)
     in label (show lhs) (lhs === rhs)
  , QC.testProperty "equivalent reflexive" $ forAll latticeSyntaxGen $ \a -> a `equivalent` a
  , QC.testProperty "preorder reflexive" $ forAll latticeSyntaxGen $ \a -> a `preorder` a
  ]

qcProps :: TestTree
qcProps = testGroup "By Quickcheck"
  [ QC.testProperty "licence identifiers are valid licenses" $ forAll simpleExprGen $ valid
  , lsProps
  ]

ranges :: [[LicenseId]]
ranges = [
  [
    "AFL-1.1",
    "AFL-1.2",
    "AFL-2.0",
    "AFL-2.1",
    "AFL-3.0"
  ],
  [ "AGPL-1.0", "AGPL-3.0" ],
  [
    "Apache-1.0",
    "Apache-1.1",
    "Apache-2.0"
  ],
  [
    "APSL-1.0",
    "APSL-1.1",
    "APSL-1.2",
    "APSL-2.0"
  ],
  [
    "Artistic-1.0",
    "Artistic-2.0"
  ],
  [
    "BitTorrent-1.0",
    "BitTorrent-1.1"
  ],
  [
    "CC-BY-1.0",
    "CC-BY-2.0",
    "CC-BY-2.5",
    "CC-BY-3.0",
    "CC-BY-4.0"
  ],
  [
    "CC-BY-NC-1.0",
    "CC-BY-NC-2.0",
    "CC-BY-NC-2.5",
    "CC-BY-NC-3.0",
    "CC-BY-NC-4.0"
  ],
  [
    "CC-BY-NC-ND-1.0",
    "CC-BY-NC-ND-2.0",
    "CC-BY-NC-ND-2.5",
    "CC-BY-NC-ND-3.0",
    "CC-BY-NC-ND-4.0"
  ],
  [
    "CC-BY-NC-SA-1.0",
    "CC-BY-NC-SA-2.0",
    "CC-BY-NC-SA-2.5",
    "CC-BY-NC-SA-3.0",
    "CC-BY-NC-SA-4.0"
  ],
  [
    "CC-BY-ND-1.0",
    "CC-BY-ND-2.0",
    "CC-BY-ND-2.5",
    "CC-BY-ND-3.0",
    "CC-BY-ND-4.0"
  ],
  [
    "CC-BY-SA-1.0",
    "CC-BY-SA-2.0",
    "CC-BY-SA-2.5",
    "CC-BY-SA-3.0",
    "CC-BY-SA-4.0"
  ],
  [
    "CDDL-1.0",
    "CDDL-1.1"
  ],
  [
    "CECILL-1.0",
    "CECILL-1.1",
    "CECILL-2.0"
  ],
  [
    "ECL-1.0",
    "ECL-2.0"
  ],
  [
    "EFL-1.0",
    "EFL-2.0"
  ],
  [
    "EUPL-1.0",
    "EUPL-1.1"
  ],
  [
    "GFDL-1.1",
    "GFDL-1.2",
    "GFDL-1.3"
  ],
  [
    "GPL-1.0",
    "GPL-2.0",
    "GPL-3.0"
  ],
  [
    "LGPL-2.0",
    "LGPL-2.1",
    "LGPL-3.0"
  ],
  [
    "LPL-1.0",
    "LPL-1.02"
  ],
  [
    "LPPL-1.0",
    "LPPL-1.1",
    "LPPL-1.2",
    "LPPL-1.3a",
    "LPPL-1.3c"
  ],
  [
    "MPL-1.0",
    "MPL-1.1",
    "MPL-2.0"
  ],
  [
    "NPL-1.0",
    "NPL-1.1"
  ],
  [
    "OFL-1.0",
    "OFL-1.1"
  ],
  [
    "OLDAP-1.1",
    "OLDAP-1.2",
    "OLDAP-1.3",
    "OLDAP-1.4",
    "OLDAP-2.0",
    "OLDAP-2.0.1",
    "OLDAP-2.1",
    "OLDAP-2.2",
    "OLDAP-2.2.1",
    "OLDAP-2.2.2",
    "OLDAP-2.3",
    "OLDAP-2.4",
    "OLDAP-2.5",
    "OLDAP-2.6",
    "OLDAP-2.7",
    "OLDAP-2.8"
  ],
  [
    "OSL-1.0",
    "OSL-1.1",
    "OSL-2.0",
    "OSL-2.1",
    "OSL-3.0"
  ],
  [
    "PHP-3.0",
    "PHP-3.01"
  ],
  [
    "RPL-1.1",
    "RPL-1.5"
  ],
  [
    "SGI-B-1.0",
    "SGI-B-1.1",
    "SGI-B-2.0"
  ],
  [
    "YPL-1.0",
    "YPL-1.1"
  ],
  [
    "ZPL-1.1",
    "ZPL-2.0",
    "ZPL-2.1"
  ],
  [
    "Zimbra-1.3",
    "Zimbra-1.4"
  ],
  [
    "bzip2-1.0.5",
    "bzip2-1.0.6"
  ]
  ]
