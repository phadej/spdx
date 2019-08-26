{-# LANGUAGE CPP #-}
module Main (main) where

import Data.Maybe                       (isJust)
import Distribution.Pretty
import Distribution.SPDX
import Distribution.SPDX.Extra
import Distribution.SPDX.Extra.Internal (LatticeSyntax (..))
import Generators
import Prelude ()
import Prelude.Compat
import Test.Tasty
import Test.Tasty.QuickCheck            as QC

#if MIN_VERSION_Cabal(3,0,0)
import Distribution.Parsec
#else
import Distribution.Parsec.Class
#endif

import qualified Distribution.SPDX.Extra.Internal as LS

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, units]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

up :: String -> LicenseExpression
up = maybe (error "invalid expr") id . simpleParsec

valid :: String -> Bool
valid s = isJust (simpleParsec s :: Maybe License)

validExpr :: TestName -> TestTree
validExpr str = QC.testProperty str $ once $ property $ valid str

invalidExpr :: TestName -> TestTree
invalidExpr str = QC.testProperty str $ once $ property $ not $ valid str

units :: TestTree
units = testGroup "Unit tests" [ simpleUnits
                               , compositeUnits
                               ]


-- | Like `equivalent`, but prints a counterexample when it fails.
equivalentProp :: Either String License -> Maybe License -> Property
equivalentProp (Left _)  Nothing  = property True
equivalentProp (Left e)  (Just y) = counterexample (e ++ " /      = " ++ show y) $ False
equivalentProp (Right x) Nothing  = counterexample (show x ++ " / = <nothing>") $ False
equivalentProp (Right x) (Just y) = counterexample (show x ++ " /= " ++ show y) $ equivalent x y

simpleUnits :: TestTree
simpleUnits = testGroup "Simple License Expressions"
  [ invalidExpr "Invalid-Identifier"
  , validExpr "GPL-2.0-only"
  , validExpr "GPL-2.0-or-later"
  , validExpr "LicenseRef-23"
  , validExpr "LicenseRef-MIT-Style-1"
  , validExpr "DocumentRef-spdx-tool-1.2:LicenseRef-MIT-Style-2"
  ]

compositeUnits :: TestTree
compositeUnits = testGroup "Composite License Expressions"
  [ validExpr "LGPL-2.1-only OR MIT"
  , validExpr "LGPL-2.1-only OR MIT OR BSD-3-Clause"
  , validExpr "LGPL-2.1-only AND MIT"
  , validExpr "LGPL-2.1-only AND MIT AND BSD-2-Clause"
  , validExpr "GPL-2.0-or-later WITH Bison-exception-2.2"
  , QC.testProperty "Order of Precedence and Parentheses" $ once $ property $
    up "LGPL-2.1-only OR BSD-3-Clause AND MIT" == EOr (up "LGPL-2.1-only") (EAnd (up "BSD-3-Clause") (up "MIT"))
  ]

lsProps :: TestTree
lsProps = testGroup "LatticeSyntax"
  [ QC.testProperty "a ≤ b ⇔ a ∨ b ≡ b ⇔ a ≡ a ∧ b" $ forAll latticeSyntaxGen $ \a -> forAll latticeSyntaxGen $ \b ->
     let lhs = ((a `LJoin` b) `LS.equivalent` b)
         rhs = ((a `LMeet` b) `LS.equivalent` a)
     in label (show lhs) (lhs === rhs)
  , QC.testProperty "equivalent reflexive" $ forAll latticeSyntaxGen $ \a -> a `LS.equivalent` a
  , QC.testProperty "preorder reflexive" $ forAll latticeSyntaxGen $ \a -> a `LS.preorder` a
  ]

qcProps :: TestTree
qcProps = testGroup "By Quickcheck"
  [ QC.testProperty "licence identifiers are valid licenses" $ forAll licenseIdGen $ valid . prettyShow
  , lsProps
  , parsePrettyProps
  ]

parsePrettyProps :: TestTree
parsePrettyProps = testGroup "parse . pretty"
  [ QC.testProperty "LicenseId" $ forAll licenseIdGen $ \i -> mkLicenseId latestLicenseListVersion (prettyShow i) === Just i
  , QC.testProperty "LicenseExpression"  $ forAllShrink (scaleGen (`div` 3) exprGen)  exprShrink $ \e ->
      let p = prettyShow e
      in counterexample p $ eitherParsec p `equivalentProp` Just e
  , QC.testProperty "LicenseExpression'" $ forAllShrink (scaleGen (`div` 3) exprGen') exprShrink $ \e ->
      let p = prettyShow e
      in counterexample p $ eitherParsec p `equivalentProp` Just e
  ]
