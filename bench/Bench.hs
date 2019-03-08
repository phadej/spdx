module Main (main) where

import Criterion.Main (Benchmark, bench, defaultMain, env, bgroup, nf)

import Control.Monad (forM)

import qualified Test.QuickCheck        as QC
import qualified Test.QuickCheck.Gen    as QC
import qualified Test.QuickCheck.Random as QC

import Distribution.SPDX
import Distribution.SPDX.Extra

import Generators
import OSI

main :: IO ()
main = defaultMain
    [ randomBench 15
    , randomBench 31
    , randomBench 132
    , osiBench 1337
    , osiBench 42
    ]

-------------------------------------------------------------------------------
-- Random satisfies bench
-------------------------------------------------------------------------------

randomBench :: Int -> Benchmark
randomBench seed = env (pairs <$> getExprs seed) $ \ps -> bgroup "random"
    [ bench "satisfies" $ nf (map $ uncurry satisfies . bimapLicense) ps
    , bench "satisfies2" $ nf (map $ uncurry satisfies2) ps
    ]

bimapLicense :: (LicenseExpression, LicenseExpression) -> (License, License)
bimapLicense (a, b) = (License a, License b)

pairs :: [a] -> [(a,a)]
pairs []        = []
pairs xs@(_:ys) = zip xs ys

-------------------------------------------------------------------------------
-- Bench of isOsiApprovedExpr
-------------------------------------------------------------------------------

osiBench :: Int -> Benchmark
osiBench seed = env (getExprs seed) $ \exprs -> bgroup ("Seed " ++ show seed)
    -- interestingly forcing expr takes more time than checking if it's osi approved
    [ bench "baseline"   $ nf id exprs
    , bench "direct"     $ nf (map isOsiApprovedExprDirect) exprs
    , bench "satisfies"  $ nf (map isOsiApprovedExprSatisfies) exprs
    , bench "satisfies2" $ nf (map isOsiApprovedExprSatisfies2) exprs
    ]

-------------------------------------------------------------------------------
-- Generate expression data
-------------------------------------------------------------------------------

getExprs :: Int -> IO [LicenseExpression]
getExprs seed = return exprs where
    exprs :: [LicenseExpression]
    exprs = QC.unGen g initGen 100
      where
        g :: QC.Gen [LicenseExpression]
        g = forM [1..1000] $ \s -> QC.resize (min 10 s) exprGen

        initGen :: QC.QCGen
        initGen = QC.mkQCGen seed
