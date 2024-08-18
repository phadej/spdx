{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Control.Monad            (forM, forM_)
import Control.Monad.ST         (runST)
import Data.Foldable            (foldl')
import Data.Primitive           (Prim)
import Data.Primitive.PrimArray (newPrimArray, readPrimArray, writePrimArray)
import System.FilePath          ((</>))
import Test.Tasty               (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit         (assertBool, testCaseSteps)

import qualified DPLL
import qualified DPLL.DIMACS
import qualified MiniSat

main :: IO ()
main = defaultMain $ testGroup "dpll-dimacs"
    [ testGroup "bf"
        [ testSAT "bf" "bf0432-007"
        , testSAT "bf" "bf1355-075"
        , testSAT "bf" "bf1355-638"
        , testSAT "bf" "bf2670-001"
        , testSAT "bf" "ssa0432-003"
        , testSAT "bf" "ssa2670-130"
        , testSAT "bf" "ssa2670-141"
        -- , testSAT "bf" "ssa6288-047" -- big problem
        , testSAT "bf" "ssa7552-038"
        , testSAT "bf" "ssa7552-158"
        , testSAT "bf" "ssa7552-159"
        , testSAT "bf" "ssa7552-160"
        ]
    , testGroup "blocksworld"
        [ testSAT "blocksworld" "anomaly"
        , testSAT "blocksworld" "medium"
        , testSAT "blocksworld" "bw_large.a"
        , testSAT "blocksworld" "bw_large.b"
        -- , testSAT "blocksworld" "bw_large.c"
        ]
    , testGroup "bmc"
        [ testSAT "bmc" "bmc-ibm-2"
        -- , testSAT "bmc" "bmc-ibm-1"
        -- , testSAT "bmc" "bmc-ibm-3"
        -- , testSAT "bmc" "bmc-ibm-4"
        -- , testSAT "bmc" "bmc-ibm-5"
        -- , testSAT "bmc" "bmc-ibm-6"
        -- , testSAT "bmc" "bmc-ibm-7"
        ]
    , testGroup "logicstics"
        [ testSAT "logistics" "logistics.a"
        , testSAT "logistics" "logistics.b"
        , testSAT "logistics" "logistics.c"
        , testSAT "logistics" "logistics.d"
        ]
    , testGroup "parity"
        [ testSAT "parity" "par8-1"
        , testSAT "parity" "par8-1-c"
        , testSAT "parity" "par8-2"
        , testSAT "parity" "par8-2-c"
        , testSAT "parity" "par8-3"
        , testSAT "parity" "par8-3-c"
        , testSAT "parity" "par8-4"
        , testSAT "parity" "par8-4-c"
        , testSAT "parity" "par8-5"
        , testSAT "parity" "par8-5-c"
        , testSAT "parity" "par16-1"
        , testSAT "parity" "par16-1-c"
        ]
    , testGroup "phole"
        [ testSAT "phole" "hole6"
        , testSAT "phole" "hole7"
        , testSAT "phole" "hole8"
        ]
    ]

testSAT :: FilePath -> FilePath -> TestTree
testSAT dir name = testCaseSteps name $ \info -> do
    dimacs <- DPLL.DIMACS.parseDimacsFile ("dimacs" </> dir </> (name ++ ".cnf"))
    let solution = solveDPLL dimacs
    if null solution
    then do
        info "UNSAT"
        solution' <- solveMiniSat dimacs
        assertBool "MiniSat thinks its unsat" (null solution')
    else do
        info "SAT"
        solution' <- solveMiniSat $ [[x] | x <- solution] ++ dimacs
        assertBool "MiniSat validates" (not (null solution'))

-------------------------------------------------------------------------------
-- DPLL
-------------------------------------------------------------------------------

solveDPLL :: [[Int]] -> [Int]
solveDPLL clauses = runST $ do
    s <- DPLL.newSolver

    -- create literal
    literals <- newPrimArray maxLiteral
    forM_ [1..maxLiteral] $ \i -> do
        l <- DPLL.newLit s
        writePrimArray literals (i - 1) l

    -- addClauses
    forM_ clauses $ \clause -> do
        clause' <- forM clause $ \i -> do
            l <- readPrimArray literals (abs i - 1)
            return $ if i < 0 then DPLL.neg l else l

        DPLL.addClause s clause'

    -- solve
    res <- DPLL.solve s

    -- read back the solution
    if res
    then do
        forM [1..maxLiteral] $ \i -> do
            l <- readPrimArray literals (i - 1)
            x <- DPLL.modelValue s l
            return $ if x then i else negate i

    else return []
  where
    maxLiteral = foldl' (\acc clause -> foldl' (\x y -> max x (abs y)) acc clause) 0 clauses

solveMiniSat :: [[Int]] -> IO [Int]
solveMiniSat clauses = do
    s <- MiniSat.newSolver

    -- create literal
    literals <- newPrimArray maxLiteral
    forM_ [1..maxLiteral] $ \i -> do
        l <- MiniSat.newLit s
        writePrimArray literals (i - 1) l

    -- addClauses
    forM_ clauses $ \clause -> do
        clause' <- forM clause $ \i -> do
            l <- readPrimArray literals (abs i - 1)
            return $ if i < 0 then MiniSat.neg l else l

        MiniSat.addClause s clause'

    -- solve
    res <- MiniSat.solve s []

    -- read back the solution
    if res
    then do
        forM [1..maxLiteral] $ \i -> do
            l <- readPrimArray literals (i - 1)
            x <- MiniSat.modelValue s l
            return $ case x of
                Nothing    -> i
                Just True  -> i
                Just False -> negate i

    else return []
  where
    maxLiteral = foldl' (\acc clause -> foldl' (\x y -> max x (abs y)) acc clause) 0 clauses

-------------------------------------------------------------------------------
-- orphans
-------------------------------------------------------------------------------

deriving newtype instance Prim MiniSat.Lit
