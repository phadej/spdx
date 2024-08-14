module Main where

import Control.Monad.ST (runST)
import System.Exit (exitFailure)

import DPLL

main :: IO ()
main = if example1 then exitFailure else return ()

-- this example is unsat
example1 :: Bool
example1 = runST $ do
    s <- newSolver

    a <- newLit s
    b <- newLit s
    c <- newLit s
    d <- newLit s
    e <- newLit s
    f <- newLit s

    _ <- addClause s [a, b, c]
    _ <- addClause s [a, b, neg c]
    _ <- addClause s [neg b, d]
    _ <- addClause s [a, neg b, neg d]
    _ <- addClause s [neg a, e, f]
    _ <- addClause s [neg a, e, neg f]
    _ <- addClause s [neg e, neg f]
    _ <- addClause s [neg a, neg e, f]

    solve s