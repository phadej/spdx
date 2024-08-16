module Main where

import Control.Monad.ST   (runST)
import System.Environment (getArgs)
import System.Exit        (exitFailure)

import DPLL

main :: IO ()
main = do
    args <- getArgs
    case args of
        "pigeonhole" : _ -> main' example2
        _                -> main' example1

main' :: Bool -> IO ()
main' example = do
    print example
    if example then exitFailure else return ()

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

-- pigeonhole: 3 pigeons, 2 holes
example2 :: Bool
example2  = runST $ do
    s <- newSolver

    p11 <- newLit s
    p12 <- newLit s
    p21 <- newLit s
    p22 <- newLit s
    p31 <- newLit s
    p32 <- newLit s

    -- each pigeon is somewhere
    _ <- addClause s [p11, p12]
    _ <- addClause s [p21, p22]
    _ <- addClause s [p31, p32]

    -- there's only one pigeon in each hole
    _ <- addClause s [neg p11, neg p21]
    _ <- addClause s [neg p21, neg p31]
    _ <- addClause s [neg p31, neg p11]

    _ <- addClause s [neg p12, neg p22]
    _ <- addClause s [neg p22, neg p32]
    _ <- addClause s [neg p32, neg p12]

    solve s
