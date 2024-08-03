module DPLL.LitTable where

import Control.Monad.ST     (ST)
import Data.Primitive.Array (MutableArray, newArray, readArray, sizeofMutableArray, writeArray)

import DPLL.LitVar

-------------------------------------------------------------------------------
-- LitTable
-------------------------------------------------------------------------------

newtype LitTable s a = LitT (MutableArray s a)

newLitTable :: Int -> a -> ST s (LitTable s a)
newLitTable !size x = do
    xs <- newArray size x
    return (LitT xs)

sizeofLitTable :: LitTable s a -> ST s Int
sizeofLitTable (LitT arr) = return (sizeofMutableArray arr)

readLitTable :: LitTable s a -> Lit -> ST s a
readLitTable (LitT xs) (MkLit l) = readArray xs l

writeLitTable :: LitTable s a -> Lit -> a -> ST s ()
writeLitTable (LitT xs) (MkLit l) x = writeArray xs l x
