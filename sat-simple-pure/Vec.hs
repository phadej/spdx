module Vec (
    Vec,
    newVec,
    sizeofVec,
    insertVec,
    readVec,
    writeVec,
    shrinkVec,
) where

import Control.Monad (forM_)
import Control.Monad.ST (ST)
import Data.STRef
import Data.Primitive.Array
import Unsafe.Coerce (unsafeCoerce)

data Vec s a = Vec {-# UNPACK #-} !(STRef s Int) {-# UNPACK #-} !(MutableArray s a)

newVec
    :: Int             -- ^ capacity
    -> ST s (Vec s a)
newVec capacity = do
    arr <- newArray capacity unused
    size <- newSTRef 0
    return (Vec size arr)

unused :: a
unused = unsafeCoerce ()

sizeofVec :: Vec s a -> ST s Int
sizeofVec (Vec size _) = readSTRef size

-- | Insert at the end: @push_back@
--
-- The new vector may be returned.
-- The vec is done such way, as we use it in mutable context already,
-- so we don't need an extra STRef.
insertVec :: Vec s a -> a -> ST s (Vec s a)
insertVec vec@(Vec sizeRef arr) x = do
    size <- readSTRef sizeRef
    let !capacity = sizeofMutableArray arr
    if size < capacity
    then do
        writeArray arr size x
        writeSTRef sizeRef (size + 1)
        return vec

    else do
        new <- newArray (capacity * 2) unused
        copyMutableArray new 0 arr 0 size
        writeArray new size x
        writeSTRef sizeRef (size + 1)
        return (Vec sizeRef new)

readVec :: Vec s a -> Int -> ST s a
readVec (Vec _ arr) i = readArray arr i

writeVec :: Vec s a -> Int -> a -> ST s ()
writeVec (Vec _ arr) i x = writeArray arr i x

-- | Shrink vector. New size should be smaller than the current.
shrinkVec :: Vec s a -> Int -> ST s ()
shrinkVec (Vec sizeRef arr) newSize = do
    size <- readSTRef sizeRef
    forM_ [newSize .. size - 1] $ \i -> writeArray arr i unused
    writeSTRef sizeRef newSize
