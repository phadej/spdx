module SparseSet (
    SparseSet,
    newSparseSet,
    memberSparseSet,
    insertSparseSet,
    popSparseSet,
    elemsSparseSet,
) where

import Control.Monad.ST (ST)
import Data.Primitive.PrimArray
import Data.Primitive.PrimVar

-- $setup
-- >>> import Control.Monad.ST (runST)

-- | https://research.swtch.com/sparse
--
-- An 'Int' set which support efficient popping ('popSparseSet').
data SparseSet s = SS (PrimVar s Int) (MutablePrimArray s Int) (MutablePrimArray s Int)

-- | Create new sparse set
--
-- >>> runST $ newSparseSet 100 >>= elemsSparseSet
-- []
newSparseSet
    :: Int -- ^ max integer
    -> ST s (SparseSet s)
newSparseSet capacity = do
    size <- newPrimVar 0
    dense <- newPrimArray capacity
    sparse <- newPrimArray capacity
    return (SS size dense sparse)

-- | Test for membership
--
-- >>> runST $ do { set <- newSparseSet 100; mapM_ (insertSparseSet set) [3,5,7,11,13,11]; memberSparseSet set 10 }
-- False
--
-- >>> runST $ do { set <- newSparseSet 100; mapM_ (insertSparseSet set) [3,5,7,11,13,11]; memberSparseSet set 13 }
-- True
--
memberSparseSet :: SparseSet s -> Int -> ST s Bool
memberSparseSet (SS size dense sparse) x = do
    n <- readPrimVar size
    i <- readPrimArray sparse x
    if i < n
    then do
        x' <- readPrimArray dense i
        return (x' == x)
    else return False

-- | Insert into set
--
-- >>> runST $ do { set <- newSparseSet 100; mapM_ (insertSparseSet set) [3,5,7,11,13,11]; elemsSparseSet set }
-- [13,11,7,5,3]
--
insertSparseSet :: SparseSet s -> Int -> ST s ()
insertSparseSet (SS size dense sparse) x = do
    n <- readPrimVar size
    i <- readPrimArray sparse x
    if i < n
    then do
        x' <- readPrimArray dense i
        if x == x' then return () else insert n
    else insert n
  where
    insert n = do
        writePrimArray dense n x
        writePrimArray sparse x n
        writePrimVar size (n + 1)

-- | Pop element from the set
--
-- >>> runST $ do { set <- newSparseSet 100; mapM_ (insertSparseSet set) [3,5,7,11,13,11]; popSparseSet set }
-- Just 13
--
popSparseSet :: SparseSet s -> ST s (Maybe Int)
popSparseSet (SS size dense _sparse) = do
    n <- readPrimVar size
    if n <= 0
    then return Nothing
    else do
        let !n' = n - 1
        i <- readPrimArray dense n'
        writePrimVar size n'
        return (Just i)

-- | Elements of the set
elemsSparseSet :: SparseSet s -> ST s [Int]
elemsSparseSet (SS size dense _sparse) = do
    n <- readPrimVar size
    go [] 0 n
  where
    go !acc !i !n
        | i < n
        = do
            x <- readPrimArray dense i
            go (x : acc) (i + 1) n
        
        | otherwise
        = return acc