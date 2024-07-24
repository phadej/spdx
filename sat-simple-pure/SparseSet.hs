module SparseSet (
    SparseSet,
    sizeofSparseSet,
    newSparseSet,
    memberSparseSet,
    insertSparseSet,
    deleteSparseSet,
    popSparseSet,
    elemsSparseSet,
    clearSparseSet,
) where

import Control.Monad            (unless)
import Control.Monad.ST         (ST)
import Data.Primitive.PrimArray
import Data.Primitive.PrimVar

-- $setup
-- >>> import Control.Monad.ST (runST)

-- | https://research.swtch.com/sparse
--
-- An 'Int' set which support efficient popping ('popSparseSet').
data SparseSet s = SS (PrimVar s Int) (MutablePrimArray s Int) (MutablePrimArray s Int)

_invariant :: SparseSet s -> ST s ()
_invariant (SS size dense sparse) = do
    n         <- readPrimVar size
    capacity  <- getSizeofMutablePrimArray dense
    capacity' <- getSizeofMutablePrimArray sparse

    unless (n <= capacity && capacity == capacity') $
        error $ "capacities " ++ show (n, capacity, capacity')

    go capacity n 0
  where
    go capacity n i =
        if i >= n
        then return ()
        else do
            x <- readPrimArray dense i
            unless (x < capacity) $ error $ "x < capacity" ++ show (x, capacity)
            j <- readPrimArray sparse x
            unless (i == j) $ error $ "i == j" ++ show (i, j)

checkInvariant :: SparseSet s -> ST s ()
-- checkInvariant = _invariant
checkInvariant _ = return ()

-- | Create new sparse set
--
-- >>> runST $ newSparseSet 100 >>= elemsSparseSet
-- []
newSparseSet
    :: Int -- ^ max integer
    -> ST s (SparseSet s)
newSparseSet capacity' = do
    let capacity = max 1024 capacity'
    size <- newPrimVar 0
    dense <- newPrimArray capacity
    sparse <- newPrimArray capacity
    return (SS size dense sparse)

-- | Size of sparse set.
--
-- >>> runST $ do { set <- newSparseSet 100; mapM_ (insertSparseSet set) [3,5,7,11,13,11]; sizeofSparseSet set }
-- 5
--
sizeofSparseSet :: SparseSet s -> ST s Int
sizeofSparseSet (SS size _ _) = readPrimVar size

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
    if 0 <= i && i < n
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
insertSparseSet set@(SS size dense sparse) x = do
    checkInvariant set
    n <- readPrimVar size
    i <- readPrimArray sparse x
    if 0 <= i && i < n
    then do
        x' <- readPrimArray dense i
        if x == x' then return () else insert n
    else insert n
  where
    {-# INLINE insert #-}
    insert n = do
        writePrimArray dense n x
        writePrimArray sparse x n
        writePrimVar size (n + 1)

-- | Delete from set
--
-- >>> runST $ do { set <- newSparseSet 100; deleteSparseSet set 10; elemsSparseSet set }
-- []
--
-- >>> runST $ do { set <- newSparseSet 100; mapM_ (insertSparseSet set) [3,5,7,11,13,11]; deleteSparseSet set 10; elemsSparseSet set }
-- [13,11,7,5,3]
--
-- >>> runST $ do { set <- newSparseSet 100; mapM_ (insertSparseSet set) [3,5,7,11,13,11]; deleteSparseSet set 13; elemsSparseSet set }
-- [11,7,5,3]
--
-- >>> runST $ do { set <- newSparseSet 100; mapM_ (insertSparseSet set) [3,5,7,11,13,11]; deleteSparseSet set 11; elemsSparseSet set }
-- [13,7,5,3]
--
deleteSparseSet :: SparseSet s -> Int -> ST s ()
deleteSparseSet set@(SS size dense sparse) x = do
    checkInvariant set
    n <- readPrimVar size
    i <- readPrimArray sparse x
    if 0 <= i && i < n
    then do
        x' <- readPrimArray dense i
        if x == x' then delete i n else return ()
    else return ()
  where
    {-# INLINE delete #-}
    delete i n = do
        writePrimVar size (n - 1)
        swap dense sparse i x (n - 1)

{-# INLINE swap #-}
swap :: MutablePrimArray s Int -> MutablePrimArray s Int -> Int -> Int -> Int -> ST s ()
swap !dense !sparse !i !x !j
    | i == j
    = return ()

    | otherwise = do
        -- x <- readPrimArray dense i
        y <- readPrimArray dense j

        writePrimArray dense j x
        writePrimArray dense i y
        writePrimArray sparse x j
        writePrimArray sparse y i

-- | Pop element from the set.
--
-- >>> runST $ do { set <- newSparseSet 100; mapM_ (insertSparseSet set) [3,5,7,11,13,11]; popSparseSet set }
-- Just 13
--
popSparseSet :: SparseSet s -> ST s (Maybe Int)
popSparseSet set@(SS size dense _sparse) = do
    checkInvariant set
    n <- readPrimVar size
    if n <= 0
    then return Nothing
    else do
        let !n' = n - 1
        x <- readPrimArray dense n'
        writePrimVar size n'
        return (Just x)
--
-- | Clear sparse set.
--
-- >>> runST $ do { set <- newSparseSet 100; mapM_ (insertSparseSet set) [3,5,7,11,13,11]; clearSparseSet set; elemsSparseSet set }
-- []
--
clearSparseSet :: SparseSet s -> ST s ()
clearSparseSet (SS size _ _) = do
    writePrimVar size 0

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
