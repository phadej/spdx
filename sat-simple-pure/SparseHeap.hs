{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
-- {-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
module SparseHeap (
    SparseHeap,
    sizeofSparseHeap,
    newSparseHeap,
    memberSparseHeap,
    insertSparseHeap,
    deleteSparseHeap,
    popSparseHeap,
    elemsSparseHeap,
    clearSparseHeap,
    extendSparseHeap,
    drainSparseHeap,
) where

import Control.Monad            (unless, when)
import Control.Monad.ST         (ST)
import Data.Bits
import Data.Primitive.PrimArray
import Data.Primitive.PrimVar

-- $setup
-- >>> import Control.Monad.ST (runST)

-- | Like sparse set https://research.swtch.com/sparse,
-- but also a min heap https://en.wikipedia.org/wiki/Heap_(data_structure)
--
-- i.e. pop returns minimum element.
--
data SparseHeap s = SH
    { size   :: !(PrimVar s Int)
    , dense  :: !(MutablePrimArray s Int)
    , sparse :: !(MutablePrimArray s Int)
    }

_invariant :: SparseHeap s -> ST s ()
_invariant SH {..} = do
    n         <- readPrimVar size
    capacity  <- getSizeofMutablePrimArray dense
    capacity' <- getSizeofMutablePrimArray sparse

    unless (n <= capacity && capacity == capacity') $
        error $ "capacities " ++ show (n, capacity, capacity')

    go capacity n 0
    heaps n 0
  where
    go capacity n i =
        if i >= n
        then return ()
        else do
            x <- readPrimArray dense i
            unless (x < capacity) $ error $ "x < capacity" ++ show (x, capacity)
            j <- readPrimArray sparse x
            unless (i == j) $ error $ "i == j" ++ show (i, j)
            go capacity n (i + 1)

    heaps n i =
        if i >= n
        then return ()
        else do
            x <- readPrimArray dense i
            heap n i x

    heap n i x = do
        let !j = 2 * i + 1
        let !k = 2 * i + 2

        when (j < n) $ do
            y <- readPrimArray dense j
            unless (x <= y) $ error $ "heap 2 " ++ show (x, y)

        when (k < n) $ do
            z <- readPrimArray dense k
            unless (x <= z) $ error $ "heap 2 " ++ show (x, z)


checkInvariant :: SparseHeap s -> ST s ()
-- checkInvariant = _invariant
checkInvariant _ = return ()
{-# INLINE checkInvariant #-}

-- | Create new sparse heap.
--
-- >>> runST $ newSparseHeap 100 >>= elemsSparseHeap
-- []
--
newSparseHeap
    :: Int -- ^ max integer
    -> ST s (SparseHeap s)
newSparseHeap capacity' = do
    let capacity = max 1024 capacity'
    size <- newPrimVar 0
    dense <- newPrimArray capacity
    sparse <- newPrimArray capacity
    return SH {..}

-- | Size of sparse heap.
--
-- >>> runST $ do { set <- newSparseHeap 100; mapM_ (insertSparseHeap set) [3,5,7,11,13,11]; sizeofSparseHeap set }
-- 5
--
sizeofSparseHeap :: SparseHeap s -> ST s Int
sizeofSparseHeap SH {..} = readPrimVar size

-- | Extend sparse heap to fit new capacity.
extendSparseHeap
    :: Int -- ^ new capacity
    -> SparseHeap s
    -> ST s (SparseHeap s)
extendSparseHeap capacity1 SH {..} = do
    capacity2 <- getSizeofMutablePrimArray dense
    let capacity' = max capacity2 capacity1 - 1
    let capacity = unsafeShiftL 1 (finiteBitSize (0 :: Int) - countLeadingZeros capacity')

    dense' <- resizeMutablePrimArray dense capacity
    sparse' <- resizeMutablePrimArray sparse capacity

    return SH { size, dense = dense', sparse = sparse' }

-- | Test for membership.
--
-- >>> runST $ do { set <- newSparseHeap 100; mapM_ (insertSparseHeap set) [3,5,7,11,13,11]; memberSparseHeap set 10 }
-- False
--
-- >>> runST $ do { set <- newSparseHeap 100; mapM_ (insertSparseHeap set) [3,5,7,11,13,11]; memberSparseHeap set 13 }
-- True
--
memberSparseHeap :: SparseHeap s -> Int -> ST s Bool
memberSparseHeap heap@SH {..} x = do
    checkInvariant heap

    n <- readPrimVar size
    i <- readPrimArray sparse x
    if 0 <= i && i < n
    then do
        x' <- readPrimArray dense i
        return (x' == x)
    else return False

-- | Insert into the heap.
--
-- >>> runST $ do { set <- newSparseHeap 100; mapM_ (insertSparseHeap set) [3,5,7,11,13,11]; elemsSparseHeap set }
-- [3,5,7,11,13]
--
insertSparseHeap :: SparseHeap s -> Int -> ST s ()
insertSparseHeap heap@SH {..} x = do
    checkInvariant heap

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
        swim (n + 1) dense sparse n x

-- | Delete element from the heap.
--
-- >>> runST $ do { set <- newSparseHeap 100; deleteSparseHeap set 10; elemsSparseHeap set }
-- []
--
-- >>> runST $ do { set <- newSparseHeap 100; mapM_ (insertSparseHeap set) [3,5,7,11,13,11]; deleteSparseHeap set 10; elemsSparseHeap set }
-- [3,5,7,11,13]
--
-- >>> runST $ do { set <- newSparseHeap 100; mapM_ (insertSparseHeap set) [3,5,7,11,13,11]; deleteSparseHeap set 13; elemsSparseHeap set }
-- [3,5,7,11]
--
-- >>> runST $ do { set <- newSparseHeap 100; mapM_ (insertSparseHeap set) [3,5,7,11,13,11]; deleteSparseHeap set 11; elemsSparseHeap set }
-- [3,5,7,13]
--
-- >>> runST $ do { set <- newSparseHeap 100; mapM_ (insertSparseHeap set) [3,5,7,11,13,11]; deleteSparseHeap set 3; elemsSparseHeap set }
-- [5,11,7,13]
--
-- >>> runST $ do { set <- newSparseHeap 100; mapM_ (insertSparseHeap set) $ [0,2..20] ++ [19,17..3]; deleteSparseHeap set 10; elemsSparseHeap set }
-- [0,2,4,5,3,17,12,9,6,8,20,19,18,15,13,14,11,16,7]
--
deleteSparseHeap :: SparseHeap s -> Int -> ST s ()
deleteSparseHeap heap@SH {..} x = do
    checkInvariant heap

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
        let !j = n - 1
        writePrimVar size j

        if i == j
        then return ()
        else do
            -- similarly as with pop, swap the last element, and sink it.
            y <- readPrimArray dense j
            swap' dense sparse i x j y
            sink j dense sparse i y

{-# INLINE swap' #-}
swap' :: MutablePrimArray s Int -> MutablePrimArray s Int -> Int -> Int -> Int -> Int -> ST s ()
swap' !dense !sparse !i !x !j !y = do
        writePrimArray dense j x
        writePrimArray dense i y
        writePrimArray sparse x j
        writePrimArray sparse y i

-- sift down
sink :: Int -> MutablePrimArray s Int -> MutablePrimArray s Int -> Int -> Int -> ST s ()
sink !n !dense !sparse !i !x
    | k < n
    = do
        l <- readPrimArray dense j
        r <- readPrimArray dense k
        -- traceM $ "sink " ++ show (i, j, k, x, l, r)

        if x <= l
        then do
            if x <= r
            then return ()
            else do
                 -- r < x <= l; swap x and r
                swap' dense sparse i x k r
                sink n dense sparse k x

        else do
            if l <= r
            then do
                -- l < x, l <= r; swap x and l
                swap' dense sparse i x j l
                sink n dense sparse j x

            else do
                -- r < l <= x; swap x and r
                swap' dense sparse i x k r
                sink n dense sparse k x

    | j < n
    = do
        l <- readPrimArray dense j
        if x <= l
        then return ()
        else do
            swap' dense sparse i x j l
            -- no need to sink further, as we sinked to the last element.

    | otherwise
    = return ()
  where
    !j = 2 * i + 1
    !k = j + 1

-- sift up
swim :: Int -> MutablePrimArray s Int -> MutablePrimArray s Int -> Int -> Int -> ST s ()
swim !_n !dense !sparse !i !x
    | i <= 0
    = return ()

    | otherwise
    = do
        -- j = floor (i - 1 / 2)
        let !j = unsafeShiftR (i - 1) 1
        y <- readPrimArray dense j

        unless (y <= x) $ do
            swap' dense sparse i x j y
            swim _n dense sparse j x

-- | Pop element from the heap.
--
-- >>> runST $ do { heap <- newSparseHeap 100; mapM_ (insertSparseHeap heap) [5,3,7,11,13,11]; popSparseHeap heap }
-- Just 3
--
popSparseHeap :: SparseHeap s -> ST s (Maybe Int)
popSparseHeap heap@SH {..} = do
    checkInvariant heap

    n <- readPrimVar size
    if n <= 0
    then return Nothing
    else do
        let !j = n - 1
        writePrimVar size j

        x <- readPrimArray dense 0
        y <- readPrimArray dense j
        swap' dense sparse 0 x j y
        sink j dense sparse 0 y

        checkInvariant heap
        return (Just x)

-- | Clear sparse heap.
--
-- >>> runST $ do { set <- newSparseHeap 100; mapM_ (insertSparseHeap set) [3,5,7,11,13,11]; clearSparseHeap set; elemsSparseHeap set }
-- []
--
clearSparseHeap :: SparseHeap s -> ST s ()
clearSparseHeap SH {..} = do
    writePrimVar size 0

-- | Elements of the heap.
--
-- Returns elements as they are internally stored.
--
elemsSparseHeap :: SparseHeap s -> ST s [Int]
elemsSparseHeap SH {..} = do
    n <- readPrimVar size
    go [] 0 n
  where
    go !acc !i !n
        | i < n
        = do
            x <- readPrimArray dense i
            go (x : acc) (i + 1) n

        | otherwise
        = return (reverse acc)

-- | Drain element from the heap.
--
-- >>> runST $ do { set <- newSparseHeap 100; mapM_ (insertSparseHeap set) [3,5,7,11,13,11]; drainSparseHeap set }
-- [3,5,7,11,13]
--
drainSparseHeap :: SparseHeap s -> ST s [Int]
drainSparseHeap heap = go id where
    go acc = popSparseHeap heap >>= \case
        Nothing -> return (acc [])
        Just x  -> go (acc . (x :))