{-# LANGUAGE CPP #-}
module DPLL.VarSet where

-- #define INTSET_VARS

import Control.Monad.ST (ST)
import Data.Coerce      (coerce)

import DPLL.LitVar

#ifdef INTSET_VARS
import qualified Data.IntSet as IS
import           Data.STRef  (modifySTRef)
#else
import SparseMaxHeap
#endif

-------------------------------------------------------------------------------
-- VarSet
-------------------------------------------------------------------------------

#ifdef INTSET_VARS
newtype VarSet s = VS (STRef s IS.IntSet)

newVarSet :: ST s (VarSet s)
newVarSet = VS <$> newSTRef IS.empty

sizeofVarSet :: VarSet s -> ST s Int
sizeofVarSet (VS xs) = IS.size <$> readSTRef xs

extendVarSet :: Int -> VarSet s -> ST s (VarSet s)
extendVarSet _ x = return x

weightVarSet :: Var -> (Int -> Int) -> VarSet s -> ST s ()
weightVarSet _ _ _ = return ()

insertVarSet :: Var -> VarSet s -> ST s ()
insertVarSet (MkVar x) (VS xs) = modifySTRef xs (IS.insert x)

deleteVarSet :: Var -> VarSet s -> ST s ()
deleteVarSet (MkVar x) (VS xs) = modifySTRef xs (IS.delete x)

clearVarSet :: VarSet s -> ST s ()
clearVarSet (VS xs) = writeSTRef xs IS.empty

minViewVarSet :: VarSet s -> ST s r -> (Var -> ST s r) -> ST s r
minViewVarSet (VS xs) no yes = do
    is <- readSTRef xs
    case IS.minView is of
        Nothing -> no
        Just (x, is') -> do
            writeSTRef xs is'
            yes (MkVar x)

#else

newtype VarSet s = VS (SparseHeap s)

sizeofVarSet :: VarSet s -> ST s Int
sizeofVarSet (VS xs) = sizeofSparseHeap xs

newVarSet :: ST s (VarSet s)
newVarSet = VS <$> newSparseHeap 0

extendVarSet :: Int -> VarSet s -> ST s (VarSet s)
extendVarSet capacity (VS xs) = VS <$> extendSparseHeap capacity xs

weightVarSet :: Var -> (Int -> Int) -> VarSet s -> ST s ()
weightVarSet (MkVar x) f (VS xs) = modifyWeightSparseHeap xs x f
{-# INLINE weightVarSet #-}

insertVarSet :: Var -> VarSet s -> ST s ()
insertVarSet (MkVar x) (VS xs) = do
    insertSparseHeap xs x

deleteVarSet :: Var -> VarSet s -> ST s ()
deleteVarSet (MkVar x) (VS xs) = do
    deleteSparseHeap xs x

clearVarSet :: VarSet s -> ST s ()
clearVarSet (VS xs) = clearSparseHeap xs

{-# INLINE minViewVarSet #-}
minViewVarSet :: VarSet s -> ST s r -> (Var -> ST s r) -> ST s r
minViewVarSet (VS xs) no yes = popSparseHeap_ xs no (coerce yes)

#endif
