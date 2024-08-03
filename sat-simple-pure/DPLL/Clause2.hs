module DPLL.Clause2 where

import Control.Monad            (when)
import Control.Monad.ST         (ST)
import Data.Primitive.PrimArray (PrimArray, emptyPrimArray, foldrPrimArray, indexPrimArray, sizeofPrimArray)

import DPLL.LitVar

data Clause2 = MkClause2 {-# UNPACK #-} !Lit {-# UNPACK #-} !Lit {-# UNPACK #-} !(PrimArray Lit)
  deriving Show

litInClause :: Lit -> Clause2 -> Bool
litInClause l (MkClause2 l1 l2 ls) =
    l == l1 || l == l2 || foldrPrimArray (\l' next -> l' == l || next) False ls

forLitInClause2_ :: Clause2 -> (Lit -> ST s ()) -> ST s ()
forLitInClause2_ (MkClause2 l1 l2 ls) f = do
    f l1
    f l2
    -- manually inlines traversePrimArray_
    forLitInClause2Go 0
  where
    !sz = sizeofPrimArray ls
    forLitInClause2Go !i = when (i < sz) $ f (indexPrimArray ls i) >> forLitInClause2Go (i + 1)
{-# INLINE forLitInClause2_ #-}

nullClause :: Clause2
nullClause = MkClause2 (MkLit 1) (MkLit 1) emptyPrimArray

isNullClause :: Clause2 -> Bool
isNullClause (MkClause2 l1 l2 _) = l1 == l2
