module DPLL.Clause2 where

import Data.Primitive.PrimArray (PrimArray, emptyPrimArray, foldrPrimArray, traversePrimArray_)

import DPLL.LitVar

data Clause2 = MkClause2 {-# UNPACK #-} !Lit {-# UNPACK #-} !Lit {-# UNPACK #-} !(PrimArray Lit)
  deriving Show

litInClause :: Lit -> Clause2 -> Bool
litInClause l (MkClause2 l1 l2 ls) =
    l == l1 || l == l2 || foldrPrimArray (\l' next -> l' == l || next) False ls

forLitInClause2_ :: Applicative f => Clause2 -> (Lit -> f b) -> f ()
forLitInClause2_ (MkClause2 l1 l2 ls) f =
    f l1 *> f l2 *> traversePrimArray_ f ls

nullClause :: Clause2
nullClause = MkClause2 (MkLit 1) (MkLit 1) emptyPrimArray

isNullClause :: Clause2 -> Bool
isNullClause (MkClause2 l1 l2 _) = l1 == l2
