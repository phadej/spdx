{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}
module UnliftedSTRef (
    USTRef,
    newUSTRef,
    readUSTRef,
    writeUSTRef,
) where

import Data.Kind (Type)
import GHC.Exts  (MutVar#, UnliftedType, newMutVar#, readMutVar#, writeMutVar#)
import GHC.ST    (ST (..))

import Lifted

type USTRef :: Type -> UnliftedType -> Type
data USTRef s a = USTRef (MutVar# s a)

newUSTRef :: a -> ST s (USTRef s a)
newUSTRef x = ST $ \s1# ->
    case newMutVar# x s1# of { (# s2#, var# #) ->
    (# s2#, USTRef var# #) }

readUSTRef :: USTRef s a -> ST s (Lifted a)
readUSTRef (USTRef ref) = ST $ \s1 ->
    case readMutVar# ref s1 of
        (# s2, x #) -> (# s2, Lift x #)

writeUSTRef :: USTRef s a -> a -> ST s ()
writeUSTRef (USTRef var#) val = ST $ \s1# ->
    case writeMutVar# var# val s1#      of { s2# ->
    (# s2#, () #) }
