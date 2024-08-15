{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DPLL.Level where

import Control.Monad.ST         (ST)
import Data.Coerce              (coerce)
import Data.Primitive.PrimArray
       (MutablePrimArray, getSizeofMutablePrimArray, newPrimArray, readPrimArray, setPrimArray, writePrimArray)
import Data.Primitive.Types     (Prim)

import DPLL.LitVar

newtype Level = Level Int
  deriving stock (Eq, Ord, Show)
  deriving newtype (Prim)

instance Enum Level where
    fromEnum = coerce
    toEnum = coerce

    succ (Level l) = Level (l + 1)
    pred (Level l) = Level (l - 1)

zeroLevel :: Level
zeroLevel = Level 0

isZeroLevel :: Level -> Bool
isZeroLevel (Level n) = n == 0

newtype Levels s = Levels (MutablePrimArray s Level)

getLevel :: Levels s -> Lit -> ST s Level
getLevel (Levels level) (MkLit l) =
    readPrimArray level (lit_to_var l)

setLevel :: Levels s -> Lit -> Level -> ST s ()
setLevel (Levels levels) (MkLit l) d = do
    writePrimArray levels (lit_to_var l) d

clearLevels :: Levels s -> ST s ()
clearLevels (Levels levels) = do
    size <- getSizeofMutablePrimArray levels
    setPrimArray levels 0 size (Level 0)

newLevels :: Int -> ST s (Levels s)
newLevels size = do
    levels <- newPrimArray size
    setPrimArray levels 0 size (Level 0)
    return (Levels levels)
