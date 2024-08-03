{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DPLL.LitVar where

import Data.Bits                (complementBit, testBit, unsafeShiftL, unsafeShiftR)
import Data.Primitive.PrimArray (PrimArray)
import Data.Primitive.Types     (Prim)
import Unsafe.Coerce            (unsafeCoerce)

-------------------------------------------------------------------------------
-- Literals
-------------------------------------------------------------------------------

-- | Literals
newtype Lit = MkLit Int
  deriving (Eq, Ord)

instance Show Lit where
    showsPrec d (MkLit l)
        | testBit l 0 = showParen (d > 10) $ showChar '-' . shows (lit_to_var l)
        | otherwise   = shows (lit_to_var l)

deriving newtype instance Prim Lit

-- | Negate literal
neg :: Lit -> Lit
neg (MkLit l) = MkLit (complementBit l 0)

-- unLit :: Lit -> Int
-- unLit (MkLit l) = l

coercePrimArrayLit :: PrimArray Int -> PrimArray Lit
coercePrimArrayLit = unsafeCoerce

-------------------------------------------------------------------------------
-- Variables
-------------------------------------------------------------------------------

newtype Var = MkVar Int
  deriving (Eq, Ord, Show)

litToVar :: Lit -> Var
litToVar (MkLit l) = MkVar (lit_to_var l)

lit_to_var :: Int -> Int
lit_to_var l = unsafeShiftR l 1

varToLit :: Var -> Lit
varToLit (MkVar x) = MkLit (var_to_lit x)

var_to_lit :: Int -> Int
var_to_lit l = unsafeShiftL l 1
