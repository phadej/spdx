{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
-- |
--
-- Inspired by <http://www.well-typed.com/blog/2014/12/simple-smt-solver/ Simple SMT Solver>.
--
-- In future this module will probably be moved into separate package.
module Distribution.SPDX.Extra.Internal
  (LatticeSyntax(..), dual, freeVars, equivalent, preorder, satisfiable) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Data
import Data.Foldable
import Data.Traversable
import Prelude                          hiding (all, or)

import qualified Data.Map.Strict as Map

data LatticeSyntax a = LVar a
                     | LBound Bool
                     | LJoin (LatticeSyntax a) (LatticeSyntax a)
                     | LMeet (LatticeSyntax a) (LatticeSyntax a)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Typeable, Data)

instance Applicative LatticeSyntax where
  pure  = return
  (<*>) = ap

instance Monad LatticeSyntax where
  return = LVar
  LVar x    >>= f = f x
  LBound b  >>= _ = LBound b
  LJoin a b >>= f = LJoin (a >>= f) (b >>= f)
  LMeet a b >>= f = LMeet (a >>= f) (b >>= f)

freeVars :: LatticeSyntax a -> [a]
freeVars = toList

dual :: LatticeSyntax a -> LatticeSyntax a
dual (LVar v) = LVar v
dual (LBound t) = LBound $ not t
dual (LJoin a b) = LMeet (dual a) (dual b)
dual (LMeet a b) = LJoin (dual a) (dual b)

-- | Test for equivalence.
--
-- >>> equivalent (LMeet (LVar 'a') (LVar 'b')) (LMeet (LVar 'b') (LVar 'a'))
-- True
--
-- >>> equivalent (LVar 'a') (LMeet (LVar 'a') (LVar 'a'))
-- True
--
-- >>> equivalent (LMeet (LVar 'a') (LVar 'b')) (LMeet (LVar 'b') (LVar 'b'))
-- False
equivalent :: Ord a => LatticeSyntax a -> LatticeSyntax a -> Bool
equivalent a b = all (uncurry (==)) . runEval $ p
  where p = (,) <$> evalLattice a <*> evalLattice b

-- | Test for preorder.
--
-- @ a ≤ b ⇔ a ∨ b ≡ b ⇔ a ≡ a ∧ b @
--
-- >>> preorder (LVar 'a' `LMeet` LVar 'b') (LVar 'a')
-- True
--
-- >>> preorder (LVar 'a') (LVar 'a' `LMeet` LVar 'b')
-- False
preorder :: Ord a => LatticeSyntax a -> LatticeSyntax a -> Bool
preorder a b = (a `LJoin` b) `equivalent` b

-- | Return `True` if for some variable assigment expression evaluates to `True`.
satisfiable :: Ord a => LatticeSyntax a -> Bool
satisfiable = or . runEval . evalLattice

newtype Eval v a = Eval { unEval :: StateT (Map.Map v Bool) [] a }

instance Functor (Eval v) where
  fmap = liftM

instance Applicative (Eval v) where
  pure = return
  (<*>) = ap

instance Alternative (Eval v) where
  empty = mzero
  (<|>) = mplus

instance Monad (Eval v) where
  return = Eval . return
  Eval m >>= k = Eval $ m >>= unEval . k

instance MonadPlus (Eval v) where
  mzero = Eval mzero
  Eval a `mplus` Eval b = Eval $ a `mplus` b

runEval :: Eval v a -> [a]
runEval act = evalStateT (unEval act) Map.empty

evalLattice :: Ord v => LatticeSyntax v -> Eval v Bool
evalLattice (LVar v)    = guess v
evalLattice (LBound b)  = return b
evalLattice (LJoin a b) = evalLattice a ||^ evalLattice b
evalLattice (LMeet a b) = evalLattice a &&^ evalLattice b

guess :: Ord v => v -> Eval v Bool
guess v = Eval $ do
  st <- get
  let remember b = put (Map.insert v b st) >> return b
  case Map.lookup v st of
    Just b  -> return b
    Nothing -> remember True <|> remember False

-- From Control.Monad.Extra of extra

-- | Like @if@, but where the test can be monadic.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b' <- b; if b' then t else f

-- | The lazy '||' operator lifted to a monad. If the first
--   argument evaluates to 'True' the second argument will not
--   be evaluated.
--
-- > Just True  ||^ undefined  == Just True
-- > Just False ||^ Just True  == Just True
-- > Just False ||^ Just False == Just False
(||^) :: Monad m => m Bool -> m Bool -> m Bool
(||^) a b = ifM a (return True) b

-- | The lazy '&&' operator lifted to a monad. If the first
--   argument evaluates to 'False' the second argument will not
--   be evaluated.
--
-- > Just False &&^ undefined  == Just False
-- > Just True  &&^ Just True  == Just True
-- > Just True  &&^ Just False == Just False
(&&^) :: Monad m => m Bool -> m Bool -> m Bool
(&&^) a b = ifM a b (return False)
