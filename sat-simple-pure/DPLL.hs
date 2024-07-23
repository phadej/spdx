{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
module DPLL (
    Solver,
    newSolver,
    Lit (..),
    newLit,
    neg,
    addClause,
    solve,
    simplify,
    modelValue,
) where

#define TWO_WATCHED_LITERALS

import Control.Monad.ST         (ST)
import Data.Bits                (complementBit, testBit, unsafeShiftL, unsafeShiftR)
import Data.Functor             ((<&>))
import Data.IntSet              (IntSet)
import Data.Primitive.PrimArray (PrimArray, indexPrimArray, primArrayFromList, sizeofPrimArray)
import Data.Primitive.Types     (Prim)
import Data.STRef               (STRef, modifySTRef, newSTRef, readSTRef, writeSTRef)
import Data.Word                (Word8)

import Data.Primitive.ByteArray
       (MutableByteArray (..), MutableByteArray#, getSizeofMutableByteArray, newByteArray, readByteArray,
       resizeMutableByteArray, shrinkMutableByteArray, writeByteArray)

import Lifted
import UnliftedSTRef

#ifdef TWO_WATCHED_LITERALS
import Control.Monad        (forM_)
import Data.Primitive.Array (MutableArray, newArray, readArray, sizeofMutableArray, writeArray)
import Vec
#endif

import qualified Data.IntSet as IntSet

-- import Debug.Trace

-------------------------------------------------------------------------------
-- Literals
-------------------------------------------------------------------------------

-- | Literals
newtype Lit = MkLit Int
  deriving (Eq, Ord, Show)

deriving newtype instance Prim Lit

-- | Negate literal
neg :: Lit -> Lit
neg (MkLit l) = MkLit (complementBit l 0)

-- unLit :: Lit -> Int
-- unLit (MkLit l) = l

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

-------------------------------------------------------------------------------
-- LBool
-------------------------------------------------------------------------------

data LBool = LFalse | LTrue | LUndef
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- Partial Assignment
-------------------------------------------------------------------------------

newtype PartialAssignment s = PA (USTRef s (MutableByteArray# s))

newPartialAssignment :: ST s (PartialAssignment s)
newPartialAssignment = do
    arr@(MutableByteArray arr#) <- newByteArray 4096
    shrinkMutableByteArray arr 0
    ref <- newUSTRef arr#
    return (PA ref)

clearPartialAssignment :: PartialAssignment s -> ST s ()
clearPartialAssignment (PA ref) = do
    Lift arr <- readUSTRef ref
    shrinkMutableByteArray (MutableByteArray arr) 0

extendPartialAssignment :: PartialAssignment s -> ST s ()
extendPartialAssignment (PA ref) = do
    Lift arr <- readUSTRef ref
    size <- getSizeofMutableByteArray (MutableByteArray arr)
    arr'@(MutableByteArray arr#) <- resizeMutableByteArray (MutableByteArray arr) (size + 1)
    writeByteArray arr' size (0xff :: Word8)
    writeUSTRef ref arr#

lookupPartialAssignment :: Lit -> PartialAssignment s -> ST s LBool
lookupPartialAssignment (MkLit l) (PA ref) = do
    Lift arr <- readUSTRef ref
    readByteArray @Word8 (MutableByteArray arr) (lit_to_var l) >>= \case
        0x0 -> return (if y then LFalse else LTrue)
        0x1 -> return (if y then LTrue else LFalse)
        _   -> return LUndef
  where
    y = testBit l 0
    {-# INLINE y #-}

insertPartialAssignment :: Lit -> PartialAssignment s -> ST s ()
insertPartialAssignment (MkLit l) (PA ref) = do
    Lift arr <- readUSTRef ref
    writeByteArray (MutableByteArray arr) (lit_to_var l) (if testBit l 0 then 0x1 else 0x0 :: Word8)

deletePartialAssignment :: Lit -> PartialAssignment s -> ST s ()
deletePartialAssignment (MkLit l) (PA ref) = do
    Lift arr <- readUSTRef ref
    writeByteArray (MutableByteArray arr) (lit_to_var l) (0xff :: Word8)

-------------------------------------------------------------------------------
-- VarSet
-------------------------------------------------------------------------------

newtype VarSet = VS IntSet

emptyVarSet :: VarSet
emptyVarSet = VS IntSet.empty

insertVarSet :: Var -> VarSet -> VarSet
insertVarSet (MkVar x) (VS xs) = VS (IntSet.insert x xs)

deleteVarSet :: Var -> VarSet -> VarSet
deleteVarSet (MkVar x) (VS xs) = VS (IntSet.delete x xs)

minViewVarSet :: VarSet -> Maybe (Var, VarSet)
minViewVarSet (VS xs) = case IntSet.minView xs of
    Nothing -> Nothing
    Just (x, xs') -> Just (MkVar x, VS xs')

-------------------------------------------------------------------------------
-- LitSet
-------------------------------------------------------------------------------

newtype LitSet = LS IntSet

emptyLitSet :: LitSet
emptyLitSet = LS IntSet.empty

insertLitSet :: Lit -> LitSet -> LitSet
insertLitSet (MkLit l) (LS ls) = LS (IntSet.insert l ls)

minViewLitSet :: LitSet -> Maybe (Lit, LitSet)
minViewLitSet (LS xs) = case IntSet.minView xs of
    Nothing -> Nothing
    Just (x, xs') -> Just (MkLit x, LS xs')

-------------------------------------------------------------------------------
-- Clauses
-------------------------------------------------------------------------------

type Clauses = [Clause2]

-------------------------------------------------------------------------------
-- ClauseDB
-------------------------------------------------------------------------------

#ifdef TWO_WATCHED_LITERALS

newtype ClauseDB s = CDB (MutableArray s (Vec s Watch))

data Watch = W !Lit !Clause2

newClauseDB :: Int -> ST s (ClauseDB s)
newClauseDB size = do
    arr <- newArray size undefined

    forM_ [0 .. size - 1] $ \i -> do
        vec <- newVec 16
        writeArray arr i vec

    return (CDB arr)

insertClauseDB :: Lit -> Lit -> Clause2 -> ClauseDB s -> ST s ()
insertClauseDB l1 l2 clause cdb = do
    insertWatch l1 (W l2 clause) cdb
    insertWatch l2 (W l1 clause) cdb

insertWatch :: Lit -> Watch -> ClauseDB s -> ST s ()
insertWatch (MkLit l) !w (CDB cdb) = do
    ws  <- readArray cdb l
    ws' <- insertVec ws w
    writeArray cdb l ws'

lookupClauseDB :: Lit -> ClauseDB s -> ST s (Vec s Watch)
lookupClauseDB (MkLit l) (CDB arr) = do
    readArray arr l

_sizeofClauseDB :: ClauseDB s -> ST s Int
_sizeofClauseDB (CDB arr) = go 0 0 (sizeofMutableArray arr)
  where
    go !acc !i !size
        | i < size
        = do
            vec <- readArray arr i
            elm <- sizeofVec vec
            go (acc + elm) (i + 1) size

        | otherwise
        = return acc

#else
type ClauseDB s = [Clause2]
#endif

-------------------------------------------------------------------------------
-- Clause
-------------------------------------------------------------------------------

type Clause = [Lit]

data Satisfied
    = Satisfied
    | Conflicting
    | Unit !Lit
    | Unresolved !Clause2
  deriving Show

satisfied :: PartialAssignment s -> Clause -> ST s Satisfied
satisfied pa = go0 where
    go0 []     = return Conflicting
    go0 (l:ls) = lookupPartialAssignment l pa >>= \case
        LUndef -> go1 l ls
        LTrue  -> return Satisfied
        LFalse -> go0 ls

    go1 l1 []     = return (Unit l1)
    go1 l1 (l:ls) = lookupPartialAssignment l pa >>= \case
        LUndef -> go2 l1 l [] ls
        LTrue  -> return Satisfied
        LFalse -> go1 l1 ls

    go2 l1 l2 acc []     = return (Unresolved (MkClause2 l1 l2 (primArrayFromList acc)))
    go2 l1 l2 acc (l:ls) = lookupPartialAssignment l pa >>= \case
        LUndef -> go2 l1 l2 (l : acc) ls
        LTrue  -> return Satisfied
        LFalse -> go2 l1 l2 acc ls

-------------------------------------------------------------------------------
-- Clause2
-------------------------------------------------------------------------------

data Clause2 = MkClause2 {-# UNPACK #-} !Lit {-# UNPACK #-} !Lit {-# UNPACK #-} !(PrimArray Lit)
  deriving Show

data Satisfied_
    = Satisfied_
    | Conflicting_
    | Unit_ !Lit
    | Unresolved_ !Lit !Lit
  deriving Show

satisfied2_ :: PartialAssignment s -> Clause2 -> ST s Satisfied_
satisfied2_ pa (MkClause2 l1 l2 ls) = go0
  where
    !len = sizeofPrimArray ls

    -- initial state
    go0 = lookupPartialAssignment l1 pa >>= \case
        LUndef -> go1
        LTrue  -> return Satisfied_
        LFalse -> go2

    -- l1 -> Undef
    go1 = lookupPartialAssignment l2 pa >>= \case
        LUndef -> goTwo l1 l2 0
        LTrue  -> return Satisfied_
        LFalse -> goOne l1 0

    -- l1 -> False
    go2 = lookupPartialAssignment l2 pa >>= \case
        LUndef -> goOne l2 0
        LTrue  -> return Satisfied_
        LFalse -> goNone 0

    goNone i
        | i >= len
        = return Conflicting_

        | otherwise
        , let !l = indexPrimArray ls i
        = lookupPartialAssignment l pa >>= \case
            LUndef -> goOne l (i + 1)
            LTrue  -> return Satisfied_
            LFalse -> goNone (i + 1)

    goOne k1 i
        | i >= len
        = return (Unit_ k1)

        | otherwise
        , let !l = indexPrimArray ls i
        = lookupPartialAssignment l pa >>= \case
            LUndef -> goTwo k1 l (i + 1)
            LTrue  -> return Satisfied_
            LFalse -> goOne k1 (i + 1)

    goTwo k1 k2 i
        | i >= len
        = return (Unresolved_ k1 k2)

        | otherwise
        , let !l = indexPrimArray ls i
        = lookupPartialAssignment l pa >>= \case
            LUndef -> goTwo k1 k2 (i + 1)
            LTrue  -> return Satisfied_
            LFalse -> goTwo k1 k2 (i + 1)

-------------------------------------------------------------------------------
-- Solver
-------------------------------------------------------------------------------

-- | Solver
data Solver s = Solver
    { ok        :: !(STRef s Bool)
    , nextLit   :: !(STRef s Int)
    , solution  :: !(PartialAssignment s)
    , variables :: !(STRef s VarSet)
    , clauses   :: !(STRef s Clauses)
    }

-- | Create new solver
newSolver :: ST s (Solver s)
newSolver = do
    ok        <- newSTRef True
    nextLit   <- newSTRef 0
    solution  <- newPartialAssignment
    variables <- newSTRef emptyVarSet
    clauses   <- newSTRef []
    return Solver {..}

-- | Create fresh literal
newLit :: Solver s -> ST s Lit
newLit Solver {..} = do
    extendPartialAssignment solution
    l' <- readSTRef nextLit
    writeSTRef nextLit (l' + 2)
    let l = MkLit l'
    modifySTRef variables (insertVarSet (litToVar l))
    return l

addClause :: Solver s -> [Lit] -> ST s Bool
addClause solver@Solver {..} clause = do
    ok' <- readSTRef ok
    if ok'
    then do
        s <- satisfied solution clause
        case s of
            Satisfied    -> return True
            Conflicting  -> conflict solver
            Unresolved c -> do
                clauses' <- readSTRef clauses
                writeSTRef clauses (c : clauses')
                return True
            Unit l -> do
                insertPartialAssignment l solution
                modifySTRef variables (deleteVarSet (litToVar l))
                return True
    else return False

conflict :: Solver s -> ST s Bool
conflict Solver {..} = do
    writeSTRef ok False
    clearPartialAssignment solution
    writeSTRef clauses []
    return False

data Trail
    = End
    | Deduced !Lit !Trail
    | Decided !Lit !Trail
  deriving Show

solve :: Solver s -> ST s Bool
solve solver@Solver {..} = whenOk_ (simplify solver) $ do
    clauses' <- readSTRef clauses
    vars     <- readSTRef variables
    -- traceM $ "solve " ++ show (length clauses')

#ifdef TWO_WATCHED_LITERALS
    litCount <- readSTRef nextLit
    clauseDB <- newClauseDB litCount
    forM_ clauses' $ \c -> satisfied2_ solution c >>= \case
        Unresolved_ l1 l2 -> insertClauseDB l1 l2 c clauseDB
        _                 -> error "PANIC! not simplified DB"
#else
    let clauseDB = clauses'
#endif

    solveLoop clauseDB End emptyLitSet solution vars >>= \case
        False -> conflict solver
        True  -> return True

solveLoop :: ClauseDB s -> Trail -> LitSet -> PartialAssignment s -> VarSet -> ST s Bool
solveLoop !clauseDb !trail !units !pa !vars
    | Just (l, units') <- minViewLitSet units
    = lookupPartialAssignment l pa >>= \case
        LUndef -> do
            insertPartialAssignment l pa
            let !vars' = deleteVarSet (litToVar l) vars
            unitPropagate l clauseDb (Deduced l trail) units pa vars'
        LTrue  -> solveLoop clauseDb trail units' pa vars
        LFalse -> backtrack clauseDb trail pa vars

    | Just (v, vars') <- minViewVarSet vars
    = do
        -- traceM $ "decide" ++ show v
        let l = varToLit v
        insertPartialAssignment l pa
        unitPropagate l clauseDb (Decided l trail) emptyLitSet pa vars'

    | otherwise
    = return True

unitPropagate :: forall s. Lit -> ClauseDB s -> Trail -> LitSet -> PartialAssignment s -> VarSet -> ST s Bool

#ifdef TWO_WATCHED_LITERALS

unitPropagate !l !clauseDb !trail !units !pa !vars = do
    -- dbSize <- _sizeofClauseDB clauseDb
    -- traceM $ "unitPropagate " ++ show (l, dbSize)
    watches <- lookupClauseDB (neg l) clauseDb
    size <- sizeofVec watches
    go units watches 0 0 size
  where
    go :: LitSet -> Vec s Watch -> Int -> Int -> Int -> ST s Bool
    go !us watches i j size
        | i >= size
        = do
            shrinkVec watches j
            solveLoop clauseDb trail us pa vars

        | otherwise
        = readVec watches i >>= \ w@(W l' c) -> satisfied2_ pa c >>= \case
            Conflicting_      -> do
                writeVec watches j w

                let copy i' j' =
                        if i' < size
                        then do
                            w' <- readVec watches i'
                            writeVec watches j' w'
                            copy (i' + 1) (j' + 1)

                        else shrinkVec watches j'

                copy (i + 1) (j + 1)

                backtrack clauseDb trail pa vars
            Satisfied_        -> do
                writeVec watches j w
                go us watches (i + 1) (j + 1) size
            Unit_ u           -> do
                writeVec watches j w
                go (insertLitSet u us) watches (i + 1) (j + 1) size
            Unresolved_ l1 l2
                | l2 /= l', l2 /= l
                -> do
                    insertWatch l2 w clauseDb
                    go us watches (i + 1) j size

                | l1 /= l', l1 /= l
                -> do
                    insertWatch l1 w clauseDb
                    go us watches (i + 1) j size

                | otherwise
                -> error ("watch" ++ show (l, l1, l2, l'))

#else

unitPropagate !_ !clauseDb !trail !units !pa !vars = go units clauseDb
  where
    go :: LitSet -> [Clause2] -> ST s Bool
    go us []     = solveLoop clauseDb trail us pa vars
    go us (c:cs) = satisfied2_ pa c >>= \case
        Conflicting_    -> backtrack clauseDb trail pa vars
        Satisfied_      -> go us cs
        Unit_ u         -> go (insertLitSet u us) cs
        Unresolved_ _ _ -> go us cs
#endif

backtrack :: ClauseDB s -> Trail -> PartialAssignment s -> VarSet -> ST s Bool
backtrack !_clauseDb End               !_pa !_vars = return False
backtrack   clauseDb (Deduced l trail)   pa   vars = do
    deletePartialAssignment l pa
    backtrack clauseDb trail pa (insertVarSet (litToVar l) vars)
backtrack  clauseDb (Decided l trail) pa vars = do
    deletePartialAssignment l pa
    insertPartialAssignment (neg l) pa
    unitPropagate (neg l) clauseDb (Deduced (neg l) trail) emptyLitSet pa vars

-------------------------------------------------------------------------------
-- simplify
-------------------------------------------------------------------------------

-- | Simplify solver
simplify :: Solver s -> ST s Bool
simplify solver@Solver {..} = whenOk ok $ do
    clauses0 <- readSTRef clauses
    vars0    <- readSTRef variables

    simplifyLoop [] solution clauses0 vars0 >>= \case
        Nothing -> conflict solver
        Just (clauses1, vars1) -> do
            writeSTRef clauses clauses1
            writeSTRef variables vars1
            -- traceM $ "simplify " ++ show (length pa0, length pa1, IntSet.size vars0, IntSet.size vars1)
            return True

simplifyLoop :: [Clause2] -> PartialAssignment s -> [Clause2] -> VarSet -> ST s (Maybe ([Clause2], VarSet))
simplifyLoop !acc !_  []     !vars = return (Just (acc, vars))
simplifyLoop !acc !pa (c:cs) !vars = satisfied2_ pa c >>= \case
    Conflicting_    -> return Nothing
    Satisfied_      -> simplifyLoop acc     pa cs vars
    Unresolved_ _ _ -> simplifyLoop (c:acc) pa cs vars
    Unit_ l         -> do
        insertPartialAssignment l pa
        simplifyLoop [] pa (acc ++ cs) (deleteVarSet (litToVar l) vars)

-------------------------------------------------------------------------------
-- queries
-------------------------------------------------------------------------------

-- | Lookup model value
modelValue :: Solver s -> Lit -> ST s Bool
modelValue Solver {..} l = do
    lookupPartialAssignment l solution <&> \case
        LUndef -> False
        LTrue  -> True
        LFalse -> False

-------------------------------------------------------------------------------
-- utilities
-------------------------------------------------------------------------------

whenOk :: STRef s Bool -> ST s Bool -> ST s Bool
whenOk ok = whenOk_ (readSTRef ok)

whenOk_ :: ST s Bool -> ST s Bool -> ST s Bool
whenOk_ ok action = do
    ok' <- ok
    if ok' then action else return False
