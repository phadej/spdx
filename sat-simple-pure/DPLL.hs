{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE NoFieldSelectors           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
-- {-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
module DPLL (
    Solver,
    newSolver,
    Lit (..),
    newLit,
    boostScore,
    neg,
    addClause,
    solve,
    simplify,
    modelValue,
) where

#define TWO_WATCHED_LITERALS
-- #define INTSET_VARS

import Control.Monad.ST     (ST)
import Data.Bits            (complementBit, testBit, unsafeShiftL, unsafeShiftR)
import Data.Coerce          (coerce)
import Data.Functor         ((<&>))
import Data.List            (nub)
import Data.Primitive.Types (Prim)
import Data.STRef           (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Word            (Word8)

import Data.Primitive.ByteArray
       (MutableByteArray (..), MutableByteArray#, getSizeofMutableByteArray, newByteArray, readByteArray,
       resizeMutableByteArray, shrinkMutableByteArray, writeByteArray)
import Data.Primitive.PrimArray (PrimArray, emptyPrimArray, indexPrimArray, primArrayFromList, sizeofPrimArray)

import LCG
import Lifted
import SparseSet
import UnliftedSTRef

#ifdef TWO_WATCHED_LITERALS
import Control.Monad            (forM_)
import Data.Primitive.Array     (MutableArray, newArray, readArray, sizeofMutableArray, writeArray)
import Data.Primitive.PrimArray (traversePrimArray_)
import Vec
#endif

#ifdef INTSET_VARS
import qualified Data.IntSet as IS
import           Data.STRef  (modifySTRef)
#else
import SparseMaxHeap
#endif


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

#ifdef INTSET_VARS
newtype VarSet s = VS (STRef s IS.IntSet)

newVarSet :: ST s (VarSet s)
newVarSet = VS <$> newSTRef IS.empty

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

newVarSet :: ST s (VarSet s)
newVarSet = VS <$> newSparseHeap 0

extendVarSet :: Int -> VarSet s -> ST s (VarSet s)
extendVarSet capacity (VS xs) = VS <$> extendSparseHeap capacity xs

weightVarSet :: Var -> (Int -> Int) -> VarSet s -> ST s ()
weightVarSet (MkVar x) f (VS xs) = modifyWeightSparseHeap xs x f

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

-------------------------------------------------------------------------------
-- LitSet
-------------------------------------------------------------------------------

newtype LitSet s = LS (SparseSet s)

newLitSet :: Int -> ST s (LitSet s)
newLitSet n = LS <$> newSparseSet n

insertLitSet :: Lit -> LitSet s -> ST s ()
insertLitSet (MkLit l) (LS ls) = insertSparseSet ls l

{-# INLINE minViewLitSet #-}
minViewLitSet :: LitSet s -> ST s r -> (Lit -> ST s r) -> ST s r
minViewLitSet (LS xs) no yes = popSparseSet_ xs no (coerce yes)

clearLitSet :: LitSet s -> ST s ()
clearLitSet (LS xs) = clearSparseSet xs

-------------------------------------------------------------------------------
-- Clauses
-------------------------------------------------------------------------------

type Clauses = [Clause2]

-------------------------------------------------------------------------------
-- LitTable
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- ClauseDB
-------------------------------------------------------------------------------

#ifdef TWO_WATCHED_LITERALS

newtype ClauseDB s = CDB (MutableArray s (Vec s Watch))

data Watch = W !Lit !Clause2

newClauseDB :: Int -> ST s (ClauseDB s)
newClauseDB !size = do
    arr <- newArray size undefined

    forM_ [0 .. size - 1] $ \i -> do
        vec <- newVec 16
        writeArray arr i vec

    return (CDB arr)

insertClauseDB :: Lit -> Lit -> Clause2 -> ClauseDB s -> ST s ()
insertClauseDB !l1 !l2 !clause !cdb = do
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
satisfied !pa = go0 . nub where
    go0 []     = return Conflicting
    go0 (l:ls) = lookupPartialAssignment l pa >>= \case
        LUndef -> go1 l ls
        LTrue  -> return Satisfied
        LFalse -> go0 ls

    go1 !l1 []     = return (Unit l1)
    go1 !l1 (l:ls) = lookupPartialAssignment l pa >>= \case
        LUndef -> go2 l1 l [] ls
        LTrue  -> return Satisfied
        LFalse -> go1 l1 ls

    go2 !l1 !l2 acc []     = return (Unresolved (MkClause2 l1 l2 (primArrayFromList acc)))
    go2 !l1 !l2 acc (l:ls) = lookupPartialAssignment l pa >>= \case
        LUndef -> go2 l1 l2 (l : acc) ls
        LTrue  -> return Satisfied
        LFalse -> go2 l1 l2 acc ls

-------------------------------------------------------------------------------
-- Clause2
-------------------------------------------------------------------------------

data Clause2 = MkClause2 {-# UNPACK #-} !Lit {-# UNPACK #-} !Lit {-# UNPACK #-} !(PrimArray Lit)
  deriving Show

_nullClause :: Clause2
_nullClause = MkClause2 (MkLit 0) (MkLit 0) emptyPrimArray

data Satisfied_
    = Satisfied_
    | Conflicting_
    | Unit_ !Lit
    | Unresolved_ !Lit !Lit
  deriving Show

{-# INLINE satisfied2_ #-}
satisfied2_ :: PartialAssignment s -> Clause2 -> (Satisfied_ -> ST s r) -> ST s r
satisfied2_ !pa !(MkClause2 l1 l2 ls) kont = go0
  where
    !len = sizeofPrimArray ls

    -- initial state
    go0 = lookupPartialAssignment l1 pa >>= \case
        LUndef -> go1
        LTrue  -> kont Satisfied_
        LFalse -> go2

    -- l1 -> Undef
    go1 = lookupPartialAssignment l2 pa >>= \case
        LUndef -> goTwo l1 l2 0
        LTrue  -> kont Satisfied_
        LFalse -> goOne l1 0

    -- l1 -> False
    go2 = lookupPartialAssignment l2 pa >>= \case
        LUndef -> goOne l2 0
        LTrue  -> kont Satisfied_
        LFalse -> goNone 0

    goNone !i
        | i >= len
        = kont Conflicting_

        | otherwise
        , let !l = indexPrimArray ls i
        = lookupPartialAssignment l pa >>= \case
            LUndef -> goOne l (i + 1)
            LTrue  -> kont Satisfied_
            LFalse -> goNone (i + 1)

    goOne !k1 !i
        | i >= len
        = kont $! Unit_ k1

        | otherwise
        , let !l = indexPrimArray ls i
        = lookupPartialAssignment l pa >>= \case
            LUndef -> goTwo k1 l (i + 1)
            LTrue  -> kont Satisfied_
            LFalse -> goOne k1 (i + 1)

    goTwo !k1 !k2 !i
        | i >= len
        = kont $! Unresolved_ k1 k2

        | otherwise
        , let !l = indexPrimArray ls i
        = lookupPartialAssignment l pa >>= \case
            LUndef -> goTwo k1 k2 (i + 1)
            LTrue  -> kont Satisfied_
            LFalse -> goTwo k1 k2 (i + 1)

-------------------------------------------------------------------------------
-- Solver
-------------------------------------------------------------------------------

-- | Solver
data Solver s = Solver
    { ok        :: !(STRef s Bool)
    , nextLit   :: !(STRef s Int)
    , solution  :: !(PartialAssignment s)
    , variables :: !(STRef s (VarSet s))
    , clauses   :: !(STRef s Clauses)
    , lcg       :: !(LCG s)
    }

-- | Create new solver
newSolver :: ST s (Solver s)
newSolver = do
    ok        <- newSTRef True
    nextLit   <- newSTRef 0
    solution  <- newPartialAssignment
    variables <- newVarSet >>= newSTRef
    clauses   <- newSTRef []
    lcg       <- newLCG 44
    return Solver {..}

-- | Create fresh literal
newLit :: Solver s -> ST s Lit
newLit Solver {..} = do
    extendPartialAssignment solution
    l' <- readSTRef nextLit
    writeSTRef nextLit (l' + 2)
    let l = MkLit l'

    -- add unsolved variable.
    vars <- readSTRef variables
    vars' <- extendVarSet (unsafeShiftR l' 1 + 1) vars
    writeSTRef variables vars'

    insertVarSet (litToVar l) vars'

    return l

boost :: Int -> Int
boost n
    | n <= 0    = 1
    | otherwise = n + 1

_decay :: Int -> Int
_decay n = unsafeShiftR n 1

boostScore :: Solver s -> Lit -> ST s ()
boostScore Solver {..} l = do
    vars <- readSTRef variables
    weightVarSet (litToVar l) (boost . boost . boost) vars

addClause :: Solver s -> [Lit] -> ST s Bool
addClause solver@Solver {..} clause = do
    ok' <- readSTRef ok
    if ok'
    then do
        s <- satisfied solution clause
        case s of
            Satisfied    -> return True
            Conflicting  -> unsat solver
            Unresolved c -> do
                clauses' <- readSTRef clauses
                writeSTRef clauses (c : clauses')
                return True
            Unit l -> do
                insertPartialAssignment l solution
                readSTRef variables >>= deleteVarSet (litToVar l)
                return True
    else return False

unsat :: Solver s -> ST s Bool
unsat Solver {..} = do
    writeSTRef ok False
    clearPartialAssignment solution
    writeSTRef clauses []
    readSTRef variables >>= clearVarSet
    return False

-------------------------------------------------------------------------------
-- Solving
-------------------------------------------------------------------------------

data Self s = Self
    { clauseDB :: !(ClauseDB s)
    , pa       :: !(PartialAssignment s)
    , units    :: !(LitSet s)
    , vars     :: !(VarSet s)
    }

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

    litCount <- readSTRef nextLit
    units <- newLitSet litCount

#ifdef TWO_WATCHED_LITERALS
    clauseDB <- newClauseDB litCount
    forM_ clauses' $ \c@(MkClause2 a b d) ->
        let kontSolve = \case
                Unresolved_ l1 l2 -> do
                    if sizeofPrimArray d == 0
                    then do
                        weightVarSet (litToVar a) (boost . boost . boost) vars
                        weightVarSet (litToVar b) (boost . boost . boost) vars
                    else if sizeofPrimArray d <= 2 then do
                        weightVarSet (litToVar a) (boost . boost) vars
                        weightVarSet (litToVar b) (boost . boost) vars
                        traversePrimArray_ (\l -> weightVarSet (litToVar l) (boost . boost) vars) d
                    else do
                        weightVarSet (litToVar a) boost vars
                        weightVarSet (litToVar b) boost vars
                        traversePrimArray_ (\l -> weightVarSet (litToVar l) boost vars) d

                    insertClauseDB l1 l2 c clauseDB
                _                 -> error "PANIC! not simplified DB"
            {-# INLINE [1] kontSolve #-}
        in satisfied2_ solution c kontSolve
#else
    let clauseDB = clauses'
#endif

    let pa = solution
    let self = Self {..}

    solveLoop self End >>= \case
        False -> unsat solver
        True  -> return True

solveLoop :: forall s. Self s -> Trail -> ST s Bool
solveLoop self@Self {..} !trail = minViewLitSet units noUnit yesUnit
  where
    yesUnit :: Lit -> ST s Bool
    yesUnit !l = lookupPartialAssignment l pa >>= \case
            LUndef -> do
                insertPartialAssignment l pa
                deleteVarSet (litToVar l) vars
                unitPropagate self l (Deduced l trail)
            LTrue  -> solveLoop self trail
            LFalse -> backtrack self trail

    noUnit :: ST s Bool
    noUnit = minViewVarSet vars noVar yesVar

    noVar :: ST s Bool
    noVar = return True

    yesVar :: Var -> ST s Bool
    yesVar !v = do
        -- traceM $ "decide" ++ show v
        let !l = varToLit v

        insertPartialAssignment l pa
        unitPropagate self l (Decided l trail)

unitPropagate :: forall s. Self s -> Lit -> Trail -> ST s Bool

#ifdef TWO_WATCHED_LITERALS

unitPropagate self@Self {..} !l !trail  = do
    -- dbSize <- _sizeofClauseDB clauseDb
    -- traceM $ "unitPropagate " ++ show (l, dbSize)
    watches <- lookupClauseDB (neg l) clauseDB
    size <- sizeofVec watches
    go watches 0 0 size
  where
    go :: Vec s Watch -> Int -> Int -> Int -> ST s Bool
    go !watches !i !j !size
        | i >= size
        = do
            shrinkVec watches j
            solveLoop self trail

        | otherwise
        = readVec watches i >>= \ w@(W l' c) ->
            let kontUnitPropagate = \case
                    Conflicting_      -> do
                        writeVec watches j w

                        let copy !i' !j' =
                                if i' < size
                                then do
                                    w' <- readVec watches i'
                                    writeVec watches j' w'
                                    copy (i' + 1) (j' + 1)

                                else shrinkVec watches j'

                        copy (i + 1) (j + 1)

                        backtrack self trail
                    Satisfied_        -> do
                        writeVec watches j w
                        go watches (i + 1) (j + 1) size
                    Unit_ u           -> do
                        writeVec watches j w
                        insertLitSet u units
                        go watches (i + 1) (j + 1) size
                    Unresolved_ l1 l2
                        | l2 /= l', l2 /= l
                        -> do
                            insertWatch l2 w clauseDB
                            go watches (i + 1) j size

                        | l1 /= l', l1 /= l
                        -> do
                            insertWatch l1 w clauseDB
                            go watches (i + 1) j size

                        | otherwise
                        -> error ("watch" ++ show (l, l1, l2, l'))
                {-# INLINE [1] kontUnitPropagate #-}
            in satisfied2_ pa c kontUnitPropagate
#else

unitPropagate self@Self {..} _l trail = go clauseDB
  where
    go :: [Clause2] -> ST s Bool
    go []     = solveLoop self trail
    go (c:cs) = satisfied2_ pa c $ \case
        Conflicting_    -> backtrack self trail
        Satisfied_      -> go cs
        Unit_ u         -> do
            insertLitSet u units
            go cs
        Unresolved_ _ _ -> go cs
#endif

backtrack :: Self s -> Trail -> ST s Bool
backtrack self@Self {..} = go
  where
    go End               = return False
    go (Deduced l trail) = do
        deletePartialAssignment l pa
        insertVarSet (litToVar l) vars
        go trail
    go (Decided l trail) = do
        deletePartialAssignment l pa
        insertPartialAssignment (neg l) pa
        clearLitSet units
        unitPropagate self (neg l) (Deduced (neg l) trail)

-------------------------------------------------------------------------------
-- simplify
-------------------------------------------------------------------------------

-- | Simplify solver
simplify :: Solver s -> ST s Bool
simplify solver@Solver {..} = whenOk ok $ do
    clauses0 <- readSTRef clauses
    vars     <- readSTRef variables

    simplifyLoop [] solution clauses0 vars >>= \case
        Nothing -> unsat solver
        Just clauses1 -> do
            writeSTRef clauses clauses1
            -- traceM $ "simplify " ++ show (length pa0, length pa1, IntSet.size vars0, IntSet.size vars1)
            return True

simplifyLoop :: [Clause2] -> PartialAssignment s -> [Clause2] -> VarSet s -> ST s (Maybe [Clause2])
simplifyLoop !acc !_  []     !_vars = return (Just acc)
simplifyLoop  acc  pa (c:cs)   vars = satisfied2_ pa c kontSimplify
  where
    {-# INLINE [1] kontSimplify #-}
    kontSimplify = \case
        Conflicting_    -> return Nothing
        Satisfied_      -> simplifyLoop acc     pa cs vars
        Unresolved_ _ _ -> simplifyLoop (c:acc) pa cs vars
        Unit_ l         -> do
            insertPartialAssignment l pa
            deleteVarSet (litToVar l) vars
            simplifyLoop [] pa (acc ++ cs) vars

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
