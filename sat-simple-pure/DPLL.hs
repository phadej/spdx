{-# LANGUAGE CPP                        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE NoFieldSelectors           #-}
{-# LANGUAGE RecordWildCards            #-}
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

-- #define ENABLE_ASSERTS
-- #define ENABLE_TRACE

#define TWO_WATCHED_LITERALS

import Control.Monad.ST     (ST)
import Data.Bits            (complementBit, testBit, unsafeShiftL, unsafeShiftR)
import Data.Coerce          (coerce)
import Data.Functor         ((<&>))
import Data.List            (nub)
import Data.Primitive.Types (Prim)
import Data.STRef           (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Word            (Word8)

import Data.Primitive.ByteArray
       (MutableByteArray (..), getSizeofMutableByteArray, newByteArray, readByteArray, resizeMutableByteArray,
       shrinkMutableByteArray, writeByteArray)
import Data.Primitive.PrimArray
       (PrimArray, emptyPrimArray, foldrPrimArray, freezePrimArray, indexPrimArray, primArrayFromList, readPrimArray,
       sizeofPrimArray)
import Data.Primitive.PrimArray (MutablePrimArray, readPrimArray, writePrimArray, newPrimArray)
import Data.Primitive.PrimVar   (PrimVar, newPrimVar, writePrimVar, readPrimVar)

#ifdef ENABLE_ASSERTS
import Assert
#endif

import LCG
import SparseSet
import DPLL.LBool
import DPLL.LitVar
import DPLL.LitSet
import DPLL.VarSet
import DPLL.LitTable
import DPLL.Clause2

import Control.Monad            (forM_)
import Data.Primitive.Array     (MutableArray, newArray, readArray, writeArray)
import Data.Primitive.PrimArray (traversePrimArray_)

#ifdef TWO_WATCHED_LITERALS
import Vec
#endif

import Unsafe.Coerce (unsafeCoerce)

import Debug.Trace
import GHC.Stack

#ifdef ENABLE_TRACE
#define TRACING(x) x
#else
#define TRACING(x)
#endif

#ifdef ENABLE_ASSERTS
#define ASSERTING(x) x
#else
#define ASSERTING(x)
#endif

-------------------------------------------------------------------------------
-- Stats
-------------------------------------------------------------------------------

data Stats s = MkStats (PrimArray Int)

-------------------------------------------------------------------------------
-- Partial Assignment
-------------------------------------------------------------------------------

newtype PartialAssignment s = PA (MutableByteArray s)

newPartialAssignment :: ST s (PartialAssignment s)
newPartialAssignment = do
    arr <- newByteArray 4096
    shrinkMutableByteArray arr 0
    return (PA arr)

extendPartialAssignment :: PartialAssignment s -> ST s (PartialAssignment s)
extendPartialAssignment (PA arr) = do
    size <- getSizeofMutableByteArray arr
    arr' <- resizeMutableByteArray arr (size + 1)
    writeByteArray arr' size (0xff :: Word8)
    return (PA arr')

lookupPartialAssignment :: Lit -> PartialAssignment s -> ST s LBool
lookupPartialAssignment (MkLit l) (PA arr) = do
    readByteArray @Word8 arr (lit_to_var l) >>= \case
        0x0 -> return (if y then LFalse else LTrue)
        0x1 -> return (if y then LTrue else LFalse)
        _   -> return LUndef
  where
    y = testBit l 0
    {-# INLINE y #-}

insertPartialAssignment :: Lit -> PartialAssignment s -> ST s ()
insertPartialAssignment (MkLit l) (PA arr) = do
    writeByteArray arr (lit_to_var l) (if testBit l 0 then 0x1 else 0x0 :: Word8)

deletePartialAssignment :: Lit -> PartialAssignment s -> ST s ()
deletePartialAssignment (MkLit l) (PA arr) = do
    writeByteArray arr (lit_to_var l) (0xff :: Word8)

tracePartialAssignment :: PartialAssignment s -> ST s ()
tracePartialAssignment (PA arr) = do
    n <- getSizeofMutableByteArray arr
    lits <- go n [] 0
    traceM $ "PartialAssignment " ++ show lits
  where
    go n acc i
        | i < n
        , let l = MkLit (var_to_lit i)
        = readByteArray @Word8 arr i >>= \case
          0x0 -> go n (neg l : acc) (i + 1)
          0x1 -> go n (    l : acc) (i + 1)
          _   -> go n          acc  (i + 1)

        | otherwise
        = return (reverse acc)

#ifdef ENABLE_ASSERTS
assertLiteralInPartialAssignment :: Lit -> PartialAssignment s -> ST s ()
assertLiteralInPartialAssignment l pa =
    lookupPartialAssignment l pa >>= \case
        LTrue -> return ()
        x     -> assertST ("lit in partial: " ++ show x) False
#endif

-------------------------------------------------------------------------------
-- Clauses
-------------------------------------------------------------------------------

type Clauses = [Clause2]

-------------------------------------------------------------------------------
-- ClauseDB
-------------------------------------------------------------------------------

#ifdef TWO_WATCHED_LITERALS

newtype ClauseDB s = CDB (LitTable s (Vec s Watch))

data Watch = W !Lit !Clause2

newClauseDB :: Int -> ST s (ClauseDB s)
newClauseDB !size = do
    arr <- newLitTable size undefined

    forM_ [0 .. size - 1] $ \i -> do
        vec <- newVec 16
        writeLitTable arr (MkLit i) vec

    return (CDB arr)

insertClauseDB :: Lit -> Lit -> Clause2 -> ClauseDB s -> ST s ()
insertClauseDB !l1 !l2 !clause !cdb = do
    insertWatch l1 (W l2 clause) cdb
    insertWatch l2 (W l1 clause) cdb

insertWatch :: Lit -> Watch -> ClauseDB s -> ST s ()
insertWatch !l !w (CDB cdb) = do
    ws  <- readLitTable cdb l
    ws' <- insertVec ws w
    writeLitTable cdb l ws'

lookupClauseDB :: Lit -> ClauseDB s -> ST s (Vec s Watch)
lookupClauseDB !l (CDB arr) = do
    readLitTable arr l

clearClauseDB :: ClauseDB s -> Lit -> ST s ()
clearClauseDB (CDB cdb) l = do
    v <- newVec 0
    writeLitTable cdb l v

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

#ifdef ENABLE_ASSERTS
assertClauseConflicting :: PartialAssignment s -> Clause2 -> ST s ()
assertClauseConflicting pa c =
    satisfied2_ pa c $ \case
        Conflicting_ -> return ()
        ot           -> assertST (show ot) False

assertClauseUnit :: PartialAssignment s -> Clause2 -> ST s ()
assertClauseUnit pa c =
    satisfied2_ pa c $ \case
        Unit_ {} -> return ()
        ot       -> assertST (show ot) False

assertClauseSatisfied :: PartialAssignment s -> Clause2 -> ST s ()
assertClauseSatisfied pa c =
    satisfied2_ pa c $ \case
        Satisfied_ {} -> return ()
        ot            -> assertST (show ot) False
#endif

-------------------------------------------------------------------------------
-- Solver
-------------------------------------------------------------------------------

-- | Solver
data Solver s = Solver
    { ok        :: !(STRef s Bool)
    , nextLit   :: !(STRef s Int)
    , solution  :: !(STRef s (PartialAssignment s))
    , variables :: !(STRef s (VarSet s))
    , clauses   :: !(STRef s Clauses)
    , lcg       :: !(LCG s)
    }

-- | Create new solver
newSolver :: ST s (Solver s)
newSolver = do
    ok        <- newSTRef True
    nextLit   <- newSTRef 0
    solution  <- newPartialAssignment >>= newSTRef
    variables <- newVarSet >>= newSTRef
    clauses   <- newSTRef []
    lcg       <- newLCG 44
    return Solver {..}

-- | Create fresh literal
newLit :: Solver s -> ST s Lit
newLit Solver {..} = do
    pa <- readSTRef solution
    pa' <- extendPartialAssignment pa
    writeSTRef solution pa'

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
        pa <- readSTRef solution
        s <- satisfied pa clause
        case s of
            Satisfied    -> return True
            Conflicting  -> unsat solver
            Unresolved c -> do
                clauses' <- readSTRef clauses
                writeSTRef clauses (c : clauses')
                return True
            Unit l -> do
                insertPartialAssignment l pa
                readSTRef variables >>= deleteVarSet (litToVar l)
                return True
    else return False

unsat :: Solver s -> ST s Bool
unsat Solver {..} = do
    writeSTRef ok False
    writeSTRef clauses []
    readSTRef variables >>= clearVarSet
    return False

-------------------------------------------------------------------------------
-- Trail
-------------------------------------------------------------------------------

data Trail s = Trail !(PrimVar s Int) !(MutablePrimArray s Lit)

newTrail :: Int -> ST s (Trail s)
newTrail capacity = do
    size <- newPrimVar 0
    ls <- newPrimArray capacity
    return (Trail size ls)

popTrail :: Trail s -> ST s Lit
popTrail (Trail size ls) = do
    n <- readPrimVar size
    ASSERTING(assertST "non empty trail" (n >= 1))
    writePrimVar size (n - 1)
    readPrimArray ls (n - 1)

pushTrail :: Lit -> Trail s -> ST s ()
pushTrail l (Trail size ls) = do
    n <- readPrimVar size
    writePrimVar size (n + 1)
    writePrimArray ls n l

traceTrail :: forall s. LitTable s Clause2 -> Trail s -> ST s ()
traceTrail reasons (Trail size ls) = do
    n <- readPrimVar size
    out <- go 0 n
    traceM $ unlines $ "=== Trail ===" : out
  where
    go :: Int -> Int -> ST s [String]
    go i n
        | i >= n
        = return ["=== ===== ==="]

        | otherwise
        = do
            l <- readPrimArray ls i
            c <- readLitTable reasons l
            ls <- go (i + 1) n
            if isNullClause c
            then return ((showString "Decided " . showsPrec 11 l) "" : ls)
            else return ((showString "Deduced " . showsPrec 11 l . showChar ' ' . showsPrec 11 c) "" : ls)

assertEmptyTrail :: HasCallStack => Trail s -> ST s ()
assertEmptyTrail (Trail size _) = do
    n <- readPrimVar size
    ASSERTING (assertST "n == 0" $ n == 0)
    return ()

-------------------------------------------------------------------------------
-- Solving
-------------------------------------------------------------------------------

data Self s = Self
    { clauses_ :: ![Clause2]
      -- ^ original clauses

    , clauseDB :: !(ClauseDB s)
      -- ^ clause database actually used for BCP

    , pa       :: !(PartialAssignment s)
      -- ^ current partial assignment

    , units    :: !(LitSet s)
      -- ^ unit literals to be processed

    , vars     :: !(VarSet s)
      -- ^ undecided variables

    , reasons  :: !(LitTable s Clause2)
      -- ^ reason clauses

    , sandbox  :: !(LitSet s)
      -- ^ sandbox used to construct conflict clause

    , trail :: {-# UNPACK #-} !(Trail s)
    }

solve :: Solver s -> ST s Bool
solve solver@Solver {..} = whenOk_ (simplify solver) $ do
    clauses' <- readSTRef clauses
    vars     <- readSTRef variables

    litCount <- readSTRef nextLit
    units    <- newLitSet litCount
    reasons  <- newLitTable litCount nullClause
    sandbox  <- newLitSet litCount
    pa       <- readSTRef solution
    trail    <- newTrail litCount

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
        in satisfied2_ pa c kontSolve
#else
    let clauseDB = clauses'
#endif

    let clauses_ = clauses'
    let self     = Self {..}

    solveLoop self >>= \case
        False -> unsat solver
        True  -> return True

restart :: Self s -> Lit -> ST s Bool
restart self@Self {..} l = do
    TRACING(traceM ("restart " ++ show l))

    unwind trail

    clearLitSet units
    insertLitSet l units
    res <- initialLoop self

    ASSERTING(assertEmptyTrail trail)
    if res
    then solveLoop self
    else return False
  where
    unwind (Trail size ls) = do
        n <- readPrimVar size
        writePrimVar size 0
        go 0 n
      where
        go i n
            | i >= n = return ()
            | otherwise = do
                l' <- readPrimArray ls i
                unsetLiteral self l'

setLiteral1 :: Self s -> Lit -> ST s ()
setLiteral1 Self {..} l = do
    -- TODO: assert l not in pa
    -- TODO: assert (litToVar l) in vars
    insertPartialAssignment l pa
    deleteVarSet (litToVar l) vars

setLiteral2 :: Self s -> Lit -> ST s ()
setLiteral2 Self {..} l = do
    -- TODO: assert l not in pa
    -- TODO: assert (litToVar l) not in vars
    insertPartialAssignment l pa

unsetLiteral :: Self s -> Lit -> ST s ()
unsetLiteral Self {..} l = do
    -- TODO: assert l in pa
    -- TODO: assert (litToVar l) not in vars
    deletePartialAssignment l pa
    insertVarSet (litToVar l) vars

solveLoop :: forall s. Self s -> ST s Bool
solveLoop self@Self {..} = minViewLitSet units noUnit yesUnit
  where
    yesUnit :: Lit -> ST s Bool
    yesUnit !l = lookupPartialAssignment l pa >>= \case
        LUndef -> do
            setLiteral1 self l
            pushTrail l trail
            c <- readLitTable reasons l
            ASSERTING(assertST "reason is not null" (not (isNullClause c)))
            unitPropagate self l

        LFalse -> do
            c <- readLitTable reasons l
            -- traceM $ "backtrack unit " ++ show (l, c)
            -- traceTrail reasons trail
            backtrack self c

        LTrue  -> solveLoop self

    noUnit :: ST s Bool
    noUnit = minViewVarSet vars noVar yesVar

    noVar :: ST s Bool
    noVar = return True

    yesVar :: Var -> ST s Bool
    yesVar !v = do
        TRACING(traceM ("loop: decide var " ++ show l))

        setLiteral2 self l
        writeLitTable reasons l nullClause
        pushTrail l trail
        unitPropagate self l
      where
        !l = varToLit v

foundUnitClause :: Self s -> Lit -> Clause2 -> ST s ()
foundUnitClause Self{..} l c = do
#ifdef ENABLE_ASSERTS
    -- TODO: literalInClause l c
#endif
    insertLitSet l units
    writeLitTable reasons l c

unitPropagate :: forall s. Self s -> Lit -> ST s Bool

#ifdef TWO_WATCHED_LITERALS

unitPropagate self@Self {..} !l  = do
    TRACING(traceM ("unitPropagate " ++ show l))
    watches <- lookupClauseDB (neg l) clauseDB
    size <- sizeofVec watches
    go watches 0 0 size
  where
    go :: Vec s Watch -> Int -> Int -> Int -> ST s Bool
    go !watches !i !j !size
        | i >= size
        = do
            shrinkVec watches j
            solveLoop self

        | otherwise
        = readVec watches i >>= \ w@(W l' c) ->
            let kontUnitPropagate = \case
                    Conflicting_      -> do
                        writeVec watches j w
                        copy watches (i + 1) (j + 1) size
                        backtrack self c

                    Satisfied_        -> do
                        writeVec watches j w
                        go watches (i + 1) (j + 1) size

                    Unit_ u           -> do
                        writeVec watches j w
                        foundUnitClause self u c
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

    copy :: Vec s Watch -> Int -> Int -> Int -> ST s ()
    copy watches i j size = do
        if i < size
        then do
            w' <- readVec watches i
            writeVec watches j w'
            copy watches (i + 1) (j + 1) size

        else shrinkVec watches j
#else

unitPropagate self@Self {..} _l trail = go clauseDB
  where
    go :: [Clause2] -> ST s Bool
    go []     = solveLoop self trail
    go (c:cs) = satisfied2_ pa c $ \case
        Conflicting_    -> backtrack self c trail
        Satisfied_      -> go cs
        Unit_ u         -> do
            foundUnitClause self u c
            go cs
        Unresolved_ _ _ -> go cs
#endif

traceCause :: LitSet s -> ST s ()
traceCause sandbox = do
    xs <- elemsLitSet sandbox
    traceM $ "current cause " ++ show xs

backtrack :: forall s. Self s -> Clause2 -> ST s Bool
backtrack self@Self {..} !cause = do
    let Trail size _ = trail
    TRACING(traceM ("backtrack reason " ++ show cause))
    TRACING(traceTrail reasons trail)
    clearLitSet sandbox
    forLitInClause2_ cause $ \l -> insertLitSet l sandbox
    TRACING(traceCause sandbox)
    go size
  where
    go :: PrimVar s Int -> ST s Bool
    go size = do
        n <- readPrimVar size
        if n <= 0
        then return False
        else do
            l <- popTrail trail
            c <- readLitTable reasons l
            if not $ isNullClause c
            then do
                TRACING(traceM ("backtrack deduce " ++ show l))
                ASSERTING(assertLiteralInPartialAssignment l pa)

                unsetLiteral self l

                b <- memberLitSet sandbox (neg l)
                if b
                then do
                    TRACING(traceM ("deduced undo " ++ show l ++ " " ++ show c))
                    ASSERTING(assertST "literal in reason clause" $ litInClause l c)

                    -- resolution of current conflict with the deduction cause
                    forLitInClause2_ c $ \l' -> insertLitSet l' sandbox
                    deleteLitSet l       sandbox
                    deleteLitSet (neg l) sandbox
        {-
                conflictCause <- litSetToClause sandbox
                satisfied2_ pa conflictCause $ \case
                    Conflicting_ -> return ()
                    ot           -> assertST (show ot) False
        -}

                    TRACING(traceCause sandbox)
                    sizeofLitSet sandbox >>= \case
                        1 -> unsingletonLitSet sandbox >>= \l' -> restart self l'
                        _ -> go size

                else do
                    -- assertConflict
                    TRACING(traceM ("deduced skip " ++ show l))
                    ASSERTING(conflictCause <- litSetToClause sandbox)
                    ASSERTING(assertClauseConflicting pa conflictCause)
                    go size

            else do
                TRACING(traceM ("backtrack decide " ++ show l))
                TRACING(traceCause sandbox)
                TRACING(traceTrail reasons trail)
                ASSERTING(assertLiteralInPartialAssignment l pa)

                b <- memberLitSet sandbox (neg l)
                if b
                then do
                    TRACING(traceM ("decided flip " ++ show l))
                    conflictCause <- litSetToClause sandbox
                    ASSERTING(assertClauseConflicting pa conflictCause)

                    -- TODO: toggleLiteral self pa
                    deletePartialAssignment l pa
                    ASSERTING(assertClauseUnit pa conflictCause)

                    insertPartialAssignment (neg l) pa
                    ASSERTING(assertClauseSatisfied pa conflictCause)

                    clearLitSet units

                    writeLitTable reasons (neg l) conflictCause
                    pushTrail (neg l) trail
                    unitPropagate self (neg l)

                else do
                    TRACING(traceM ("decided skip " ++ show l))
                    -- continue
                    deletePartialAssignment l pa
                    insertVarSet (litToVar l) vars

                    ASSERTING(conflictCause <- litSetToClause sandbox)
                    ASSERTING(assertClauseConflicting pa conflictCause)

                    go size

-------------------------------------------------------------------------------
-- initial loop
-------------------------------------------------------------------------------

initialLoop :: forall s. Self s -> ST s Bool
initialLoop self@Self {..} = minViewLitSet units noUnit yesUnit
  where
    noUnit :: ST s Bool
    noUnit = return True

    yesUnit :: Lit -> ST s Bool
    yesUnit !l = lookupPartialAssignment l pa >>= \case
        LTrue  -> initialLoop self
        LFalse -> return False
        LUndef -> do
            setLiteral1 self l
            initialUnitPropagate self l

initialUnitPropagate :: forall s. Self s -> Lit -> ST s Bool
initialUnitPropagate self@Self {..} l = do
    let _unused = l
    TRACING(traceM ("initialUnitPropagate " ++ show l))
#ifdef TWO_WATCHED_LITERALS
    clearClauseDB clauseDB l
    watches <- lookupClauseDB (neg l) clauseDB
    size <- sizeofVec watches
    go watches 0 0 size
  where
    go :: Vec s Watch -> Int -> Int -> Int -> ST s Bool
    go !watches !i !j !size
        | i >= size
        = do
            shrinkVec watches j
            initialLoop self

        | otherwise
        = readVec watches i >>= \ w@(W l' c) ->
          satisfied2_ pa c (kontInitialUnitPropagate w l')
      where
        {-# INLINE [1] kontInitialUnitPropagate #-}
        kontInitialUnitPropagate w l' = \case
            Conflicting_ -> return False
            Satisfied_ -> do
                go watches (i + 1) j size
            Unit_ u -> do
                insertLitSet u units
                go watches (i + 1) j size
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
#else
    go clauseDB
  where
    go [] = initialLoop self
    go (c:cs) = satisfied2_ pa c (kontInitialUnitPropagate cs)

    {-# INLINE [1] kontInitialUnitPropagate #-}
    kontInitialUnitPropagate :: [Clause2] -> Satisfied_ -> ST s Bool
    kontInitialUnitPropagate cs = \case
        Conflicting_    -> return False
        Unresolved_ _ _ -> go cs
        Satisfied_      -> go cs
        Unit_ u         -> do
            insertLitSet u units
            go cs
#endif

-------------------------------------------------------------------------------
-- simplify
-------------------------------------------------------------------------------

-- | Simplify solver
simplify :: Solver s -> ST s Bool
simplify solver@Solver {..} = whenOk ok $ do
    clauses0 <- readSTRef clauses
    vars     <- readSTRef variables
    pa       <- readSTRef solution

    simplifyLoop [] pa clauses0 vars >>= \case
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
    pa <- readSTRef solution
    lookupPartialAssignment l pa <&> \case
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
