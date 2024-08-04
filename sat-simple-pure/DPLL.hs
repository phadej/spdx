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
    -- * Statistics
    num_vars,
    num_clauses,
    num_learnts,
    num_conflicts,
    num_restarts,
) where

-- #define ENABLE_ASSERTS
-- #define ENABLE_TRACE

#define TWO_WATCHED_LITERALS

import Control.Monad        (forM_, when)
import Control.Monad.ST     (ST)
import Data.Bits            (testBit, unsafeShiftR)
import Data.Functor         ((<&>))
import Data.List            (nub)
import Data.STRef           (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Word            (Word8)

import Data.Primitive.ByteArray
       (MutableByteArray (..), getSizeofMutableByteArray, newByteArray, readByteArray, resizeMutableByteArray,
       shrinkMutableByteArray, writeByteArray, fillByteArray, copyMutableByteArray)
import Data.Primitive.PrimArray
       (indexPrimArray, primArrayFromList, sizeofPrimArray)
import Data.Primitive.PrimArray (MutablePrimArray, readPrimArray, writePrimArray, newPrimArray)
import Data.Primitive.PrimVar   (PrimVar, newPrimVar, writePrimVar, readPrimVar)

import LCG
import DPLL.Clause2
import DPLL.LBool
import DPLL.LitSet
import DPLL.LitTable
import DPLL.LitVar
import DPLL.Stats
import DPLL.VarSet

#ifdef TWO_WATCHED_LITERALS
import Vec
#endif

#ifdef ENABLE_TRACE
import Debug.Trace
#define TRACING(x) x
#else
#define TRACING(x)
#endif

#ifdef ENABLE_ASSERTS
import Assert
import GHC.Stack
#define ASSERTING(x) x
#else
#define ASSERTING(x)
#endif

import Debug.Trace

-------------------------------------------------------------------------------
-- Partial Assignment
-------------------------------------------------------------------------------

newtype PartialAssignment s = PA (MutableByteArray s)

newPartialAssignment :: Int -> ST s (PartialAssignment s)
newPartialAssignment size = do
    arr <- newByteArray (min 4096 size)
    shrinkMutableByteArray arr size
    fillByteArray arr 0 size 0xff
    return (PA arr)

copyPartialAssignment :: PartialAssignment s -> PartialAssignment s -> ST s ()
copyPartialAssignment (PA src) (PA tgt) = do
    n <- getSizeofMutableByteArray src
    m <- getSizeofMutableByteArray tgt
    let size = min n m
    copyMutableByteArray tgt 0 src 0 size

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
    ASSERTING(readByteArray arr (lit_to_var l) >>= \x -> assertST "insert" (x == (0xff :: Word8)))
    writeByteArray arr (lit_to_var l) (if testBit l 0 then 0x1 else 0x0 :: Word8)

deletePartialAssignment :: Lit -> PartialAssignment s -> ST s ()
deletePartialAssignment (MkLit l) (PA arr) = do
    writeByteArray arr (lit_to_var l) (0xff :: Word8)

#ifdef ENABLE_TRACE
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
#endif

#ifdef ENABLE_ASSERTS
assertLiteralInPartialAssignment :: Lit -> PartialAssignment s -> ST s ()
assertLiteralInPartialAssignment l pa =
    lookupPartialAssignment l pa >>= \case
        LTrue -> return ()
        x     -> assertST ("lit in partial: " ++ show x) False
#endif

-------------------------------------------------------------------------------
-- ClauseDB
-------------------------------------------------------------------------------

#ifdef TWO_WATCHED_LITERALS

newtype ClauseDB s = CDB (LitTable s (Vec s Watch))

data Watch = W !Lit !Clause2

newClauseDB :: Int -> ST s (ClauseDB s)
newClauseDB !size' = do
    let size = max size' 4096
    arr <- newLitTable size undefined

    forM_ [0 .. size - 1] $ \i -> do
        vec <- newVec 16
        writeLitTable arr (MkLit i) vec

    return (CDB arr)

extendClauseDB :: ClauseDB s -> Int -> ST s (ClauseDB s)
extendClauseDB cdb@(CDB old) newSize' = do
    -- TODO: this code is terrible.
    oldSize <- sizeofLitTable old
    let newSize = max newSize' 4096
    if newSize <= oldSize
    then return cdb
    else do
        new <- newLitTable newSize undefined

        forM_ [0 .. newSize - 1] $ \i -> do
            if i < oldSize
            then do
                x <- readLitTable old (MkLit i)
                writeLitTable new (MkLit i) x
            else do
                vec <- newVec 16
                writeLitTable new (MkLit i) vec

        return (CDB new)

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

-- TODO: this is used in learning code.
insertClauseDB :: Lit -> Lit -> Clause2 -> ClauseDB s -> ST s ()
insertClauseDB _ _ _ _ = return ()

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
    { ok         :: !(STRef s Bool)
    , nextLit    :: !(STRef s Int) -- TODO: change to PrimVar
    , solution   :: !(STRef s (PartialAssignment s))
    , variables  :: !(STRef s (VarSet s))
    , clauses    :: !(STRef s (ClauseDB s))
    , lcg        :: !(LCG s)
    , statistics :: !(Stats s)
    }

-- | Create new solver
newSolver :: ST s (Solver s)
newSolver = do
    ok         <- newSTRef True
    nextLit    <- newSTRef 0
    solution   <- newPartialAssignment 0 >>= newSTRef
    variables  <- newVarSet >>= newSTRef
    statistics <- newStats
#ifdef TWO_WATCHED_LITERALS
    clauses    <- newClauseDB 0 >>= newSTRef
#else
    clauses    <- newSTRef []
#endif
    lcg        <- newLCG 44
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

#ifdef TWO_WATCHED_LITERALS
    clauseDB  <- readSTRef clauses
    clauseDB' <- extendClauseDB clauseDB (l' + 2)
    writeSTRef clauses clauseDB'
#endif

    insertVarSet (litToVar l) vars'

    return l

boost :: Word -> Word
boost !n =
    let !m = n + 64
    in if m < n then maxBound else m
{-# INLINE [1] boost #-}

decay :: Word -> Word
decay !n = n - unsafeShiftR n 6
{-# INLINE [1] decay #-}

boostScore :: Solver s -> Lit -> ST s ()
boostScore Solver {..} l = do
    vars <- readSTRef variables
    weightVarSet (litToVar l) boost vars

addClause :: Solver s -> [Lit] -> ST s Bool
addClause solver@Solver {..} clause = whenOk ok $ do
        pa <- readSTRef solution
        s <- satisfied pa clause
        case s of
            Satisfied    -> return True
            Conflicting  -> unsat solver
            Unresolved !c -> do
                incrStatsClauses statistics

                clauseDB <- readSTRef clauses
#ifdef TWO_WATCHED_LITERALS
                let MkClause2 l1 l2 _ = c
                insertClauseDB l1 l2 c clauseDB
#else
                writeSTRef clauses (c : clauseDB)
#endif

                return True

            Unit l -> do
                TRACING(traceM $ "addClause unit: " ++ show l)

                litCount <- readSTRef nextLit
                clauseDB <- readSTRef clauses
                vars <- readSTRef variables
                units <- newLitSet litCount
                insertLitSet l units

                res <- initialLoop clauseDB units vars pa
                if res
                then return True
                else unsat solver

unsat :: Solver s -> ST s Bool
unsat Solver {..} = do
    writeSTRef ok False
    -- TODO: cleanup clauses
    -- writeSTRef clauses []
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

#ifdef ENABLE_TRACE
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
#endif

#ifdef ENABLE_ASSERTS
assertEmptyTrail :: HasCallStack => Trail s -> ST s ()
assertEmptyTrail (Trail size _) = do
    n <- readPrimVar size
    ASSERTING (assertST "n == 0" $ n == 0)
    return ()
#endif

-------------------------------------------------------------------------------
-- Solving
-------------------------------------------------------------------------------

data Self s = Self
    { clauseDB :: !(ClauseDB s)
      -- ^ clause database

    , pa       :: !(PartialAssignment s)
      -- ^ current partial assignment

    , prev     :: !(PartialAssignment s)
      -- ^ previous partial assignment

    , units    :: !(LitSet s)
      -- ^ unit literals to be processed

    , vars     :: !(VarSet s)
      -- ^ undecided variables

    , reasons  :: !(LitTable s Clause2)
      -- ^ reason clauses

    , sandbox  :: !(LitSet s)
      -- ^ sandbox used to construct conflict clause

    , trail :: {-# UNPACK #-} !(Trail s)
      -- ^ solution trail

    , stats :: !(Stats s)
    }

solve :: Solver s -> ST s Bool
solve solver@Solver {..} = whenOk_ (simplify solver) $ do
    clauseDB <- readSTRef clauses
    vars     <- readSTRef variables

    litCount <- readSTRef nextLit
    units    <- newLitSet litCount
    reasons  <- newLitTable litCount nullClause
    sandbox  <- newLitSet litCount
    pa       <- readSTRef solution
    prev     <- newPartialAssignment litCount
    trail    <- newTrail litCount
    let stats = statistics

    TRACING(sizeofVarSet vars >>= \n -> traceM $ "vars to solve " ++ show n)
    TRACING(tracePartialAssignment pa)

    let self = Self {..}

    solveLoop self >>= \case
        False -> unsat solver
        True  -> return True

restart :: Self s -> Lit -> ST s Bool
restart self@Self {..} l = do
    TRACING(traceM ("restart " ++ show l))
    incrStatsRestarts stats
    copyPartialAssignment pa prev

    unwind trail

    clearLitSet units
    insertLitSet l units
    res <- initialLoop clauseDB units vars pa
    TRACING(traceM ("restart propagation result: " ++ show res))

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
                go (i + 1) n

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
            ASSERTING(c <- readLitTable reasons l)
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
        l' <- lookupPartialAssignment l prev <&> \case
            LTrue  -> neg l
            LFalse -> l
            LUndef -> l

        setLiteral2 self l'
        writeLitTable reasons l' nullClause
        pushTrail l' trail
        unitPropagate self l'
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
        = readVec watches i >>= \ w@(W l' c) -> do
            let onConflict :: ST s Bool
                {-# INLINE onConflict #-}
                onConflict = do
                    writeVec watches j w
                    copy watches (i + 1) (j + 1) size
                    backtrack self c

                onSatisfied :: ST s Bool
                {-# INLINE onSatisfied #-}
                onSatisfied = do
                    writeVec watches j w
                    go watches (i + 1) (j + 1) size

                onUnit :: Lit -> ST s Bool
                {-# INLINE onUnit #-}
                onUnit u = do
                    writeVec watches j w
                    foundUnitClause self u c

                    isNeg <- memberLitSet units (neg u)
                    -- for now this is pessimisation,
                    -- as we don't learn from these conflicts.
                    -- (enabling it increases the conflicts)
                    if isNeg && False
                    then do
                        -- c1 <- readLitTable reasons u
                        -- c2 <- readLitTable reasons (neg u)
                        -- traceM $ "there is neg: " ++ show (l, u, c, c1, c2)

                        copy watches (i + 1) (j + 1) size

                        insertPartialAssignment (neg u) pa
                        deleteVarSet (litToVar u) vars
                        pushTrail (neg u) trail

                        backtrack self c
                    else do
                        go watches (i + 1) (j + 1) size

            if isBinaryClause2 c
            then lookupPartialAssignment l' pa >>= \case
                LUndef -> onUnit l'
                LTrue  -> onSatisfied
                LFalse -> onConflict
            else do
                let kontUnitPropagate = \case
                        Conflicting_      -> onConflict
                        Satisfied_        -> onSatisfied
                        Unit_ u           -> onUnit u
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

                satisfied2_ pa c kontUnitPropagate

    copy :: Vec s Watch -> Int -> Int -> Int -> ST s ()
    copy watches i j size = do
        if i < size
        then do
            w' <- readVec watches i
            writeVec watches j w'
            copy watches (i + 1) (j + 1) size

        else shrinkVec watches j
#else

unitPropagate self@Self {..} _l = go clauseDB
  where
    go :: [Clause2] -> ST s Bool
    go []     = solveLoop self
    go (c:cs) = satisfied2_ pa c $ \case
        Conflicting_    -> backtrack self c
        Satisfied_      -> go cs
        Unit_ u         -> do
            foundUnitClause self u c
            go cs
        Unresolved_ _ _ -> go cs
#endif

#ifdef ENABLE_TRACE
traceCause :: LitSet s -> ST s ()
traceCause sandbox = do
    xs <- elemsLitSet sandbox
    traceM $ "current cause " ++ show xs
#endif

backtrack :: forall s. Self s -> Clause2 -> ST s Bool
backtrack self@Self {..} !cause = do
    incrStatsConflicts stats
    scaleVarSet vars decay

    let Trail size _ = trail
    TRACING(traceTrail reasons trail)
    clearLitSet sandbox
    forLitInClause2_ cause insertSandbox
    TRACING(traceCause sandbox)
    go False size
   where
    insertSandbox :: Lit -> ST s ()
    insertSandbox !l = insertLitSet l sandbox
    {-# INLINE [1] insertSandbox #-}

    go :: Bool -> PrimVar s Int -> ST s Bool
    go notFirst size = do
        n <- readPrimVar size
        if n <= 0
        then return False -- end of the trail
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
                    forLitInClause2_ c insertSandbox
                    deleteLitSet l       sandbox
                    deleteLitSet (neg l) sandbox

                    -- TODO: assertConflict
                    -- satisfied2_ pa conflictCause $ \case
                    --     Conflicting_ -> return ()
                    --     ot           -> assertST (show ot) False

                    TRACING(traceCause sandbox)
                    conflictSize <- sizeofLitSet sandbox
                    case conflictSize of
                        1 -> unsingletonLitSet sandbox >>= \l' -> restart self l'
                        _ -> do
                            -- TODO: add when 2WL has specific support for binary clauses
                            -- when (notFirst && conflictSize == 2) $ do
                            --     incrStatsLearnt stats
                            --     conflictCause <- litSetToClause sandbox
                            --     case conflictCause of
                            --         MkClause2 l1 l2 _ -> insertClauseDB l1 l2 conflictCause clauseDB
                            go True size

                else do
                    -- TODO: assertConflict
                    TRACING(traceM ("deduced skip " ++ show l))
                    ASSERTING(conflictCause <- litSetToClause sandbox)
                    ASSERTING(assertClauseConflicting pa conflictCause)
                    go True size

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
                    let conflictSize = sizeofClause2 conflictCause
                    TRACING(traceM $ "conflict size " ++ show conflictSize)
                    ASSERTING(assertClauseConflicting pa conflictCause)

                    -- learning: when the conflict clause is binary clause
                    -- we don't need to worry where to insert the clause in 2WL setup
                    when (notFirst && conflictSize == 2) $ do
                        incrStatsLearnt stats
                        case conflictCause of
                            MkClause2 l1 l2 _ -> insertClauseDB l1 l2 conflictCause clauseDB

                    -- TODO: toggleLiteral self pa
                    deletePartialAssignment l pa
                    ASSERTING(assertClauseUnit pa conflictCause)

                    insertPartialAssignment (neg l) pa
                    ASSERTING(assertClauseSatisfied pa conflictCause)

                    clearLitSet units

                    -- boost literals
                    let boost' !cl = weightVarSet (litToVar cl) boost vars
                        {-# INLINE [1] boost' #-}
                    forLitInClause2_ conflictCause boost'

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

                    go True size

-------------------------------------------------------------------------------
-- initial loop
-------------------------------------------------------------------------------

initialLoop :: forall s. ClauseDB s -> LitSet s -> VarSet s -> PartialAssignment s -> ST s Bool
initialLoop clauseDB units vars pa = minViewLitSet units noUnit yesUnit
  where
    noUnit :: ST s Bool
    noUnit = return True

    yesUnit :: Lit -> ST s Bool
    yesUnit !l = lookupPartialAssignment l pa >>= \case
        LTrue  -> initialLoop clauseDB units vars pa
        LFalse -> return False
        LUndef -> do
            insertPartialAssignment l pa
            deleteVarSet (litToVar l) vars
            initialUnitPropagate clauseDB units vars pa l

initialUnitPropagate :: forall s. ClauseDB s -> LitSet s -> VarSet s -> PartialAssignment s -> Lit -> ST s Bool
initialUnitPropagate clauseDB units vars pa l = do
    let _unused = l
    TRACING(traceM ("initialUnitPropagate " ++ show l))
#ifdef TWO_WATCHED_LITERALS
    clearClauseDB clauseDB l -- clear literal clauses: they are always true.
    watches <- lookupClauseDB (neg l) clauseDB
    size <- sizeofVec watches
    TRACING(traceM ("initialUnitPropagate watches: " ++ show size))
    go watches 0 0 size
  where
    go :: Vec s Watch -> Int -> Int -> Int -> ST s Bool
    go !watches !i !j !size
        | i >= size
        = do
            shrinkVec watches j
            initialLoop clauseDB units vars pa

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
    go [] = initialLoop clauseDB units vars pa
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
simplify _ = return True
-- TODO: go through clauses:
-- * filter out satisfied clauses
-- * filter out the solved literals from remaining clauses

-------------------------------------------------------------------------------
-- statistics
-------------------------------------------------------------------------------

num_vars :: Solver s -> ST s Int
num_vars Solver {..} = do
    n <- readSTRef nextLit
    return (unsafeShiftR n 1)

num_clauses :: Solver s -> ST s Int
num_clauses Solver {..} = readStatsClauses statistics

num_learnts :: Solver s -> ST s Int
num_learnts Solver {..} = readStatsLearnt statistics

num_conflicts :: Solver s -> ST s Int
num_conflicts Solver {..} = readStatsConflicts statistics

num_restarts :: Solver s -> ST s Int
num_restarts Solver {..} = readStatsRestarts statistics

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
