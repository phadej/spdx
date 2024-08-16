{-# LANGUAGE CPP              #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards  #-}
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
    num_learnt_literals,
    num_conflicts,
    num_restarts,
) where

-- #define ENABLE_ASSERTS
-- #define ENABLE_TRACE

#define TWO_WATCHED_LITERALS

import Data.Functor ((<&>))
import Data.List    (nub)
import Data.STRef   (STRef, newSTRef, readSTRef, writeSTRef)

import Data.Primitive.PrimArray (primArrayFromList, readPrimArray)
import Data.Primitive.PrimVar   (PrimVar, readPrimVar, writePrimVar, newPrimVar, modifyPrimVar)

import DPLL.Base
import DPLL.Boost
import DPLL.Clause2
import DPLL.LBool
import DPLL.Level
import DPLL.LitSet
import DPLL.LitTable
import DPLL.LitVar
import DPLL.PartialAssignment
import DPLL.Satisfied
import DPLL.Stats
import DPLL.Trail
import DPLL.VarSet
import DPLL.Utils
import LCG
import SparseSet

#ifdef TWO_WATCHED_LITERALS
import Vec
#endif

#ifdef ENABLE_TRACE
#define TRACING(x) x
#else
#define TRACING(x)
#endif

#ifdef ENABLE_ASSERTS
#define ASSERTING(x) x
#define ASSERTING_BIND(x,y) x <- y
#else
#define ASSERTING(x)
#define ASSERTING_BIND(x,y)
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
    ASSERTING(assertST "l1" (litInClause l1 clause))
    ASSERTING(assertST "l2" (litInClause l2 clause))
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

    go2 !l1 !l2 acc []     = return (Unresolved (MkClause2 False l1 l2 (primArrayFromList acc)))
    go2 !l1 !l2 acc (l:ls) = lookupPartialAssignment l pa >>= \case
        LUndef -> go2 l1 l2 (l : acc) ls
        LTrue  -> return Satisfied
        LFalse -> go2 l1 l2 acc ls

-------------------------------------------------------------------------------
-- Clause2
-------------------------------------------------------------------------------

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
                let MkClause2 _ l1 l2 _ = c
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
-- Solving
-------------------------------------------------------------------------------

data Self s = Self
    { clauseDB :: !(ClauseDB s)
      -- ^ clause database

    , level    :: !(PrimVar s Level)
      -- ^ current decision level

    , levels   :: !(Levels s)
      -- ^ decision levels of literals

    , pa       :: !(PartialAssignment s)
      -- ^ current partial assignment

    , prev     :: !(PartialAssignment s)
      -- ^ previous partial assignment

    , zero     :: !(PartialAssignment s)
      -- ^ ground partial assignment

    , qhead    :: !(PrimVar s Int)
      -- ^ unit propsagation head

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
    level    <- newPrimVar (Level 0)
    levels   <- newLevels litCount
    qhead    <- newPrimVar 0
    reasons  <- newLitTable litCount nullClause
    sandbox  <- newLitSet litCount
    pa       <- readSTRef solution
    prev     <- newPartialAssignment litCount
    zero     <- newPartialAssignment litCount
    copyPartialAssignment pa zero
    trail    <- newTrail litCount
    let stats = statistics

    TRACING(sizeofVarSet vars >>= \n -> traceM $ "vars to solve " ++ show n)
    TRACING(tracePartialAssignment pa)

    let self = Self {..}

    solveLoop self >>= \case
        False -> unsat solver
        True  -> return True

enqueue :: Self s -> Lit -> Level -> Clause2 -> ST s ()
enqueue Self {..} l d c = do
    TRACING(traceM $ "enqueue " ++ show (l, d, c))
    ASSERTING(assertLiteralUndef l pa)
    ASSERTING(assertST "enqueue reason" (isNullClause c || litInClause l c))

    insertPartialAssignment l pa
    deleteVarSet (litToVar l) vars
    writeLitTable reasons l c
    setLevel levels l d
    pushTrail l trail

unsetLiteral :: Self s -> Lit -> ST s ()
unsetLiteral Self {..} l = do
    -- TODO: assert l in pa
    -- TODO: assert (litToVar l) not in vars
    deletePartialAssignment l pa
    insertVarSet (litToVar l) vars

boostSandbox :: Self s -> ST s ()
boostSandbox Self {..} = do
    n <- readPrimVar size
    go 0 n
  where
    LS SS {..} = sandbox

    go !i !n = when (i < n) $ do
        l <- readPrimArray dense i
        weightVarSet (litToVar (MkLit l)) boost vars
        go (i + 1) n

solveLoop :: forall s. Self s -> ST s Bool
solveLoop self@Self {..} = do
    let Trail sizeVar _ = trail
    n <- readPrimVar sizeVar
    i <- readPrimVar qhead

    TRACING(traceM $ "!!! SOLVE: " ++ show (i, n))
    TRACING(tracePartialAssignment zero)
    TRACING(tracePartialAssignment pa)
    TRACING(traceTrail reasons levels trail)

    if i < n
    then do
        -- traceM $ "i < n: " ++ show (i, n)
        -- traceTrail reasons levels trail
        l <- indexTrail trail i

        writePrimVar qhead (i + 1)
        unitPropagate self l
    else
        noUnit
  where
    noUnit :: ST s Bool
    noUnit = minViewVarSet vars noVar yesVar

    noVar :: ST s Bool
    noVar = do
        TRACING(traceM ">>> SOLVE: SAT")
        return True

    yesVar :: Var -> ST s Bool
    yesVar !v = do
        TRACING(traceM $ ">>> SOLVE: deciding variable " ++ show v)
        -- increase decision level
        lvl <- readPrimVar level
        let !lvl' = succ lvl
        writePrimVar level lvl'

        l' <- lookupPartialAssignment l prev <&> \case
            LTrue  -> neg l
            LFalse -> l
            LUndef -> l

        enqueue self l' lvl' nullClause

        -- solve loop
        modifyPrimVar qhead $ \i -> i + 1
        unitPropagate self l'
      where
        !l = varToLit v

unitPropagate :: forall s. Self s -> Lit -> ST s Bool

#ifdef TWO_WATCHED_LITERALS

unitPropagate self@Self {..} !l  = do
    TRACING(traceM ("!!! PROPAGATE " ++ show l))

    ASSERTING(let Trail sizeVar trailLits = trail)
    ASSERTING(n <- readPrimVar sizeVar)
    ASSERTING(assertST "trail not empty" $ n > 0)
    ASSERTING(q <- readPrimVar qhead)
    ASSERTING(assertST "qhead" $ q <= n)
    TRACING(traceM $ show q)
    ASSERTING(ll <- indexTrail trail (q - 1))
    ASSERTING(assertST "end of the trail is the var we propagate" $ l == ll)

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

                    lvl <- readPrimVar level
                    enqueue self u lvl c
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

traceCause :: LitSet s -> ST s ()
traceCause sandbox = do
    xs <- elemsLitSet sandbox
    traceM $ "current cause " ++ show xs

withTwoLargestLevels :: LitSet s -> Int -> Levels s -> (Level -> Level -> ST s r) -> ST s r
withTwoLargestLevels !sandbox !conflictSize !levels kont =
    go zeroLevel zeroLevel 0
  where
    go d1 d2 i
        | i >= conflictSize = kont d1 d2
        | otherwise = do
            d <- indexLitSet sandbox i >>= getLevel levels
            if d > d2 then go d2 d (i + 1)
            else if d > d1 then go d d2 (i + 1)
            else go d1 d2 (i + 1)

analyse :: forall s. Self s -> Clause2 -> ST s Level
analyse Self {..} !cause = do
    TRACING(traceM $ "!!! ANALYSE: " ++ show cause)
    let Trail size lits = trail
    n <- readPrimVar size
    clearLitSet sandbox
    forLitInClause2_ cause insertSandbox
    conflictSize <- sizeofLitSet sandbox

    withTwoLargestLevels sandbox conflictSize levels $ \d1 d2 -> do
        lvl <- readPrimVar level
        if (d1 < lvl) then return d1 else if (d2 < lvl) then return d2 else go lits n (n - 1)
  where
    insertSandbox !l = insertLitSet l sandbox
    {-# INLINE insertSandbox #-}
    go !lits !n !i
        | i >= 0 = do
            l <- readPrimArray lits i
            c <- readLitTable reasons l


            if isNullClause c
            then do
                TRACING(traceM $ ">>> decided " ++ show (l, c))
                b <- memberLitSet sandbox (neg l)
                if b
                then do
                    tracePartialAssignment zero
                    traceCause sandbox
                    traceTrail reasons levels trail
                    error $ "decision variable" ++ show (b, n, i, l, c, cause)
                else
                    go lits n (i - 1)
            else do
                b <- memberLitSet sandbox (neg l)
                if b
                then do
                    TRACING(traceM $ ">>> deduced undo" ++ show (l, c))
                    TRACING(traceCause sandbox)

                    ASSERTING(assertST "literal in reason clause" $ litInClause l c)

                    -- resolution of current conflict with the deduction cause
                    forLitInClause2_ c insertSandbox
                    deleteLitSet l       sandbox
                    deleteLitSet (neg l) sandbox

                    TRACING(traceCause sandbox)
                    conflictSize <- sizeofLitSet sandbox

                    withTwoLargestLevels sandbox conflictSize levels $ \d1 d2 -> do
                        lvl <- readPrimVar level
                        -- traceM $ "UIP? " ++ show (lvl, d1, d2)
                        if (d1 < lvl) then return d1 else if (d2 < lvl) then return d2 else go lits n (i - 1)
                else do
                    TRACING(traceM $ ">>> decuced skip" ++ show (l, c))
                    go lits n (i - 1)

        | otherwise
        = assertST "reached end of trail" False >> error "-"

backjump0 :: forall s. Self s -> ST s Bool
backjump0 self@Self {..} = do
    TRACING(traceM $ "!!! BACKJUMP0")
    TRACING(traceCause sandbox)
    incrStatsRestarts stats

    -- unwind trail
    unwindTrail
    writePrimVar qhead 0
    writePrimVar level zeroLevel
    clearLevels levels

    conflictSize <- sizeofLitSet sandbox
    ASSERTING(assertST "conflict size is not zero" $ conflictSize > 0)

    l <- case conflictSize of
        1 -> unsingletonLitSet sandbox
        _ -> do
            conflictCause <- litSetToClause sandbox
            satisfied2_ zero conflictCause $ \case
                Unit_ l' -> return l'
                x -> error $ "TODO " ++ show (conflictSize, x)

    TRACING(traceM $ ">>> literal " ++ show l)

    -- initial propagation
    let units = sandbox
    clearLitSet units
    insertLitSet l units
    res <- initialLoop clauseDB units vars pa
    TRACING(traceM (">>> propagation result: " ++ show res))

    copyPartialAssignment pa zero

    ASSERTING(assertEmptyTrail trail)
    if res
    then solveLoop self -- TODO: decision
    else return False
  where
    unwindTrail = do
        n <- readPrimVar size
        writePrimVar size 0
        go 0 n
      where
        Trail size ls = trail

        go i n
            | i >= n = return ()
            | otherwise = do
                l' <- readPrimArray ls i
                unsetLiteral self l'
                go (i + 1) n

backjump :: forall s. Self s -> Level -> ST s Bool
backjump self@Self {..} conflictLevel = do
    TRACING(traceM $ "!!! BACKJUMP: " ++ show conflictLevel)
    TRACING(traceCause sandbox)
    TRACING(traceTrail reasons levels trail)

    ASSERTING(assertST "backump level > 0" $ conflictLevel > zeroLevel)

    writePrimVar level conflictLevel

    let Trail sizeVar _ = trail
    i <- readPrimVar sizeVar
    go sizeVar (i - 1)
  where
    go sizeVar i = do
        l <- indexTrail trail i
        dlvl <- getLevel levels l

        if dlvl == conflictLevel
        then do
            TRACING(traceM $ ">>> JUMP: " ++ show (i, l, dlvl, conflictLevel))
            conflictSize <- sizeofLitSet sandbox
            ASSERTING(assertST "conflict size >= 2" $ conflictSize >= 2)

            conflictClause <- litSetToClause sandbox
            TRACING(traceM $ "JUMPED: " ++ show (i, l, dlvl, conflictLevel, conflictClause))

            satisfied2_ pa conflictClause $ \case
                Unit_ u -> do
                    writePrimVar sizeVar (i + 1)
                    writePrimVar qhead (i + 2)
                    enqueue self u dlvl conflictClause

                    TRACING(traceM $ ">>> JUMPED: " ++ show (i, l, dlvl, conflictLevel, conflictClause, u))
                    TRACING(tracePartialAssignment pa)
                    TRACING(traceTrail reasons levels trail)

                    unitPropagate self u

                x -> error $ "TODO _" ++ show (conflictSize, x)
        else do
            TRACING(traceM $ ">>> UNDO: " ++ show (i, l, dlvl))
            unsetLiteral self l
            go sizeVar (i - 1)

backtrack :: forall s. Self s -> Clause2 -> ST s Bool
backtrack self@Self {..} !cause = do
    TRACING(traceM $ "!!! CONFLICT " ++ show cause)
    TRACING(tracePartialAssignment pa)
    TRACING(traceTrail reasons levels trail)

    incrStatsConflicts stats
    scaleVarSet vars decay

    TRACING(lvl <- readPrimVar level)
    clvl <- analyse self cause

    TRACING(traceM $ ">>> analysed " ++ show (lvl, clvl, cause))
    TRACING(traceCause sandbox)

    -- learn binary clauses
    conflictSize <- sizeofLitSet sandbox
    when (conflictSize == 2) $ do
        conflictClause <- litSetToClause sandbox
        incrStatsLearnt stats
        incrStatsLearntLiterals stats conflictSize

        case conflictClause of
            MkClause2 _     l1 l2 _ -> insertClauseDB l1 l2 conflictClause clauseDB

    -- boost literals in conflict clause
    boostSandbox self

    if clvl == Level 0
    then backjump0 self
    else backjump self clvl

-------------------------------------------------------------------------------
-- initial loop
-------------------------------------------------------------------------------

initialLoop :: forall s. ClauseDB s -> LitSet s -> VarSet s -> PartialAssignment s -> ST s Bool
initialLoop clauseDB units vars pa = do
    TRACING(traceM $ "!!! INITIAL PROPAGATE")
    TRACING(tracePartialAssignment pa)

    minViewLitSet units noUnit yesUnit
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

num_learnt_literals :: Solver s -> ST s Int
num_learnt_literals Solver {..} = readStatsLearntLiterals statistics

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
