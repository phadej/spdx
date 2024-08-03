module DPLL.Stats (
    Stats,
    newStats,
    readStatsConflicts, incrStatsConflicts,
    readStatsRestarts, incrStatsRestarts,
    readStatsLearnt, incrStatsLearnt,
    readStatsClauses, incrStatsClauses,
) where

import Control.Monad.ST (ST)
import Data.Primitive.PrimArray

-------------------------------------------------------------------------------
-- Stats
-------------------------------------------------------------------------------

data Stats s = MkStats (MutablePrimArray s Int)

newStats :: ST s (Stats s)
newStats = MkStats <$> do
    arr <- newPrimArray 4
    setPrimArray arr 0 4 0
    return arr

readStatsConflicts :: Stats s -> ST s Int
incrStatsConflicts :: Stats s -> ST s ()
(readStatsConflicts, incrStatsConflicts) = makeStat 0

readStatsRestarts :: Stats s -> ST s Int
incrStatsRestarts :: Stats s -> ST s ()
(readStatsRestarts, incrStatsRestarts) = makeStat 1

readStatsLearnt :: Stats s -> ST s Int
incrStatsLearnt :: Stats s -> ST s ()
(readStatsLearnt, incrStatsLearnt) = makeStat 2

readStatsClauses :: Stats s -> ST s Int
incrStatsClauses :: Stats s -> ST s ()
(readStatsClauses, incrStatsClauses) = makeStat 3

makeStat :: Int -> (Stats s1 -> ST s1 Int, Stats s2 -> ST s2 ())
makeStat i = (read_, incr_)
  where
    read_ (MkStats arr) = readPrimArray arr i
    incr_ (MkStats arr) = do
        x <- readPrimArray arr i
        writePrimArray arr i (x + 1)
