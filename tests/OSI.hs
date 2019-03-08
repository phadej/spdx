module OSI where

import Distribution.SPDX
import Distribution.SPDX.Extra

import qualified Data.Set as Set

-------------------------------------------------------------------------------
-- Using direct approach
-------------------------------------------------------------------------------

isOsiApprovedExprDirect :: LicenseExpression -> Bool
isOsiApprovedExprDirect = go where
    go (ELicense _ (Just _)) = False
    go (ELicense sl Nothing) = simple sl
    go (EAnd e1 e2)          = go e1 && go e2
    go (EOr e1 e2)           = go e1 || go e2

simple :: SimpleLicenseExpression -> Bool
simple (ELicenseId i)     = licenseIsOsiApproved i
simple (ELicenseIdPlus _) = False -- we don't know ranges
simple (ELicenseRef _)    = False

osiLicenseIds :: [LicenseId]
osiLicenseIds = filter licenseIsOsiApproved [minBound .. maxBound]

-------------------------------------------------------------------------------
-- Using satisfies
-------------------------------------------------------------------------------

osiLicenseExpr :: LicenseExpression
osiLicenseExpr = foldr1 EAnd $ map (\l -> ELicense (ELicenseId l) Nothing) osiLicenseIds

isOsiApprovedExprSatisfies :: LicenseExpression -> Bool
isOsiApprovedExprSatisfies e = License e `satisfies` License osiLicenseExpr

-------------------------------------------------------------------------------
-- Alternative satisfies
-------------------------------------------------------------------------------

satisfies2 :: LicenseExpression -> LicenseExpression -> Bool
satisfies2 a b = search (toLS b) (toLS a)

isOsiApprovedExprSatisfies2 :: LicenseExpression -> Bool
isOsiApprovedExprSatisfies2 e = e `satisfies2` osiLicenseExpr

toLS :: LicenseExpression -> LS Lic
toLS = go where
    go (ELicense l e) = LV (Lic l e)
    go (EAnd a b)     = LConj (go a) (go b)
    go (EOr a b)      = LDisj (go a) (go b)

data Lic = Lic !SimpleLicenseExpression !(Maybe LicenseExceptionId)
  deriving (Eq, Ord)

data LS a
    = LV a
    | LConj (LS a) (LS a)
    | LDisj (LS a) (LS a)

data Ctx a = Ctx
    { ctxAtoms      :: Set.Set a       -- atoms encountered
    , ctxDisj       :: [(LS a, LS a)]  -- disjunctions
    , ctxHypothesis :: [LS a]          -- working set
    }

singletonCtx :: LS a -> Ctx a
singletonCtx l = Ctx Set.empty [] [l]

search :: Ord a => LS a -> LS a -> Bool
search from0 to0 = singletonCtx from0 |- to0 where
    (|-) :: Ord a => Ctx a -> LS a -> Bool

    -- short circuit
    Ctx atoms _ _ |- LV a
        | Set.member a atoms = True

    Ctx atoms disj (LV a : hs) |- r =
        Ctx (Set.insert a atoms) disj hs |- r

    Ctx atoms disj (LConj a b : hs) |- r =
        Ctx atoms disj (a : b : hs) |- r

    Ctx atoms disj (LDisj a b : hs) |- r =
        Ctx atoms ((a,b) : disj) hs |- r

    -- after all obvious things are done we branch
    ctx |- LDisj a b =
        ctx |- a || ctx |- b

    Ctx atoms ((a,b) : disj) [] |- r =
        Ctx atoms disj [a] |- r &&
        Ctx atoms disj [b] |- r

    Ctx atoms [] [] |- r = atoms ||- r
        
    (||-) :: Ord a => Set.Set a -> LS a -> Bool
    atoms ||- LV a      = Set.member a atoms
    atoms ||- LDisj a b = atoms ||- a || atoms ||- b
    atoms ||- LConj a b = atoms ||- a && atoms ||- b

