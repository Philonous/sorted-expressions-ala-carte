{-# LANGUAGE DeriveDataTypeable
           , GADTs
           , RankNTypes
           , StandaloneDeriving
           , ScopedTypeVariables
           , TypeOperators
           , TypeInType #-}

module Expression.Sort.Dynamic ( DynamicSort(..)
                               , DynamicallySorted(..)
                               -- , sortEq
                               , toStaticSort
                               , toStaticallySorted
                               , TypeableSort(..)
                               , toTypeableSort
                               , withTypeableSort
                               ) where

import Data.Kind
import Data.Singletons
import Data.Typeable
import Data.Singletons.Decide

import Expression.Sort
import Utils.Indexed.Fixpoint

deriving instance Typeable Sort

data DynamicSort where
    DynamicSort :: forall (s :: Sort). Sing s -> DynamicSort

data DynamicallySorted (f :: (Sort -> *) -> (Sort -> *)) where
  DynamicallySorted
    :: forall (s :: Sort) f. Typeable s
    => Sing s
    -> IFix f s
    -> DynamicallySorted f

data TypeableSort a where
  STS :: Typeable a => Sing a -> TypeableSort a

-- | Recover Typeable instance for s. To get the instance in scope, match on the
-- resulting STS
toTypeableSort :: forall (s :: Sort). Sing s -> TypeableSort s
toTypeableSort x =
  case x of
    -- The trick here is that by pattern matching on x we learn what s is.
    -- We thereby also implicitly learn which Typeable instance s has. GHC
    -- then invisibly packs it up for us.
    SBooleanSort -> STS x
    SIntegralSort -> STS x
    (SArraySort i e) ->
      case (toTypeableSort i, toTypeableSort e) of
           (STS i', STS e') -> STS (SArraySort i' e')

-- | Alternative way to bring Typeable instance into scope using Rank2-types
-- instead of existentials
withTypeableSort :: forall (s :: Sort) a. Sing s -> (Typeable s => a) -> a
withTypeableSort s f =
  case s of
    SBooleanSort -> f
    SIntegralSort -> f
    SArraySort i e ->
      -- Bringing typeable instances of i and e into scope is enough to
      -- reconstruct Typeable instance of ArraySort i e
      withTypeableSort i $ withTypeableSort e $ f


-- Similar to the old sortEq, but instead of Nothing we get a proof by contradiction
--
-- There's no reason not to use (%~) directly, I just put this here for
-- demonstration
sortEq :: forall (a :: Sort) (b :: Sort) . Sing a -> Sing b -> Decision (a :~: b)
sortEq = (%~)

-- -- Same trick as before, by pattern matching on the singletons we learn their
-- -- types, which allows the type checker to check that a ~ b and create Refl as
-- -- evidence.
-- -- Note that something like
-- -- sortEq SBooleanSort SIntegralSort = Just Refl
-- -- luckily does not typecheck (because BooleanSort /~ IntegralSort)
-- sortEq SBooleanSort SBooleanSort = Just Refl
-- sortEq SIntegralSort SIntegralSort = Just Refl
-- sortEq (SArraySort si1 se1) (SArraySort si2 se2) =
--   -- First we need to bring i1 ~ i2 and e1 ~ e2 into scope.
--   case (sortEq si1 si2, sortEq se1 se2) of
--     (Just Refl, Just Refl) -> Just Refl
--     _ -> Nothing
-- sortEq _ _ = Nothing


-- Here's how to write toStaticSort using singletons and SingI instead of
-- Typeable
--
-- SingI allows us to reflect types into singletons. SingI constraints act like
-- implicit parameters. You can make it into an explicit singleton using
-- the overloaded values sing, as in (sing :: Sing s)
toStaticSort' :: forall (s :: Sort). SingI s => DynamicSort -> Maybe (Sing s)
toStaticSort' dx =
  case dx of
    DynamicSort x ->
      -- Needs ScopedTypeVariables so we can reference the return type s
      case x %~ (sing :: Sing s) of
        Proved Refl -> Just x -- Brings into scope a proof that x :: s, so we
                              -- can return x
        Disproved _ -> Nothing

toStaticSort :: forall (s :: Sort). Typeable s => DynamicSort -> Maybe (Sing s)
toStaticSort (DynamicSort s) =
  case toTypeableSort s of
    STS s' -> gcast s'

toStaticallySorted :: forall f (s :: Sort). Typeable s => DynamicallySorted f -> Maybe (IFix f s)
toStaticallySorted (DynamicallySorted _ a) = gcast a
