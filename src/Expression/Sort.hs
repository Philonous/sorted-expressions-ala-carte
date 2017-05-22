{-# LANGUAGE GADTs
           , DataKinds
           , KindSignatures
           , RankNTypes
           , TemplateHaskell
           , TypeFamilies
           , TypeSynonymInstances
           , UndecidableInstances
#-}

module Expression.Sort (Sort(..), Sing(..)) where

import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.TH

data Sort = BooleanSort | IntegralSort | ArraySort { index :: Sort, element :: Sort }
             deriving (Eq)


genSingletons [''Sort]

-- Creates the equivalent to:
--
-- data SSort :: Sort -> * where
--     SBooleanSort  :: SSort BooleanSort
--     SIntegralSort :: SSort IntegralSort
--     SArraySort    :: { sIndex :: SSort i, sElement :: SSort e } -> SSort (ArraySort i e)

singDecideInstance ''Sort

-- Creates an instance for
--
-- class SDecide k where
--  (%~) :: forall (a :: k) (b :: k).
--          Sing a -> Sing b -> Decision (a :~: b)
--
-- Which is an overloaded sortEq


show' :: Sort -> String
show' BooleanSort     = "bool"
show' IntegralSort    = "int"
show' (ArraySort i e) = "(array " ++ show' i ++ " " ++ show' e ++ ")"

instance Show Sort where
    show s@BooleanSort   = show' s
    show s@IntegralSort  = show' s
    show (ArraySort i e) = "array " ++ show' i ++ " " ++ show' e

ssortToSort :: forall s. SSort s -> Sort
ssortToSort SBooleanSort     = BooleanSort
ssortToSort SIntegralSort    = IntegralSort
ssortToSort (SArraySort i e) = ArraySort (ssortToSort i) (ssortToSort e)

instance Show (SSort s) where
    show = show . ssortToSort
