{-# LANGUAGE GADTs
           , DataKinds
           , KindSignatures
           , RankNTypes #-}

module Expression.Sort (Sort(..), SSort(..)) where

import Data.Typeable

data Sort = BooleanSort | IntegralSort | ArraySort { index :: Sort, element :: Sort }

data SSort :: Sort -> * where
    SBooleanSort  :: SSort BooleanSort
    SIntegralSort :: SSort IntegralSort
    SArraySort    :: (Typeable i, Typeable e) => { sIndex :: SSort i, sElement :: SSort e } -> SSort (ArraySort i e)

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