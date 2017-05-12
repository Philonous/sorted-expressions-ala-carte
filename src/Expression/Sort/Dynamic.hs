{-# LANGUAGE DeriveDataTypeable
           , GADTs
           , RankNTypes
           , StandaloneDeriving
           , TypeInType #-}

module Expression.Sort.Dynamic ( DynamicSort(..)
                               , DynamicallySorted(..)
                               , toStaticSort
                               , toStaticallySorted ) where

import Data.Kind
import Data.Typeable

import Expression.Sort
import Utils.Indexed.Fixpoint

deriving instance Typeable Sort

data DynamicSort where
    DynamicSort :: forall (s :: Sort). Typeable s => SSort s -> DynamicSort

data DynamicallySorted (f :: (Sort -> *) -> (Sort -> *)) where
    DynamicallySorted :: Typeable s => SSort s -> IFix f s -> DynamicallySorted f

toStaticSort :: forall (s :: Sort). Typeable s => DynamicSort -> Maybe (SSort s)
toStaticSort (DynamicSort s) = gcast s

toStaticallySorted :: forall f (s :: Sort). Typeable s => DynamicallySorted f -> Maybe (IFix f s)
toStaticallySorted (DynamicallySorted _ a) = gcast a
