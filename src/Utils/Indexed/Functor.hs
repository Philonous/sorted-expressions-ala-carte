{-# LANGUAGE RankNTypes
           , TypeInType
           , UndecidableInstances #-}

module Utils.Indexed.Functor (IFunctor(..), icata) where

import Data.Functor.Const
import Data.Kind

import Utils.Indexed.Fixpoint
import Utils.Indexed.Show

class IFunctor (f :: (i -> *) -> (i -> *)) where
    imap :: (forall i. a i -> b i) -> (forall i. f a i -> f b i)

icata :: IFunctor f => (forall i. f a i -> a i) -> (forall i. IFix f i -> a i)
icata f = f . imap (icata f) . unIFix

instance (IFunctor f, IShow f) => Show (IFix f i) where
    show = getConst . icata ishow
