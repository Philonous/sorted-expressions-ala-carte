{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , RankNTypes
           , TypeInType
           , TypeOperators #-}

module Utils.Indexed.Sum ((:+:)(..), (:<:)(..), inject) where

import Data.Kind

import Utils.Indexed.Fixpoint
import Utils.Indexed.Functor
import Utils.Indexed.Show

data (f :+: g) a i = InL (f a i) | InR (g a i)

infixr 8 :+:

instance (IFunctor f, IFunctor g) => IFunctor (f :+: g) where
    imap f (InL fa) = InL $ imap f fa
    imap f (InR ga) = InR $ imap f ga

class (IFunctor f, IFunctor g) => f :<: g where
    inj :: f a i -> g a i
instance IFunctor f => f :<: f where
    inj = id
instance (IFunctor f, IFunctor g) => f :<: (f :+: g) where
    inj = InL
instance {-# OVERLAPPABLE #-} (IFunctor f, IFunctor g, IFunctor h, f :<: g) => f :<: (h :+: g) where
    inj = InR . inj

inject :: g :<: f => forall i. g (IFix f) i -> IFix f i
inject = IFix . inj

instance (IShow f, IShow g) => IShow (f :+: g) where
    ishow (InL fa) = ishow fa
    ishow (InR ga) = ishow ga
