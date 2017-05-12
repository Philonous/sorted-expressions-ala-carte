{-# LANGUAGE DataKinds
           , FlexibleContexts
           , GADTs
           , TypeInType
           , TypeOperators #-}

module Expression.Equality ( EqualityF(..)
                           , (.=.) ) where

import Data.Functor.Const
import Data.Kind

import Expression.Sort
import Utils.Indexed.Fixpoint
import Utils.Indexed.Functor
import Utils.Indexed.Show
import Utils.Indexed.Sum

data EqualityF a (s :: Sort) where
    Equals :: a s -> a s -> EqualityF a BooleanSort

instance IFunctor EqualityF where
    imap f (a `Equals` b) = f a `Equals` f b

instance IShow EqualityF where
    ishow (a `Equals` b) = Const $ "(= " ++ getConst a ++ " " ++ getConst b ++ ")"

(.=.) :: EqualityF :<: f => IFix f s -> IFix f s -> IFix f BooleanSort
a .=. b = inject (a `Equals` b)

infix 7 .=.
