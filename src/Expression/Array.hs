{-# LANGUAGE FlexibleContexts
           , GADTs
           , TypeInType
           , TypeOperators #-}

module Expression.Array ( ArrayF(..)
                        , select
                        , store ) where

import Data.Functor.Const
import Data.Kind

import Expression.Sort
import Utils.Indexed.Fixpoint
import Utils.Indexed.Functor
import Utils.Indexed.Show
import Utils.Indexed.Sum

data ArrayF a (s :: Sort) where
    Select :: a (ArraySort i e) -> a i        -> ArrayF a e
    Store  :: a (ArraySort i e) -> a i -> a e -> ArrayF a (ArraySort i e)

instance IFunctor ArrayF where
    imap f (Select a i)   = Select (f a) (f i)
    imap f (Store  a i e) = Store  (f a) (f i) (f e)

instance IShow ArrayF where
    ishow (Select a i)   = Const $ "(select " ++ getConst a ++ " " ++ getConst i ++ ")"
    ishow (Store  a i v) = Const $ "(store " ++ getConst a ++ " " ++ getConst i ++ " " ++ getConst v ++ ")"

select :: ArrayF :<: f => IFix f (ArraySort i e) -> IFix f i -> IFix f e
select a i = inject (Select a i)

store :: ArrayF :<: f => IFix f (ArraySort i e) -> IFix f i -> IFix f e -> IFix f (ArraySort i e)
store a i v = inject (Store a i v)
