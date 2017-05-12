{-# LANGUAGE DataKinds
           , FlexibleContexts
           , GADTs
           , TypeInType
           , TypeOperators #-}

module Expression.Arithmetic ( ArithmeticF(..)
                             , cnst
                             , add
                             , mul
                             , (.+.)
                             , (.*.)
                             , (.<.) ) where

import Data.Kind
import Data.List

import Expression.Sort
import Utils.Indexed.Fixpoint
import Utils.Indexed.Functor
import Utils.Indexed.Show
import Utils.Indexed.Sum

import qualified Data.Functor.Const as F

data ArithmeticF a (s :: Sort) where
    Const    :: Int              -> ArithmeticF a IntegralSort
    Add      :: [a IntegralSort] -> ArithmeticF a IntegralSort
    Mul      :: [a IntegralSort] -> ArithmeticF a IntegralSort
    LessThan :: a IntegralSort -> a IntegralSort -> ArithmeticF a BooleanSort

instance IFunctor ArithmeticF where
    imap _ (Const c)        = Const c
    imap f (Add as)         = Add $ map f as
    imap f (Mul ms)         = Mul $ map f ms
    imap f (a `LessThan` b) = f a `LessThan` f b

instance IShow ArithmeticF where
    ishow (Const c)        = F.Const $ show c
    ishow (Add as)         = F.Const $ "(+ " ++ intercalate " " (map F.getConst as) ++ ")"
    ishow (Mul ms)         = F.Const $ "(* " ++ intercalate " " (map F.getConst ms) ++ ")"
    ishow (a `LessThan` b) = F.Const $ "(< " ++ F.getConst a ++ " " ++ F.getConst b ++ ")"

cnst :: ArithmeticF :<: f => Int -> IFix f IntegralSort
cnst = inject . Const

(.+.), (.*.) :: ArithmeticF :<: f => IFix f IntegralSort -> IFix f IntegralSort -> IFix f IntegralSort
a .+. b = inject $ Add [a, b]
a .*. b = inject $ Mul [a, b]

add, mul :: ArithmeticF :<: f => [IFix f IntegralSort] -> IFix f IntegralSort
add = inject . Add
mul = inject . Mul

(.<.) :: ArithmeticF :<: f => IFix f IntegralSort -> IFix f IntegralSort -> IFix f BooleanSort
a .<. b = inject $ a `LessThan` b

infix 9 .*.
infix 8 .+.
infix 7 .<.
