{-# LANGUAGE DataKinds
           , FlexibleContexts
           , GADTs
           , RankNTypes
           , TypeInType
           , TypeOperators #-}

module Expression ( module Expression
                  , module Expression.Arithmetic
                  , module Expression.Array
                  , module Expression.Equality
                  , module Expression.Sort
                  , module Expression.Sort.Dynamic ) where

import Data.Kind
import Data.List

import Expression.Arithmetic
import Expression.Array
import Expression.Equality
import Expression.Sort
import Expression.Sort.Dynamic
import Utils.Indexed.Fixpoint
import Utils.Indexed.Functor
import Utils.Indexed.Show
import Utils.Indexed.Sum

import qualified Data.Functor.Const as F

type QFLogicF = EqualityF :+: BoolF :+: VarF
type QFLiaF   = ArithmeticF :+: QFLogicF
type   LiaF   = QuantifiedF BooleanSort :+: QuantifiedF IntegralSort :+: QFLiaF
type QFALiaF  = ArrayF :+: QFLiaF
type   ALiaF  = QuantifiedF BooleanSort :+: QuantifiedF IntegralSort :+: QFALiaF

type QFLogic = IFix QFLogicF
type QFLia   = IFix QFLiaF
type   Lia   = IFix   LiaF
type QFALia  = IFix QFALiaF
type   ALia  = IFix   ALiaF

type VariableName = String

data VarF a (s :: Sort) where
    Var :: VariableName -> Sing s -> VarF a s

instance IFunctor VarF where
    imap _ (Var n s)  = Var n s

instance IShow VarF where
    ishow (Var n s)  = F.Const ("(" ++ n ++ " : " ++ show s ++ ")")

var :: VarF :<: f => forall s. VariableName -> Sing s -> IFix f s
var n s = inject (Var n s)

data BoolF a (s :: Sort) where
    And :: [a BooleanSort] -> BoolF a BooleanSort
    Or  :: [a BooleanSort] -> BoolF a BooleanSort
    Not ::  a BooleanSort  -> BoolF a BooleanSort

instance IFunctor BoolF where
    imap f (And as) = And $ map f as
    imap f (Or  os) = Or  $ map f os
    imap f (Not n)  = Not $ f n

instance IShow BoolF where
    ishow (And as) = F.Const $ "(and " ++ intercalate " " (map F.getConst as) ++ ")"
    ishow (Or  os) = F.Const $ "(or "  ++ intercalate " " (map F.getConst os) ++ ")"
    ishow (Not n)  = F.Const $ "(not " ++ F.getConst n ++ ")"

(.&.), (.|.) :: BoolF :<: f => IFix f BooleanSort -> IFix f BooleanSort -> IFix f BooleanSort
a .&. b = inject $ And [a, b]
a .|. b = inject $ Or  [a, b]

infix 6 .&.
infix 5 .|.

true, false :: BoolF :<: f => IFix f BooleanSort
true  = inject $ And []
false = inject $ Or  []

and, or :: BoolF :<: f => [IFix f BooleanSort] -> IFix f BooleanSort
and = inject . And
or  = inject . Or

not :: BoolF :<: f => IFix f BooleanSort -> IFix f BooleanSort
not = inject . Not

data QuantifiedF (v :: Sort) a (s :: Sort) where
    Forall :: [IFix VarF v] -> a BooleanSort -> QuantifiedF v a BooleanSort
    Exists :: [IFix VarF v] -> a BooleanSort -> QuantifiedF v a BooleanSort

instance IFunctor (QuantifiedF v) where
    imap f (Forall vs phi) = Forall vs $ f phi
    imap f (Exists vs phi) = Exists vs $ f phi

instance IShow (QuantifiedF v) where
    ishow (Forall vs phi) = F.Const $ "(forall (" ++ intercalate " " (map show vs) ++ ") " ++ F.getConst phi ++ ")"
    ishow (Exists vs phi) = F.Const $ "(exists (" ++ intercalate " " (map show vs) ++ ") " ++ F.getConst phi ++ ")"

forall, exists :: QuantifiedF v :<: f => [IFix VarF v] -> IFix f BooleanSort -> IFix f BooleanSort
forall vs f = inject $ Forall vs f
exists vs f = inject $ Exists vs f
