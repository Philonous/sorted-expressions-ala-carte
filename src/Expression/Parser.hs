{-# LANGUAGE AllowAmbiguousTypes
           , FlexibleContexts
           , GADTs
           , OverloadedStrings
           , ScopedTypeVariables
           , TypeInType
           , TypeOperators #-}

module Expression.Parser ( qflia, qfalia, lia, alia ) where

import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Data.Functor.Const
import Data.Kind
import Data.Proxy
import Data.Text
import Data.Typeable
import Data.Singletons.Decide

import Expression hiding ( Const, var )
import Utils.Indexed.Fixpoint
import Utils.Indexed.Sum

import qualified Expression as E

qflia :: forall f. ( VarF        :<: f
                   , BoolF       :<: f
                   , EqualityF   :<: f
                   , ArithmeticF :<: f ) => Parser (DynamicallySorted f)
qflia = choice $ [ const var, bool, equality, arithmetic ] <*> [ qflia ]

qfalia :: forall f. ( VarF        :<: f
                    , BoolF       :<: f
                    , EqualityF   :<: f
                    , ArithmeticF :<: f
                    , ArrayF      :<: f ) => Parser (DynamicallySorted f)
qfalia = choice $ [ const var, bool, equality, arithmetic, array ] <*> [ qfalia ]

lia :: forall f. ( VarF                     :<: f
                 , BoolF                    :<: f
                 , EqualityF                :<: f
                 , ArithmeticF              :<: f
                 , QuantifiedF BooleanSort  :<: f
                 , QuantifiedF IntegralSort :<: f ) => Parser (DynamicallySorted f)
lia = choice $ [ const var
               , bool
               , equality
               , arithmetic
               , quantified (Proxy :: Proxy BooleanSort)
               , quantified (Proxy :: Proxy IntegralSort) ] <*> [ lia ]

alia :: forall f. ( VarF                     :<: f
                  , BoolF                    :<: f
                  , EqualityF                :<: f
                  , ArithmeticF              :<: f
                  , ArrayF                   :<: f
                  , QuantifiedF BooleanSort  :<: f
                  , QuantifiedF IntegralSort :<: f ) => Parser (DynamicallySorted f)
alia = choice $ [ const var
                , bool
                , equality
                , arithmetic
                , array
                , quantified (Proxy :: Proxy BooleanSort)
                , quantified (Proxy :: Proxy IntegralSort) ] <*> [ alia ]

sort :: Parser DynamicSort
sort = choice [ bool, int, array ] <?> "Sort" where
        bool  = string "bool" *> pure (DynamicSort SBooleanSort)
        int   = string "int"  *> pure (DynamicSort SIntegralSort)
        array = array' <$> (string "array" *> space *> sort') <*> (space *> sort')

        sort' = choice [ bool, int, char '(' *> array <* char ')' ]

        array' :: DynamicSort -> DynamicSort -> DynamicSort
        array' (DynamicSort i) (DynamicSort e) = DynamicSort (SArraySort i e)

var :: forall f. VarF :<: f => Parser (DynamicallySorted f)
var = var' <$> (char '(' *> many1 letter) <*> (space *> char ':' *> space *> sort <* char ')') <?> "Var" where
    var' :: VariableName -> DynamicSort -> DynamicallySorted f
    var' n (DynamicSort (s :: Sing s)) =
      -- This shouldn't be necessary once DynamicallySorted is fixed
        withTypeableSort s $ DynamicallySorted s $ E.var n s

bool :: forall f. BoolF :<: f => Parser (DynamicallySorted f) -> Parser (DynamicallySorted f)
bool rec = choice [ true, false, and, or, not ] <?> "Bool" where
    true  = string "true"  *> pure (DynamicallySorted SBooleanSort $ E.true)
    false = string "false" *> pure (DynamicallySorted SBooleanSort $ E.false)

    and = and' <$> (char '(' *> string "and" *> space *> (rec `sepBy1` space) <* char ')')
    or  = or'  <$> (char '(' *> string "or " *> space *> (rec `sepBy1` space) <* char ')')
    not = not' <$> (char '(' *> string "not" *> space *>  rec                 <* char ')')

    and' as = case mapM toBool as of
        Just as -> DynamicallySorted SBooleanSort $ E.and as
        Nothing -> error "and of non-boolean arguments"

    or' os = case mapM toBool os of
        Just os -> DynamicallySorted SBooleanSort $ E.or os
        Nothing -> error "or of non-boolean arguments"

    not' n = case toBool n of
        Just n -> DynamicallySorted SBooleanSort $ E.not n
        Nothing -> error "not of non-boolean arguments"

quantified :: forall f v. ( QuantifiedF v :<: f, Typeable v ) => Proxy v -> Parser (DynamicallySorted f) -> Parser (DynamicallySorted f)
quantified _ rec = choice [ forall, exists ] <?> "Quantified" where
    forall = quantified E.forall "forall"
    exists = quantified E.exists "exists"

    quantified q k = do
        char '(' *> string k *> space *> char '('
        vs <- var `sepBy1` space
        char ')' *> space
        phi <- rec
        char ')'
        quantifier q vs phi

    quantifier _ [] _   = error "quantifying zero variables"
    quantifier q vs phi = case toBool phi of
        Just phi -> case (mapM toStaticallySorted vs :: Maybe [IFix VarF v]) of
            Just vs -> return . DynamicallySorted SBooleanSort $ q vs phi
            Nothing -> fail "ill-sorted quantifier"
        Nothing  -> error "quantifying non-boolean expression"

equality :: forall f. EqualityF :<: f => Parser (DynamicallySorted f) -> Parser (DynamicallySorted f)
equality rec = equals <$> (char '(' *> char '=' *> space *> rec) <*> (space *> rec <* char ')') <?> "Equality" where
    equals (DynamicallySorted _ (a :: IFix f s1))
           (DynamicallySorted _ (b :: IFix f s2)) = case eqT :: Maybe (s1 :~: s2) of
        Just Refl -> DynamicallySorted SBooleanSort $ a .=. b
        Nothing   -> error "multi-sorted equality"

arithmetic :: forall f. ArithmeticF :<: f => Parser (DynamicallySorted f) -> Parser (DynamicallySorted f)
arithmetic rec = choice [ cnst, add, mul, lessThan ] <?> "Arithmetic" where
    cnst = DynamicallySorted SIntegralSort . E.cnst <$> signed decimal
    add  = add' <$> (char '(' *> char '+' *> space *> (rec `sepBy1` space) <* char ')')
    mul  = mul' <$> (char '(' *> char '*' *> space *> (rec `sepBy1` space) <* char ')')

    lessThan = lessThan' <$> (char '(' *> char '<' *> space *> rec) <*> (space *> rec <* char ')')

    add' as = case mapM toInt as of
        Just as -> DynamicallySorted SIntegralSort (E.add as)
        Nothing -> error "add of non-integral arguments"
    mul' ms = case mapM toInt ms of
        Just ms -> DynamicallySorted SIntegralSort (E.mul ms)
        Nothing -> error "mul of non-integral arguments"

    lessThan' a b = case mapM toInt [a, b] of
        Just [a, b] -> DynamicallySorted SBooleanSort $ a .<. b
        _           -> error "less-than of non-integral arguments"

    toInt :: DynamicallySorted f -> Maybe (IFix f IntegralSort)
    toInt = toStaticallySorted

array :: forall f. ArrayF :<: f => Parser (DynamicallySorted f) -> Parser (DynamicallySorted f)
array rec = choice [ select, store ] <?> "Array" where
    select = select' <$> (char '(' *> string "select" *> space *> rec) <*> (space *> rec <* char ')')
    store  = store'  <$> (char '(' *> string "store"  *> space *> rec) <*> (space *> rec) <*> (space *> rec <* char ')')

    select' :: DynamicallySorted f -> DynamicallySorted f -> DynamicallySorted f
    -- select' (DynamicallySorted ss1@(SArraySort _ ss3) a)
    --         (DynamicallySorted ss2                    i)
    --            = case sortEq ss1 (SArraySort ss2 ss3) of
    --                    Just Refl -> withTypeableSort ss3
    --                                    $ DynamicallySorted ss3 (E.select a i)
    --                    Nothing   -> error ""
    select' (DynamicallySorted (SArraySort si1 se1) a)
            (DynamicallySorted s2               i)
               = case si1 %~ s2 of
                       Proved Refl -> withTypeableSort se1
                                      $ DynamicallySorted se1 (E.select a i)
                       Disproved _   -> error ""
    select' _ _ = error "selecting from non-array"
    store'  (DynamicallySorted as@(SArraySort _ _) (a :: IFix f s1))
            (DynamicallySorted _                   (i :: IFix f s2))
            (DynamicallySorted _                   (v :: IFix f s3)) = case eqT :: Maybe (s1 :~: ArraySort s2 s3) of
        Just Refl -> DynamicallySorted as (E.store a i v)
        Nothing   -> error ""
    store' _ _ _ = error "storing to non-array"

toBool :: DynamicallySorted f -> Maybe (IFix f BooleanSort)
toBool = toStaticallySorted
