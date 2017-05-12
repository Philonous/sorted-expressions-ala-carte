{-# LANGUAGE TypeInType #-}

module Utils.Indexed where

newtype IConst a i = IConst { unIConst :: a }
