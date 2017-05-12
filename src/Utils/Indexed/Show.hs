{-# LANGUAGE TypeInType #-}

module Utils.Indexed.Show where

import Data.Functor.Const

class IShow f where
    ishow :: f (Const String) i -> Const String i
