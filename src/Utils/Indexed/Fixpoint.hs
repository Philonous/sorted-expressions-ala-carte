{-# LANGUAGE TypeInType #-}

module Utils.Indexed.Fixpoint (IFix(..)) where

data IFix f i = IFix { unIFix :: f (IFix f) i }
