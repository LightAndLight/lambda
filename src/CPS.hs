{-# language DeriveFunctor, StandaloneDeriving, TemplateHaskell #-}
module CPS where

import Bound
import Bound.Var
import Data.List

import Expr

-- | De Bruijn CPS transformation ala Danvy and Nielsen
cps :: Expr' a -> Expr' a -> Expr' a
cps = cps'
  where
    isAtomic App{} = False
    isAtomic Var{} = True
    isAtomic Abs{} = True

    cps' :: Expr' a -> Expr' a -> Expr' a
    cps' e@Var{} k = App k (phi e)
    cps' e@Abs{} k = App k (phi e)
    cps' (App a b) k =
      case (isAtomic a, isAtomic b) of
        (True, True) -> App (App (phi a) (phi b)) k
        (False, True) ->
          cps' a $ Abs . Scope $
          App
            (App
              (Var $ B ())
              (fmap (F . Var) $ phi b))
            (fmap (F . Var) k)
        (True, False) ->
          cps' b $ Abs . Scope $
          App
            (App
              (fmap (F . Var) $ phi a)
              (Var $ B ()))
            (fmap (F . Var) k)
        (False, False) ->
          cps' a $ Abs . Scope $
          cps' (fmap (F . Var) b) $ Abs . Scope $
          App
            (App
              (Var . F . Var $ B ())
              (Var $ B ()))
            (fmap (F . Var . F . Var) k)

    phi :: Expr' a -> Expr' a
    phi (Var x) = Var x
    phi (Abs s) =
      Abs . Scope . Abs . Scope $
      cps'
        (unvar (F . Var . B) (F . Var . F . Var) <$> fromScope s)
        (Var $ B ())
    phi App{} = undefined
