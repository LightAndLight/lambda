{-# language DeriveFunctor, StandaloneDeriving, TemplateHaskell #-}
module CPS where

import Bound
import Bound.Scope
import Data.Deriving
import Data.Int
import Data.List
import Data.Semigroup

import Expr

data CPS' a
  = CInt Int16
  | CVar a
  | CAbs Int (Scope Int CPS' a)
  | CApp (CPS' a) [CPS' a]
  deriving Functor
makeBound ''CPS'
deriveEq1 ''CPS'
deriveShow1 ''CPS'
deriving instance Eq a => Eq (CPS' a)
deriving instance Show a => Show (CPS' a)
type CPS = CPS' String
renderCPS :: CPS -> String
renderCPS = snd . go (("x"<>) . show <$> [0..])
  where
    loop supply f [] = (supply, [])
    loop supply f (x:xs) =
      let
        (supply', x') = f supply x
        (supply'', xs') = loop supply' f xs
      in
        ( supply''
        , (case x of; CAbs{} -> bracket; CApp{} -> bracket; _ -> id) x' : xs'
        )

    bracket s = "(" <> s <> ")"
    go supply (CInt n) = (supply, show n)
    go supply (CVar s) = (supply, s)
    go supply (CApp x y) =
      let
        (supply', str') = loop supply go (x:y)
      in
        (supply', intercalate " " str')
    go ss (CAbs n e) = 
       let
         (ss', str) = go (drop n ss) (instantiateVars (take n ss) e)
       in
         (ss', "\\" <> (intercalate " " $ take n ss) <> ". " <> str)

clam :: String -> CPS -> CPS
clam n e = CAbs 1 (abstract (`elemIndex` [n]) e)

cps :: Expr -> CPS -> CPS
cps = t_c
  where
    m :: Expr -> CPS
    m (Int n) = CInt n
    m (Var s) = CVar s
    m (Abs s) =
      CAbs 2 .
      abstract (`elemIndex` ["x", "k"])
      $ t_c (instantiate1 (Var "x") s) (CVar "k")
    m (App _ _) = undefined

    t_c :: Expr -> CPS -> CPS
    t_c (App f x) k =
      t_k f $ \f' ->
      t_k x $ \x' ->
      CApp f' [x', k]
    t_c a k = CApp k [m a]

    t_k :: Expr -> (CPS -> CPS) -> CPS
    t_k (App f x) k =
      t_k f $ \f' ->
      t_k x $ \x' ->
      CApp f' [x', clam "k2" $ k (CVar "k2")]
    t_k a k = k (m a)
