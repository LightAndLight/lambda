{-# language DeriveFunctor, DeriveFoldable, StandaloneDeriving,
  TemplateHaskell #-}
module Expr where

import Bound
import Data.Deriving
import Data.Int
import Data.Semigroup

data Expr' a
  = Var a
  | Abs (Scope () Expr' a)
  | App (Expr' a) (Expr' a)
  | Ctor String
  | Int Int16
  deriving (Functor, Foldable)
makeBound ''Expr'
deriveEq1 ''Expr'
deriveShow1 ''Expr'
deriving instance Eq a => Eq (Expr' a)
deriving instance Show a => Show (Expr' a)
type Expr = Expr' String
renderExpr :: Expr -> String
renderExpr = snd . go (("x"<>) . show <$> [0..])
  where
    bracket s = "(" <> s <> ")"
    go supply (Ctor s) = (supply, s)
    go supply (Var s) = (supply, s)
    go supply (App x y) =
      let
        (supply', str) = go supply x
        (supply'', str') = go supply' y
      in
        ( supply''
        , (case x of Abs{} -> bracket; _ -> id) str <> " " <>
          (case y of Abs{} -> bracket; App{} -> bracket; _ -> id) str'
        )
    go (s:ss) (Abs e) = 
       let
         (ss', str) = go ss (instantiate1 (Var s) e)
       in
         (ss', "\\" <> s <> ". " <> str)
    go _ _ = undefined

lam :: String -> Expr -> Expr
lam n e = Abs (abstract1 n e)
