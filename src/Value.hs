module Value where

import Bound.Scope
import Data.Foldable
import Data.Int
import Data.Maybe

import Expr

data Reference a = Free a | Env Int deriving (Eq, Show)

data Value' a
  = VInt Int16
  | VVar (Reference a)
  | VClosure [Reference a] (Value' a)
  | VApp (Value' a) (Value' a)
  deriving (Eq, Show)
type Value = Value' String

flattenReference :: Reference (Reference a) -> Reference a
flattenReference (Free (Free a)) = Free a
flattenReference (Free (Env n)) = Env n
flattenReference (Env n) = Env n

flattenValue :: Value' (Reference a) -> Value' a
flattenValue (VVar a) =
  VVar $ case a of
    Free x -> x
    Env n -> Env n
flattenValue (VInt a) = VInt a
flattenValue (VClosure env a) =
  VClosure (flattenReference <$> env) (flattenValue a)
flattenValue (VApp a b) = VApp (flattenValue a) (flattenValue b)

exprValue :: Eq a => Expr' a -> Value' a
exprValue (Int x) = VInt x
exprValue (Var x) = VVar $ Free x
exprValue (Abs s) =
  let
    env = Free <$> toList s
    numbered = zip env (Env <$> [1..])
  in
    VClosure
      env
      (flattenValue . exprValue $
       splat
         (Var . fromJust . flip lookup numbered)
         (const $ Var (Env 0))
         (fmap Free s))
exprValue (App f x) = VApp (exprValue f) (exprValue x)
