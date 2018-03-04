module Eval where

import Bound

import Expr
import Value

lookupEnv :: [Value' a] -> Reference a -> Value' a
lookupEnv env r =
  case r of
    Env n -> env !! n
    Free x -> VVar $ Free x

eval :: Value -> Value
eval = go []
  where
    ctorOnLeft VCtor{} = True
    ctorOnLeft (VApp a _) = ctorOnLeft a
    ctorOnLeft _ = False

    go env (VApp (VCtor "Var") s) = VApp (VCtor "Var") s
    go env (VCtor s) = VCtor s
    go env (VVar r) = lookupEnv env r
    go env (VInt n) = VInt n
    go env (VClosure env' s) = VClosure (go env <$> env') s
    go env (VApp a b) =
      let
        b' = go env b
      in
        case go env a of
          VClosure env' s -> go (b' : env') s
          VVar (Free "eval") -> reflect env b'
          a'
            | ctorOnLeft a' -> VApp a' b'
            | otherwise -> error "stuck term"
      where
        reflect env e = 
          case e of
            VApp (VCtor "Abs") (VClosure env' x) ->
              VClosure env' (reflect env' $ go (undefined : env') x)
            VApp (VApp (VCtor "App") f) x -> VApp (reflect env f) (reflect env x)
            VApp (VCtor "Var") (VVar v) -> VVar v
            VApp (VCtor "Int") (VInt n) -> VInt n
            _ -> error $ "reflection failure: " ++ show e
