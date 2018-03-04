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
    go env (VString s) = VString s
    go env (VClosure env' s) = VClosure (go env <$> env') s
    go env (VApp a b) =
      let
        b' = go env b
      in
        case go env a of
          VClosure env' s -> go (b' : env') s
          VVar (Free "eval") -> reflect env b'
          VVar (Free "quote") -> reify b'
          a'
            | ctorOnLeft a' -> VApp a' b'
            | otherwise -> error "stuck term"
      where
        reify e =
          case e of
            VString s -> VApp (VCtor "String") (VString s)
            VInt n -> VApp (VCtor "Int") (VInt n)
            VCtor s -> VApp (VCtor "Ctor") (VString s)
            VVar n -> undefined -- VApp (VCtor "Var") (VVar n)
            VClosure env body ->
              VApp
                (VCtor "Abs")
                (VClosure env $ VApp (VVar (Free "quote")) body)
                -- (VClosure env $ reify body)
            VApp a b -> undefined -- VApp (VApp (VCtor "App") (reify a)) (reify b)

        reflect env e =
          case e of
            VApp (VCtor "Abs") (VClosure env' x) ->
              VClosure env' (VApp (VVar $ Free "eval") x)
            VApp (VApp (VCtor "App") f) x ->
              go env $
              VApp
                (VApp (VVar $ Free "eval") f)
                (VApp (VVar $ Free "eval") x)
            VApp (VCtor "Ctor") (VString s) -> VCtor s
            VApp (VCtor "Var") (VVar v) -> lookupEnv env v
            VApp (VCtor "Int") (VInt n) -> VInt n
            VApp (VCtor "Value") v -> v
            VApp (VVar (Free "quote")) v -> v
            _ -> error $ "reflection failure: " ++ show e
