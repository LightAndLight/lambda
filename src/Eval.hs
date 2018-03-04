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
        replaceArg x (VVar (Env 0)) = x
        replaceArg x (VClosure env body) =
          VClosure (replaceArg x <$> env) (replaceArg x body)
        replaceArg x (VApp a b) = VApp (replaceArg x a) (replaceArg x b)
        replaceArg _ a = a

        reify e =
          case e of
            VString s -> VApp (VCtor "String") (VString s)
            VInt n -> VApp (VCtor "Int") (VInt n)
            VCtor s -> VApp (VCtor "Ctor") (VString s)
            VVar n -> error "this shouldn't happen"
            VClosure env body ->
              VApp
                (VCtor "Abs")
                (VClosure env $
                 replaceArg (VApp (VCtor "Var") (VVar $ Env 0)) body)
            VApp a b -> VApp (VApp (VCtor "App") (reify a)) (reify b)
        reflect env e = 
          case e of
            VApp (VCtor "Abs") (VClosure env' x) ->
              VClosure env' (reflect env' x)
            VApp (VApp (VCtor "App") f) x -> VApp (reflect env f) (reflect env x)
            VApp (VCtor "Ctor") (VString s) -> VCtor s
            VApp (VCtor "Var") (VVar v) -> VVar v
            VApp (VCtor "Int") (VInt n) -> VInt n
            VApp (VCtor "Value") v -> v
            _ -> error $ "reflection failure: " ++ show e
