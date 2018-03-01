module Compile where

import Data.Semigroup

import Stack
import Value

compile :: [(String, [Inst])] -> Value -> [Inst]
compile env (VInt n) = [Push $ fromIntegral n]
compile env (VVar s)
  | Just v <- lookup s env = v
  | otherwise = error $ s <> " not found in environment"
compile env (VApp v1 v2) = undefined
compile env (VClosure frees bound v) = undefined
