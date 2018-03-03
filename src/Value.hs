module Value where

import Data.Int

import Expr

data Value
  = VInt Int16
  | VVar String
  | VApp Value Value
  | VClosure [String] String Value
