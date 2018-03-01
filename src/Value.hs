module Value where

import Data.Int

import CPS

data Value
  = VInt Int16
  | VVar String
  | VApp Value Value
  | VClosure [String] [String] Value

toValue :: CPS -> Value
toValue = undefined
