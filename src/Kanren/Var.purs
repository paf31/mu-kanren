module Kanren.Var where

newtype Var = Var Number

instance eqVar :: Eq Var where
  (==) (Var v1) (Var v2) = v1 == v2
  (/=) (Var v1) (Var v2) = v1 /= v2

instance showVar :: Show Var where
  show (Var v) = show v

zero :: Var
zero = Var 0

succ :: Var -> Var
succ (Var n) = Var $ n + 1