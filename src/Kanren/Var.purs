module Kanren.Var where

import Prelude

newtype Var = Var Int

runVar :: Var -> Int
runVar (Var v) = v

instance eqVar :: Eq Var where
  eq (Var v1) (Var v2) = v1 == v2

instance ordVar :: Ord Var where
  compare (Var v1) (Var v2) = compare v1 v2

instance showVar :: Show Var where
  show (Var v) = "(Var " <> show v <> ")"

zero :: Var
zero = Var 0

succ :: Var -> Var
succ (Var n) = Var $ n + 1
