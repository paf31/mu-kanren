module Kanren.Obj where

import Prelude

newtype Obj = Obj String

instance eqObj :: Eq Obj where
  eq (Obj o1) (Obj o2) = o1 == o2

instance showObj :: Show Obj where
  show (Obj o) = "(Obj " <> o <> ")"
