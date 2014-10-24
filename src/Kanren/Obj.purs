module Kanren.Obj where
    
newtype Obj = Obj String

instance eqObj :: Eq Obj where
  (==) (Obj o1) (Obj o2) = o1 == o2
  (/=) (Obj o1) (Obj o2) = o1 /= o2

instance showObj :: Show Obj where
  show (Obj o) = o