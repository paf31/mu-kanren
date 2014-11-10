module Kanren.Goal
  ( Goal(..)
  ) where    
    
import Data.Foldable (intercalate)    
    
import Kanren.Term
import Kanren.Var    
    
data Goal 
  = Done
  | Fail
  | Unify Term Term
  | Fresh [String] Goal
  | Disj [Goal]
  | Conj [Goal]
  | Named String [Term]

instance showGoal :: Show Goal where
  show Done = "Done"
  show Fail = "Fail"
  show (Unify t1 t2) = "(Unify" ++
    " " ++ show t1 ++
    " " ++ show t2 ++
    ")"
  show (Fresh nms g) = "(Fresh" ++
    " " ++ intercalate " " nms ++
    " " ++ show g ++
    ")"
  show (Disj gs) = "(Disj" ++ 
    " " ++ intercalate " " (show <$> gs) ++
    ")"
  show (Conj gs) = "(Conj" ++ 
    " " ++ intercalate " " (show <$> gs) ++
    ")"
  show (Named name ts) = "(" ++ 
    name ++
    " " ++ intercalate " " (show <$> ts) ++
    ")"

