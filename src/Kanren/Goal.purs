module Kanren.Goal
  ( Goal(..)
  ) where    
    
import Kanren.Term
import Kanren.Var    
    
data Goal 
  = Done
  | Unify Term Term
  | Fresh (Term -> Goal)
  | Disj Goal Goal
  | Conj Goal Goal

instance showGoal :: Show Goal where
  show Done = "Done"
  show (Unify t1 t2) = "(Unify" ++
    " " ++ show t1 ++
    " " ++ show t2 ++
    ")"
  show (Fresh _) = "(Fresh ?)"
  show (Disj g1 g2) = "(Disj" ++ 
    " " ++ show g1 ++
    " " ++ show g2 ++
    ")"
  show (Conj g1 g2) = "(Conj" ++ 
    " " ++ show g1 ++
    " " ++ show g2 ++
    ")"

