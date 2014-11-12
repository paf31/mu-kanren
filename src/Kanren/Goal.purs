module Kanren.Goal where    
    
import Data.Foldable (intercalate)    
    
import Kanren.Term
import Kanren.Var    
    
data Define = Define String [String] Goal 
    
instance showDefine :: Show Define where    
  show (Define nm args g) = "(Define" ++   
    " " ++ show nm ++
    " " ++ show args ++
    " " ++ show g ++
    ")" 
    
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

