module Kanren.Term where
    
import Kanren.Var
import Kanren.Obj    
    
data Term
  = TmVar Var
  | TmObj Obj
  | TmPair Term Term

instance showTerm :: Show Term where
  show (TmVar v) = "(TmVar " ++ show v ++ ")"
  show (TmObj o) = "(TmObj " ++ show o ++ ")"
  show (TmPair t1 t2) = "(TmPair " ++ show t1 ++ " " ++ show t2 ++ ")"
  
obj :: String -> Term
obj nm = TmObj (Obj nm)