module Kanren.State where
    
import Kanren.Subst
import Kanren.Var    
    
data State = State Subst Var

stateSubst :: State -> Subst
stateSubst (State s _) = s

stateVar :: State -> Var
stateVar (State _ v) = v