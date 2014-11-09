module Kanren.State where
    
import Kanren.Subst
import Kanren.Var    
import Kanren.Stack
import Kanren.Goal
    
data State = State Goal Subst Var Stack

instance showState :: Show State where
  show (State goal subst var stack) = 
    "(State" ++ 
    " " ++ show goal ++
    " " ++ show subst ++ 
    " " ++ show var ++
    " " ++ show stack ++
    ")"

stateGoal :: State -> Goal
stateGoal (State g _ _ _) = g

stateSubst :: State -> Subst
stateSubst (State _ s _ _) = s

stateVar :: State -> Var
stateVar (State _ _ v _) = v

stateStack :: State -> Stack
stateStack (State _ _ _ s) = s
