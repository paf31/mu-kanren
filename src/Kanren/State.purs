module Kanren.State where
    
import Kanren.Subst
import Kanren.Var    
import Kanren.Stack
import Kanren.Goal
    
data State = State Goal Subst Var Stack [State]

instance showState :: Show State where
  show (State goal subst var rgs history) = 
    "(State" ++ 
    " " ++ show goal ++
    " " ++ show subst ++ 
    " " ++ show var ++
    " " ++ show rgs ++
    " " ++ show history ++
    ")"

stateGoal :: State -> Goal
stateGoal (State g _ _ _ _) = g

stateSubst :: State -> Subst
stateSubst (State _ s _ _ _) = s

stateVar :: State -> Var
stateVar (State _ _ v _ _) = v

stateStack :: State -> Stack
stateStack (State _ _ _ s _) = s

stateHistory :: State -> [State]
stateHistory (State _ _ _ _ h) = h
