module Kanren.State where

import Prelude
import Data.List (List, (:))
import Kanren.Goal (Goal(..))
import Kanren.Stack (Stack)
import Kanren.Subst (Subst)
import Kanren.Var (Var)

data State = State Goal Subst Var Stack (List State)

instance showState :: Show State where
  show (State goal subst var rgs history) =
    "(State" <>
    " " <> show goal <>
    " " <> show subst <>
    " " <> show var <>
    " " <> show rgs <>
    " " <> show history <>
    ")"

stateGoal :: State -> Goal
stateGoal (State g _ _ _ _) = g

stateSubst :: State -> Subst
stateSubst (State _ s _ _ _) = s

stateVar :: State -> Var
stateVar (State _ _ v _ _) = v

stateStack :: State -> Stack
stateStack (State _ _ _ s _) = s

stateHistory :: State -> List State
stateHistory (State _ _ _ _ h) = h

pushGoal :: Goal -> State -> State
pushGoal goal st@(State _ subst var stack hist) =
  State goal subst var stack (st : hist)

pushUnsolvedGoals :: List Goal -> State -> State
pushUnsolvedGoals unsolved (State cur subst var stack hist) =
  State cur subst var (unsolved <> stack) hist

unwindStack :: State -> State
unwindStack (State Done subst var (goal : rest) hist) = State goal subst var rest hist
unwindStack other = other
