module Kanren.Eval where

import Data.Maybe

import Kanren.Var
import Kanren.Obj
import Kanren.Term
import Kanren.Subst
import Kanren.State
import Kanren.Stack
import Kanren.Stream
import Kanren.Goal
import Kanren.Unify

example :: State
example = State goal [] zero Empty
  where
  goal :: Goal
  goal = Fresh "x" $ Fresh "y" $ Disj g1 g2
  
  g1 = Conj (Unify (obj "x") (obj "a"))
            (Unify (obj "x") (obj "y"))

  g2 = Conj (Unify (obj "x") (obj "b"))
            (Unify (obj "x") (obj "y"))

  obj nm = TmObj (Obj nm)

step :: State -> [State]
step st@(State goal subst var stack) = unwind <$> go goal
  where
  go Done = []
  go (Unify u v) = 
    case unify u v subst of
      Nothing -> []
      Just subst' -> [State Done subst' var stack]
  go (Fresh nm g) = [ State (replace nm (TmVar var) g) subst (succ var) stack ]
  go (Disj g1 g2) = [ State g1 subst var stack
                    , State g2 subst var stack
                    ]
  go (Conj g1 g2) = [ State g1 subst var (Push g2 stack) ]      
      
  unwind (State Done subst var (Push goal stack)) = State goal subst var stack
  unwind other = other     
      
  replace :: String -> Term -> Goal -> Goal
  replace nm r = onGoals
    where
    onGoals Done = Done
    onGoals (Unify u v) = Unify (onTerms u) (onTerms v)
    onGoals f@(Fresh nm' g) = if nm == nm' then f else Fresh nm' (onGoals g)
    onGoals (Disj g1 g2) = Disj (onGoals g1) (onGoals g2)
    onGoals (Conj g1 g2) = Conj (onGoals g1) (onGoals g2)

    onTerms (TmObj (Obj nm')) | nm == nm' = r
    onTerms (TmPair t1 t2) = TmPair (onTerms t1) (onTerms t2)
    onTerms other = other