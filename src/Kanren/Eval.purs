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
  goal = Fresh $ \t1 -> Fresh $ \t2 -> Disj (g1 t1 t2) (g2 t1 t2)
  
  g1 t1 t2 = Conj (Unify t1 (TmObj (Obj "a")))
                  (Unify t1 t2)

  g2 t1 t2 = Conj (Unify t1 (TmObj (Obj "b")))
                  (Unify t1 t2)

step :: State -> [State]
step st@(State goal subst var stack) = 
  case goal of
    Done -> 
      case stack of
        Empty -> []
	Push goal' stack' ->
          [ State goal' subst var stack' ]
    Unify u v -> 
      case unify u v subst of
        Nothing -> []
        Just subst' -> [State Done subst' var stack]
    Fresh f ->
      [ State (f (TmVar var)) subst (succ var) stack ]
    Disj g1 g2 ->
      [ State g1 subst var stack
      , State g2 subst var stack
      ]
    Conj g1 g2 ->
      [ State g1 subst var (Push g2 stack) ]


