module Kanren.Eval where

import Data.Maybe
import Data.Tuple
import Data.Foldable (foldl, elem)

import Kanren.Var
import Kanren.Obj
import Kanren.Term
import Kanren.Subst
import Kanren.State
import Kanren.Stack
import Kanren.Goal
import Kanren.Unify

example :: State
example = State goal [] zero []
  where
  goal :: Goal
  goal = Fresh ["xs", "ys"] $ Named "appendo" [obj "xs", obj "ys", TmPair (obj "a") (TmPair (obj "b") (TmPair (obj "c") (obj "nil")))]

  obj nm = TmObj (Obj nm)   
      
replace :: String -> Term -> Goal -> Goal
replace nm r = onGoals
  where
  onGoals Done = Done
  onGoals (Unify u v) = Unify (onTerms u) (onTerms v)
  onGoals f@(Fresh ns g) = if nm `elem` ns then f else Fresh ns (onGoals g)
  onGoals (Disj g1 g2) = Disj (onGoals g1) (onGoals g2)
  onGoals (Conj g1 g2) = Conj (onGoals g1) (onGoals g2)
  onGoals (Named name ts) = Named name (onTerms <$> ts)

  onTerms (TmObj (Obj nm')) | nm == nm' = r
  onTerms (TmPair t1 t2) = TmPair (onTerms t1) (onTerms t2)
  onTerms other = other
  
replaceAll :: [Tuple String Term] -> Goal -> Goal
replaceAll = foldl (\f (Tuple nm r) g -> replace nm r (f g)) id
  
builtIn :: String -> [Term] -> Goal
builtIn "appendo" [xs, ys, zs] = 
  Disj (Conj (Unify xs (obj "nil"))
             (Unify ys zs))
       (Fresh ["x", "xs'", "res"] $ 
         Conj (Unify xs (TmPair (obj "x") (obj "xs'")))
              (Conj (Named "appendo" [obj "xs'", ys, obj "res"])
                    (Unify zs (TmPair (obj "x") (obj "res")))))
                    
  where 
  obj :: String -> Term
  obj nm = TmObj (Obj nm)