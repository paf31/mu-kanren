module Kanren.Goal
  ( Goal(..)
  , runGoal
  ) where    
    
import Data.Maybe
    
import Kanren.Term
import Kanren.State
import Kanren.Stream
import Kanren.Var    
import Kanren.Unify
    
data Goal 
  = Delay Goal
  | Unify Term Term
  | Fresh (Term -> Goal)
  | Disj Goal Goal
  | Conj Goal Goal

runGoal :: Goal -> State -> Stream
runGoal (Delay g) st = StrDelay $ \_ -> runGoal g st
runGoal (Unify u v) st = 
  case unify u v $ stateSubst st of
    Nothing -> mzero
    Just sub -> StrCons (State sub (stateVar st)) mzero
runGoal (Fresh f) st =
  let c = stateVar st
      g = f $ TmVar c
  in runGoal g $ State (stateSubst st) (succ c)
runGoal (Disj g1 g2) st = runGoal g1 st `mplus` runGoal g2 st
runGoal (Conj g1 g2) st = runGoal g1 st `bind` g2

mplus :: Stream -> Stream -> Stream
mplus StrNil s = s
mplus (StrDelay f) s = StrDelay $ \u -> s `mplus` f u
mplus (StrCons st s1) s2 = StrCons st $ mplus s1 s2

bind :: Stream -> Goal -> Stream
bind StrNil _ = mzero
bind (StrDelay f) g = StrDelay $ \u -> f u `bind` g
bind (StrCons st s1) g = runGoal g st `mplus` (s1 `bind` g)