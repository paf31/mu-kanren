module Kanren.Goal
  ( Goal()
  , delay
  , (===)
  , fresh
  , disj
  , conj
  ) where    
    
import Data.Maybe
    
import Kanren.Term
import Kanren.State
import Kanren.Stream
import Kanren.Var    
import Kanren.Unify
    
type Goal = State -> Stream

one :: Goal
one st = StrCons st mzero

delay :: Goal -> Goal
delay g st = StrDelay $ \_ -> g st

(===) :: Term -> Term -> Goal
(===) u v st = 
  case unify u v $ stateSubst st of
    Nothing -> mzero
    Just sub -> one $ State sub (stateVar st)

fresh :: (Term -> Goal) -> Goal
fresh f st =
  let c = stateVar st
      g = f $ TmVar c
  in g $ State (stateSubst st) (succ c)

disj :: Goal -> Goal -> Goal
disj g1 g2 st = g1 st `mplus` g2 st

conj :: Goal -> Goal -> Goal
conj g1 g2 st = g1 st `bind` g2

mplus :: Stream -> Stream -> Stream
mplus StrNil s = s
mplus (StrDelay f) s = StrDelay $ \u -> s `mplus` f u
mplus (StrCons st s1) s2 = StrCons st $ mplus s1 s2

bind :: Stream -> Goal -> Stream
bind StrNil _ = mzero
bind (StrDelay f) g = StrDelay $ \u -> f u `bind` g
bind (StrCons st s1) g = g st `mplus` (s1 `bind` g)