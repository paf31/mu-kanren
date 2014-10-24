module Kanren.Subst where
    
import Data.Tuple
import Data.Maybe (fromMaybe)
import Data.Foldable (lookup)

import Kanren.Var    
import Kanren.Term
    
type Subst = [Tuple Var Term]

walk :: Subst -> Term -> Term
walk s (TmVar v) = fromMaybe (TmVar v) $ lookup v s
walk _ t = t

ext :: Var -> Term -> Subst -> Subst
ext v t s = Tuple v t : s