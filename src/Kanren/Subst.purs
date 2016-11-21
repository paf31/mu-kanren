module Kanren.Subst where

import Prelude
import Data.Tuple (Tuple(..), lookup)
import Kanren.Var (Var)
import Kanren.Term (Term(..))
import Data.List (List, (:))
import Data.Maybe (fromMaybe)

type Subst = List (Tuple Var Term)

walk :: Subst -> Term -> Term
walk s (TmVar v) = fromMaybe (TmVar v) $ lookup v s
walk s (TmPair t1 t2) = TmPair (walk s t1) (walk s t2)
walk _ t = t

ext :: Var -> Term -> Subst -> Subst
ext v t s = Tuple v t : s
