module Kanren.Unify where
    
import Data.Maybe
    
import Kanren.Term
import Kanren.Subst    
    
unify :: Term -> Term -> Subst -> Maybe Subst
unify u v s = go (walk s u) (walk s v)
  where
  go (TmVar v1) (TmVar v2) | v1 == v2 = Just s
  go (TmVar u1) v1 = Just $ ext u1 v1 s
  go u1 (TmVar v1) = Just $ ext v1 u1 s
  go (TmObj o1) (TmObj o2) | o1 == o2 = Just s
  go (TmPair a1 b1) (TmPair a2 b2) = 
    case unify a1 a2 s of
      Nothing -> Nothing
      Just s' -> unify b1 b2 s'
  go _ _ = Nothing