module Kanren.Eval where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (foldl, elem, find)
import Data.List (List, (:), length, zip)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), lookup)
import Kanren.Types (Define(..), Goal(..), Obj(..), Subst, Term(..), Var)

walk :: Subst -> Term -> Term
walk s (TmVar v) = fromMaybe (TmVar v) $ lookup v s
walk s (TmPair t1 t2) = TmPair (walk s t1) (walk s t2)
walk _ t = t

ext :: Var -> Term -> Subst -> Subst
ext v t s = Tuple v t : s

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

replace :: String -> Term -> Goal -> Goal
replace nm r = onGoals where
  onGoals Fail = Fail
  onGoals Done = Done
  onGoals (Unify u v) = Unify (onTerms u) (onTerms v)
  onGoals f@(Fresh ns g) = if nm `elem` ns then f else Fresh ns (onGoals g)
  onGoals (Disj gs) = Disj (onGoals <$> gs)
  onGoals (Conj gs) = Conj (onGoals <$> gs)
  onGoals (Named name ts) = Named name (onTerms <$> ts)

  onTerms (TmObj (Obj nm')) | nm == nm' = r
  onTerms (TmPair t1 t2) = TmPair (onTerms t1) (onTerms t2)
  onTerms other = other

replaceAll :: List (Tuple String Term) -> Goal -> Goal
replaceAll = foldl (\f (Tuple nm r) g -> replace nm r (f g)) id

builtIn :: List Define -> String -> List Term -> Either String Goal
builtIn defines nm args =
  case find (\(Define nm' _ _) -> nm == nm') defines of
    Just (Define _ argNames g) | length argNames == length args  -> Right $ replaceAll (zip argNames args) g
    Just _ -> Left $ "Incorrect number of arguments to " <> show nm
    Nothing -> Left $ "Unknown function " <> show nm
