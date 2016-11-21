module Kanren.Eval where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (foldl, elem, find)
import Data.List (List, length, zip)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Kanren.Goal (Define(..), Goal(..))
import Kanren.Obj (Obj(..))
import Kanren.Term (Term(..))

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
