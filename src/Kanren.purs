module Kanren where

import Data.Maybe

import Kanren.Var
import Kanren.Obj
import Kanren.Term
import Kanren.Subst
import Kanren.State
import Kanren.Stream
import Kanren.Goal

runOne :: Goal -> Maybe Subst
runOne g = go $ runGoal g (State [] zero)
  where
  go StrNil = Nothing
  go (StrDelay f) = go (f unit)
  go (StrCons st _) = Just $ stateSubst st

runAll :: Goal -> [Subst]
runAll g = go $ runGoal g (State [] zero)
  where
  go StrNil = []
  go (StrDelay f) = go (f unit)
  go (StrCons st s) = stateSubst st : go s

