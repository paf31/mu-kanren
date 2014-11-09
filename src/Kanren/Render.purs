module Kanren.Render where

import DOM

import Data.Traversable (for)

import Control.Bind
import Control.Monad.Eff
import Control.Monad.JQuery

import Kanren.Eval
import Kanren.State
import Kanren.Goal
import Kanren.Term
import Kanren.Var
import Kanren.Obj

render :: forall eff. State -> Eff (dom :: DOM | eff) JQuery 
render st@(State Done _ _ _) = create "<li>" >>= appendText (show st)
render st@(State g _ _ _) = do
  li <- create "<li>"
  for (renderGoal 0 g) $ \s -> do
    div <- create "<div>" >>= appendText s
    div `append` li
  a <- create "<a href='#'>More</a>"
  on "click" expand a
  a `append` li
  where    
      
  expand e jq = do
    li <- parent jq
    ul <- create "<ul>"
    for (step st) $ \st' -> do
      child <- render st'
      child `append` ul 
      preventDefault e
    ul `append` li
    
  renderGoal :: Number -> Goal -> [String]
  renderGoal n (Fresh nm g) = (spaces n ++ "fresh " ++ nm ++ "\n") : renderGoal n g
  renderGoal n (Unify u v) = [spaces n ++ renderTerm u ++ " == " ++ renderTerm v]
  renderGoal n (Disj g1 g2) = (spaces n ++ "disj") : renderGoal (n + 1) g1 ++ renderGoal (n + 1) g2
  renderGoal n (Conj g1 g2) = (spaces n ++ "conj") : renderGoal (n + 1) g1 ++ renderGoal (n + 1) g2
    
  renderTerm :: Term -> String
  renderTerm (TmVar (Var v)) = "#" ++ show v
  renderTerm (TmObj (Obj o)) = o
  renderTerm (TmPair t1 t2) = "(" ++ renderTerm t1 ++ ", " ++ renderTerm t2 ++ ")"
    
  spaces = go ""
    where
    go acc 0 = acc
    go acc n = go (acc ++ "&nbsp;&nbsp;") (n - 1)