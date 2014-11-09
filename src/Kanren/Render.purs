module Kanren.Render where

import DOM

import Data.Tuple
import Data.Foldable (intercalate)
import Data.Traversable (for)

import Control.Bind
import Control.Monad (unless)
import Control.Monad.Eff
import Control.Monad.JQuery

import Kanren.Eval
import Kanren.State
import Kanren.Goal
import Kanren.Term
import Kanren.Var
import Kanren.Obj

render :: forall eff. State -> Eff (dom :: DOM | eff) JQuery 
render st@(State g su _ stack) = do
  li <- create "<li>" >>= addClass "state"
  
  unless (isDone g) $ void do 
    goal <- create "<pre>" >>= addClass "goal"
    intercalate "\n" (renderGoal 0 g) `appendText` goal
    goal `append` li
  
  for stack $ \g' -> unless (isDone g') $ void do 
    stack <- create "<pre>" >>= addClass "stack"
    intercalate "\n" (renderGoal 0 g') `appendText` stack
    create "<hr>" >>= flip append li
    stack `append` li
  
  subst <- create "<ul>" >>= addClass "subst"
  for su $ \(Tuple (Var nm) tm) -> do
    li <- create "<li>"
    let text = "#" ++ show nm ++ " = " ++ renderTerm tm
    text `appendText` li
    li `append` subst
  subst `append` li
   
  unless (isDone g) $ void do 
    a <- create "<a href='#'>More</a>"
    on "click" expand a
    a `append` li
    
  return li
  where
      
  isDone Done = true
  isDone _    = false          
      
  expand e jq = do
    li <- parent jq
    ul <- create "<ul>"
    for (step st) $ \st' -> do
      child <- render st'
      child `append` ul 
      preventDefault e
    ul `append` li
    
  renderGoal :: Number -> Goal -> [String]
  renderGoal _ Done = []
  renderGoal n (Fresh nm g) = (spaces n ++ "fresh " ++ nm) : renderGoal n g
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