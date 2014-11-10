module Kanren.Render where

import DOM

import Data.Maybe
import Data.Tuple
import Data.Foldable (intercalate)
import Data.Traversable (for)

import Control.Bind
import Control.Apply
import Control.Monad (when, unless)
import Control.Monad.Eff
import Control.Monad.JQuery

import Kanren.Eval
import Kanren.State
import Kanren.Goal
import Kanren.Term
import Kanren.Var
import Kanren.Obj
import Kanren.Unify

render :: forall eff. State -> Eff (dom :: DOM | eff) Unit 
render st@(State g su var stk) = void do
  -- Update the goal    
    
  select "#goal .lines" >>= remove    
    
  goal <- create "<div>" >>= addClass "lines"
  renderGoal true goal g
  select "#goal .with-margin" >>= append goal
    
  -- Update the stack    
    
  select "#stack ul" >>= remove
  stack <- create "<ul>"
  
  for stk $ \g' -> void do 
    pre <- create "<pre>" >>= appendText (renderShortGoal g')
    li <- create "<li>" >>= append pre
    li `append` stack
    
  select "#stack .with-margin" >>= append stack
  
  -- Update the substitution
  
  select "#subst ul" >>= remove
  subst <- create "<ul>"
  
  for su $ \(Tuple (Var nm) tm) -> do
    let text = "#" ++ show nm ++ " = " ++ renderTerm tm
    pre <- create "<pre>" >>= appendText text
    li <- create "<li>" >>= append pre
    li `append` subst
    
  select "#subst .with-margin" >>= append subst
  where
      
  renderShortGoal :: Goal -> String
  renderShortGoal Done = "Done"
  renderShortGoal (Fresh nm _) = "fresh " ++ nm
  renderShortGoal (Unify u v) = renderTerm u ++ " == " ++ renderTerm v
  renderShortGoal (Disj g1 g2) = "disj"
  renderShortGoal (Conj g1 g2) = "conj"
  renderShortGoal (Named name ts) = name
    
  renderGoal :: forall eff. Boolean -> JQuery -> Goal -> Eff (dom :: DOM | eff) Unit 
  renderGoal _           jq Done = void do
    "Evaluation complete" `appendText` jq
  renderGoal _           jq Fail = void do
    "Contradiction!" `appendText` jq
  renderGoal renderLinks jq (Fresh nm g) = void do
    let newState = State (replace nm (TmVar var) g) su (succ var) stk
    link <- linkTo renderLinks (render newState)
              >>= appendText ("fresh " ++ nm)
    line <- newLine >>= append link
    line `append` jq
    rest <- indented
    renderGoal false rest g
    rest `append` jq
  renderGoal renderLinks jq (Unify u v) = void do
    let text = renderTerm u ++ " == " ++ renderTerm v
        action = case unify u v su of
          Nothing -> render $ State Fail su var stk
          Just su' -> render $ unwind $ State Done su' var stk
    link <- linkTo renderLinks action 
              >>= appendText text
    line <- newLine >>= append link
    line `append` jq
  renderGoal renderLinks jq (Named nm ts) = void do
    let text = nm ++ " " ++ intercalate " " (renderTerm <$> ts)
        newState = State (builtIn nm ts) su var stk
    link <- linkTo renderLinks (render newState) 
              >>= appendText text
    line <- newLine >>= append link
    line `append` jq
  renderGoal renderLinks jq (Disj g1 g2) = void do
    line <- newLine >>= appendText "disj"
    line `append` jq
    
    i1 <- indented
    a1 <- linkTo renderLinks (render (unwind (State g1 su var stk))) 
    renderGoal false a1 g1
    a1 `append` i1
    i1 `append` jq
    
    i2 <- indented
    a2 <- linkTo renderLinks (render (unwind (State g2 su var stk)))
    renderGoal false a2 g2
    a2 `append` i2
    i2 `append` jq
  renderGoal renderLinks jq (Conj g1 g2) = void do
    line <- newLine >>= appendText "conj"
    line `append` jq
    
    i1 <- indented
    a1 <- linkTo renderLinks (render (unwind (State g1 su var (g2 : stk)))) 
    renderGoal false a1 g1
    a1 `append` i1
    i1 `append` jq
    
    i2 <- indented
    a2 <- linkTo renderLinks (render (unwind (State g2 su var (g1 : stk)))) 
    renderGoal false a2 g2
    a2 `append` i2
    i2 `append` jq
  
  unwind :: State -> State
  unwind (State Done subst var (goal : stack)) = State goal subst var stack
  unwind other = other
  
  linkTo :: forall eff a. Boolean -> (Eff (dom :: DOM | eff) a) -> Eff (dom :: DOM | eff) JQuery 
  linkTo true action =
    create "<a href='#'>" 
      >>= on "click" (\e _ -> action *> preventDefault e)
  linkTo false _ = 
    create "<span>"
        
  indented :: forall eff. Eff (dom :: DOM | eff) JQuery
  indented = create "<div>" >>= addClass "indented"
  
  newLine :: forall eff. Eff (dom :: DOM | eff) JQuery
  newLine = create "<div>" >>= addClass "line"
    
  renderTerm :: Term -> String
  renderTerm (TmVar (Var v)) = "#" ++ show v
  renderTerm (TmObj (Obj o)) = o
  renderTerm (TmPair t1 t2) = "(" ++ renderTerm t1 ++ ", " ++ renderTerm t2 ++ ")"
    
  spaces = go ""
    where
    go acc 0 = acc
    go acc n = go (acc ++ "  ") (n - 1)