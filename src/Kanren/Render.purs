module Kanren.Render where

import DOM

import Data.Traversable (for)

import Control.Bind
import Control.Monad.Eff
import Control.Monad.JQuery

import Kanren.Eval
import Kanren.State
import Kanren.Goal

render :: forall eff. State -> Eff (dom :: DOM | eff) JQuery 
render st@(State Done _ _ _) = create "<li>" >>= appendText (show st)
render st = do
  a <- create "<a href='#'>"
  appendText (show st) a
  on "click" expand a
  create "<li>" >>= append a
  where    
      
  expand e jq = do
    li <- parent jq
    ul <- create "<ul>"
    for (step st) $ \st' -> do
      child <- render st'
      child `append` ul 
      preventDefault e
    ul `append` li
