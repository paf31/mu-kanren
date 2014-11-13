module Main where

import DOM

import Data.Maybe

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.JQuery

import Kanren.Eval
import Kanren.Render

withoutDefault :: forall eff a. Eff (dom :: DOM | eff) a -> JQueryEvent -> JQuery -> Eff (dom :: DOM | eff) Unit
withoutDefault action e _ = do
  action
  preventDefault e

main = do
  select "#editButton" >>= 
    on "click" (withoutDefault showEditor)
      
  select "#evalButton" >>= 
    on "click" (withoutDefault eval)
    