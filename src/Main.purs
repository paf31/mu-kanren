module Main where

import Control.Monad.Eff
import Control.Monad.JQuery

import Kanren.Eval
import Kanren.Render

main = do
  ul <- create "<ul>"
  li <- render example

  li `append` ul
  body >>= append ul
