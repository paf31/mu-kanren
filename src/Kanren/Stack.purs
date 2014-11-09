module Kanren.Stack where

import Kanren.Goal

data Stack = Empty | Push Goal Stack

instance showStack :: Show Stack where
  show Empty = "Empty"
  show (Push g s) = "(Push" ++
    " " ++ show g ++
    " " ++ show s ++
    ")"
