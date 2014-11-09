module Kanren.Stack where

import Data.Monoid
import Data.Foldable

import Kanren.Goal

type Stack = [Goal]