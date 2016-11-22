module Kanren.Types where

import Prelude
import Data.List (List, intercalate)
import Data.Tuple (Tuple)

newtype Var = Var Int

runVar :: Var -> Int
runVar (Var v) = v

derive newtype instance eqVar :: Eq Var
derive newtype instance ordVar :: Ord Var
derive newtype instance showVar :: Show Var

zero :: Var
zero = Var 0

succ :: Var -> Var
succ (Var n) = Var $ n + 1

newtype Obj = Obj String

runObj :: Obj -> String
runObj (Obj s) = s

derive newtype instance eqObj :: Eq Obj
derive newtype instance ordObj :: Ord Obj
derive newtype instance showObj :: Show Obj

data Term
  = TmVar Var
  | TmObj Obj
  | TmPair Term Term

instance showTerm :: Show Term where
  show (TmVar v) = "(TmVar " <> show v <> ")"
  show (TmObj o) = "(TmObj " <> show o <> ")"
  show (TmPair t1 t2) = "(TmPair " <> show t1 <> " " <> show t2 <> ")"

obj :: String -> Term
obj nm = TmObj (Obj nm)

type Subst = List (Tuple Var Term)

data Define = Define String (List String) Goal

instance showDefine :: Show Define where
  show (Define nm args g) = "(Define" <>
    " " <> show nm <>
    " " <> show args <>
    " " <> show g <>
    ")"

data Goal
  = Done
  | Fail
  | Unify Term Term
  | Fresh (List String) Goal
  | Disj (List Goal)
  | Conj (List Goal)
  | Named String (List Term)

instance showGoal :: Show Goal where
  show Done = "Done"
  show Fail = "Fail"
  show (Unify t1 t2) = "(Unify" <>
    " " <> show t1 <>
    " " <> show t2 <>
    ")"
  show (Fresh nms g) = "(Fresh" <>
    " " <> intercalate " " nms <>
    " " <> show g <>
    ")"
  show (Disj gs) = "(Disj" <>
    " " <> intercalate " " (show <$> gs) <>
    ")"
  show (Conj gs) = "(Conj" <>
    " " <> intercalate " " (show <$> gs) <>
    ")"
  show (Named name ts) = "(" <>
    name <>
    " " <> intercalate " " (show <$> ts) <>
    ")"

type Stack = List Goal
