module Kanren where

newtype Var = Var Number

instance eqVar :: Eq Var where
  (==) (Var v1) (Var v2) = v1 == v2
  (/=) (Var v1) (Var v2) = v1 /= v2

instance showVar :: Show Var where
  show (Var v)= show v

zero :: Var
zero = Var 0

succ :: Var -> Var
succ (Var n) = Var $ n + 1

newtype Obj = Obj String

instance eqObj :: Eq Obj where
  (==) (Obj o1) (Obj o2) = o1 == o2
  (/=) (Obj o1) (Obj o2) = o1 /= o2

instance showObj :: Show Obj where
  show (Obj o) = o

data Term
  = TmVar Var
  | TmObj Obj
  | TmPair Term Term

instance showTerm :: Show Term where
  show (TmVar v) = "#" ++ show v
  show (TmObj o) = show o
  show (TmPair t1 t2) = "(" ++ show t1 ++ ", " ++ show t2 ++ ")"

data Subst
  = SNil
  | SCons Var Term Subst

instance showSubst :: Show Subst where
  show sub = "[" ++ go sub ++ "]"
    where
    go SNil = ""
    go (SCons v t SNil) = show v ++ ": " ++ show t
    go (SCons v t s) = show v ++ ": " ++ show t ++ ", " ++ go s

walk :: Subst -> Term -> Term
walk s (TmVar v) = go s v
  where
  go SNil v = TmVar v
  go (SCons v t s) v1 | v == v1 = t
  go (SCons _ _ s) v1 = go s v1
walk _ t = t

ext :: Var -> Term -> Subst -> Subst
ext = SCons

data State = State Subst Var

stateSubst :: State -> Subst
stateSubst (State s _) = s

stateVar :: State -> Var
stateVar (State _ v) = v

data Stream
  = StrNil
  | StrDelay (Unit -> Stream)
  | StrCons State Stream

strNull :: Stream -> Boolean
strNull StrNil = true
strNull _     = false

type Goal = State -> Stream

one :: Goal
one st = StrCons st mzero

mzero :: Stream
mzero = StrNil

delay :: Goal -> Goal
delay g st = StrDelay $ \_ -> g st

data Maybe a = Nothing | Just a

instance showMaybe :: (Show a) => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just a) = "Just (" ++ show a ++ ")"

(===) :: Term -> Term -> Goal
(===) u v st = 
  case unify u v $ stateSubst st of
    Nothing -> mzero
    Just sub -> one $ State sub (stateVar st)

unify :: Term -> Term -> Subst -> Maybe Subst
unify u v s = go (walk s u) (walk s v)
  where
  go (TmVar v1) (TmVar v2) | v1 == v2 = Just s
  go (TmVar u1) v1 = Just $ ext u1 v1 s
  go u1 (TmVar v1) = Just $ ext v1 u1 s
  go (TmObj o1) (TmObj o2) | o1 == o2 = Just s
  go (TmPair a1 b1) (TmPair a2 b2) = 
    case unify a1 a2 s of
      Nothing -> Nothing
      Just s' -> unify b1 b2 s'
  go _ _ = Nothing

fresh :: (Term -> Goal) -> Goal
fresh f st =
  let c = stateVar st
      g = f $ TmVar c
  in g $ State (stateSubst st) (succ c)

disj :: Goal -> Goal -> Goal
disj g1 g2 st = g1 st `mplus` g2 st

conj :: Goal -> Goal -> Goal
conj g1 g2 st = g1 st `bind` g2

mplus :: Stream -> Stream -> Stream
mplus StrNil s = s
mplus (StrDelay f) s = StrDelay $ \u -> s `mplus` f u
mplus (StrCons st s1) s2 = StrCons st $ mplus s1 s2

bind :: Stream -> Goal -> Stream
bind StrNil _ = mzero
bind (StrDelay f) g = StrDelay $ \u -> f u `bind` g
bind (StrCons st s1) g = g st `mplus` (s1 `bind` g)

runOne :: Goal -> Maybe Subst
runOne g = go $ g (State SNil zero)
  where
  go StrNil = Nothing
  go (StrDelay f) = go (f unit)
  go (StrCons st _) = Just $ stateSubst st

runAll :: Goal -> [Subst]
runAll g = go $ g (State SNil zero)
  where
  go StrNil = []
  go (StrDelay f) = go (f unit)
  go (StrCons st s) = stateSubst st : go s

