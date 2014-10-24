module Kanren.Expr where
    
import Kanren   

import Data.Maybe
import Data.Tuple
import Data.Foldable 
    
data ETerm
  = ETmVar String
  | ETmObj Obj
  | ETmPair ETerm ETerm      
    
data Expr
  = Fail
  | Equals ETerm ETerm
  | Delay Expr
  | Fresh String Expr
  | Disj Expr Expr
  | Conj Expr Expr
  
type Env = [Tuple String Term]  
  
compile :: Expr -> Goal
compile = go []
  where
  go :: Env -> Expr -> Goal
  go _   Fail = \_ -> mzero
  go env (Equals t1 t2) = toTerm env t1 === toTerm env t2
  go env (Delay e) = delay (go env e)
  go env (Fresh nm e) = fresh $ \v -> go (Tuple nm v : env) e
  go env (Disj e1 e2) = disj (go env e1) (go env e2)
  go env (Conj e1 e2) = conj (go env e1) (go env e2)
  
  toTerm :: Env -> ETerm -> Term
  toTerm env (ETmVar v) = case lookup v env of Just t -> t
  toTerm _   (ETmObj o) = TmObj o
  toTerm env (ETmPair t1 t2) = TmPair (toTerm env t1) (toTerm env t2)